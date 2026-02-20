#!/usr/bin/env -S uv run --script
# /// script
# dependencies = ["beautifulsoup4", "lxml"]
# ///
"""
HTML Documentation Diff Comparison Tool

Compares two documentation directories, parsing HTML with BeautifulSoup
and supporting declarative rules for allowed differences.

Usage:
    uv run scripts/check_diff_soup.py <dir1> <dir2>
"""

from __future__ import annotations

import argparse
import difflib
import html
import json
import random
import re
import sqlite3
import sys
import time
from concurrent.futures import ThreadPoolExecutor, as_completed
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any, Callable
from urllib.parse import unquote, urljoin

from bs4 import BeautifulSoup, NavigableString, Tag

# Parser configuration - uses lxml for speed.
# To use built-in parser instead, remove lxml from dependencies above
# and change PARSER to "html.parser"
PARSER = "lxml"

# ANSI color codes for TTY output
IS_TTY = sys.stdout.isatty()
RED = "\033[91m" if IS_TTY else ""
GREEN = "\033[92m" if IS_TTY else ""
RESET = "\033[0m" if IS_TTY else ""
RED_BG = "\033[41m" if IS_TTY else ""
GREEN_BG = "\033[42m" if IS_TTY else ""
BOLD = "\033[1m" if IS_TTY else ""


def log(msg: str = "") -> None:
    """Print a message and flush stdout immediately."""
    print(msg, flush=True)


# =============================================================================
# URL Resolution
# =============================================================================


def _resolve_href(href: str, file_path: str) -> str:
    """Resolve an href relative to the given file path.

    Returns the resolved target as a normalized path, possibly with a fragment.
    Special URLs (external, javascript:, mailto:, data:) are returned as-is.
    Fragment-only hrefs are resolved against file_path.
    """
    if not href or href.startswith(("javascript:", "mailto:", "data:")):
        return href

    # Fragment-only: resolve against current file
    if href.startswith("#"):
        return f"{file_path}{href}"

    # External URLs
    if href.startswith(("http://", "https://", "//")):
        return href

    # Resolve relative path
    resolved = urljoin(file_path, href)

    # Normalize the path
    resolved = unquote(resolved)
    parts = resolved.split("#", 1)
    path_part = parts[0]

    # Normalize path (resolve .. and .)
    try:
        normalized = str(Path(path_part).as_posix())
        # Handle paths that go above root
        if normalized.startswith(".."):
            normalized = path_part
    except Exception:
        normalized = path_part

    if len(parts) > 1:
        return f"{normalized}#{parts[1]}"
    return normalized


# =============================================================================
# Data Classes
# =============================================================================


@dataclass
class DiffContext:
    """Context object passed to rules for evaluating differences."""

    file_path: str  # Relative path of HTML file being compared
    old_elem: Tag | None  # Element in old version (None if added)
    new_elem: Tag | None  # Element in new version (None if removed)
    old_ancestors: list[Tag]  # Parent chain from old tree (nearest first)
    new_ancestors: list[Tag]  # Parent chain from new tree (nearest first)
    diff_type: str  # "attribute", "text", "element_replaced", "element_added", "element_removed"
    attribute_name: str | None  # For attribute diffs
    old_value: Any  # Old value (attribute value, text, etc.)
    new_value: Any  # New value
    old_targets: set[str]  # Valid link targets in old docs
    new_targets: set[str]  # Valid link targets in new docs

    def _resolve_href(self, href: str) -> str:
        """Resolve an href relative to the current file path."""
        return _resolve_href(href, self.file_path)

    def old_href_is_broken(self) -> bool:
        """Check if old element's href pointed to nonexistent target."""
        if self.old_elem is None:
            return False
        href = self.old_elem.get("href")
        if not href:
            return False

        # Skip external links
        if str(href).startswith(("http://", "https://", "//", "javascript:", "mailto:")):
            return False

        return self._resolve_href(str(href)) not in self.old_targets

    def new_href_is_broken(self) -> bool:
        """Check if new element's href points to nonexistent target."""
        if self.new_elem is None:
            return False
        href = self.new_elem.get("href")
        if not href:
            return False

        # Skip external links
        if str(href).startswith(("http://", "https://", "//", "javascript:", "mailto:")):
            return False

        return self._resolve_href(str(href)) not in self.new_targets

    def has_ancestor(self, selector: str) -> bool:
        """Check if any ancestor in old tree matches CSS selector (simple matching)."""
        # Parse simple selectors: tag, .class, tag.class, #id
        tag_name = None
        class_name = None
        id_name = None

        if "#" in selector:
            parts = selector.split("#", 1)
            tag_name = parts[0] if parts[0] else None
            id_name = parts[1]
        elif "." in selector:
            parts = selector.split(".", 1)
            tag_name = parts[0] if parts[0] else None
            class_name = parts[1]
        else:
            tag_name = selector

        for ancestor in self.old_ancestors:
            if not isinstance(ancestor, Tag):
                continue

            if tag_name and ancestor.name != tag_name:
                continue
            if class_name and class_name not in ancestor.get("class", []):
                continue
            if id_name and ancestor.get("id") != id_name:
                continue

            return True

        return False


@dataclass
class Difference:
    """Represents a detected difference between two HTML documents."""

    file_path: str
    diff_type: str
    old_elem: Tag | None
    new_elem: Tag | None
    old_ancestors: list[Tag]
    new_ancestors: list[Tag]
    attribute_name: str | None = None
    old_value: Any = None
    new_value: Any = None
    accepted: bool = False
    reason: str | None = None
    rule_name: str | None = None
    section: str = "body"


@dataclass
class FileComparisonResult:
    """Result of comparing a single HTML file."""

    file_path: str
    differences: list[Difference] = field(default_factory=list)
    error: str | None = None
    elapsed_ms: float = 0.0


# =============================================================================
# Rules
# =============================================================================

# Rule type: takes DiffContext, returns reason string if allowed, None if not handled
Rule = Callable[[DiffContext], str | None]


def allow_href_change_if_old_broken(ctx: DiffContext) -> str | None:
    """Allow href changes on <a> tags if old version pointed at nonexistent target."""
    if ctx.diff_type != "attribute" or ctx.attribute_name != "href":
        return None
    if ctx.old_elem and ctx.old_elem.name == "a" and ctx.old_href_is_broken():
        return "href changed because old target was broken"
    return None


def allow_href_change_from_private(ctx: DiffContext) -> str | None:
    """Allow href changes where the old target was a private name and new target is valid.

    Covers cases like private-name redirects (#_private.X.Y → #X) where the old
    link pointed to a private name but the new link points to the public declaration.
    """
    if ctx.diff_type != "attribute" or ctx.attribute_name != "href":
        return None
    old_href = str(ctx.old_value or "")
    # Check that old href contains a private name fragment
    if "_private." not in old_href:
        return None
    if not ctx.new_href_is_broken():
        return "href changed from private name to valid target"
    return None


def allow_href_change_same_anchor_valid_target(ctx: DiffContext) -> str | None:
    """Allow href changes where only the module path changed but the anchor ID is the same.

    Covers cases like duplicate declarations mapped to different modules, where
    both targets are valid and point to the same declaration name.
    """
    if ctx.diff_type != "attribute" or ctx.attribute_name != "href":
        return None
    old_href = str(ctx.old_value or "")
    new_href = str(ctx.new_value or "")
    # Extract fragment (anchor) from both hrefs
    old_frag = old_href.split("#", 1)[1] if "#" in old_href else None
    new_frag = new_href.split("#", 1)[1] if "#" in new_href else None
    if old_frag is None or new_frag is None:
        return None
    if old_frag != new_frag:
        return None
    if not ctx.new_href_is_broken():
        return "href changed module but same anchor, new target valid"
    return None


def allow_a_to_span_if_broken(ctx: DiffContext) -> str | None:
    """Allow <a> to be replaced by <span class='fn'> if link was broken."""
    if ctx.diff_type != "element_replaced":
        return None
    if (
        ctx.old_elem
        and ctx.old_elem.name == "a"
        and ctx.new_elem
        and ctx.new_elem.name == "span"
        and "fn" in ctx.new_elem.get("class", [])
    ):
        if ctx.old_href_is_broken():
            return "<a> with broken link replaced by <span class='fn'>"
    return None


def allow_unwrap_broken_link(ctx: DiffContext) -> str | None:
    """Allow broken <a> tags to be unwrapped (contents remain, tag removed)."""
    if ctx.diff_type != "element_removed":
        return None
    if ctx.old_elem and ctx.old_elem.name == "a" and ctx.old_href_is_broken():
        return "broken <a> tag unwrapped"
    return None


def _lean_file_hrefs_equivalent(href1: str, href2: str) -> bool:
    """Check if two file:/// .lean hrefs are equivalent up to temp dir prefix."""
    if not href1.startswith("file:///") or not href2.startswith("file:///"):
        return False
    if not href1.endswith(".lean") or not href2.endswith(".lean"):
        return False
    if "/.lake/" not in href1 or "/.lake/" not in href2:
        return False
    return href1[href1.find("/.lake/"):] == href2[href2.find("/.lake/"):]


def _check_href_valid(href: str, file_path: str, targets: set[str]) -> bool:
    """Check if an href target exists in the given target set."""
    if not href:
        return False
    if href.startswith(("http://", "https://", "//", "javascript:", "mailto:")):
        return True
    return _resolve_href(href, file_path) in targets


def allow_added_link_with_valid_target(ctx: DiffContext) -> str | None:
    """Allow added <a> elements or replacements with <a> if their href target exists."""
    if ctx.diff_type not in ("element_added", "element_replaced"):
        return None
    if not ctx.new_elem or ctx.new_elem.name != "a":
        return None

    href = ctx.new_elem.get("href")
    if not href:
        return None

    # Skip external links
    if str(href).startswith(("http://", "https://", "//", "javascript:", "mailto:")):
        return None

    # Check if target exists (inverse of new_href_is_broken)
    if not ctx.new_href_is_broken():
        return "added link with valid target"

    return None


def allow_span_fn_to_link(ctx: DiffContext) -> str | None:
    """Allow <span class='fn'> to be replaced by <a> if the link target is valid."""
    if ctx.diff_type != "element_replaced":
        return None
    if not ctx.old_elem or ctx.old_elem.name != "span":
        return None
    if "fn" not in ctx.old_elem.get("class", []):
        return None
    if not ctx.new_elem or ctx.new_elem.name != "a":
        return None

    # Check if the new link has a valid target
    if not ctx.new_href_is_broken():
        return "<span class='fn'> replaced by link with valid target"

    return None


def allow_lean_file_href_change(ctx: DiffContext) -> str | None:
    """Allow href changes on file:/// URLs to .lean files if they match from .lake onwards."""
    if ctx.diff_type != "attribute" or ctx.attribute_name != "href":
        return None
    old_val = str(ctx.old_value or "")
    new_val = str(ctx.new_value or "")
    if not old_val.startswith("file:///") or not new_val.startswith("file:///"):
        return None
    if not old_val.endswith(".lean") or not new_val.endswith(".lean"):
        return None
    # Compare from .lake onwards
    old_suffix = old_val[old_val.find("/.lake/"):] if "/.lake/" in old_val else old_val
    new_suffix = new_val[new_val.find("/.lake/"):] if "/.lake/" in new_val else new_val
    if old_suffix == new_suffix:
        return "file:/// .lean href differs only in temp directory prefix"
    return None


# DB-backed declaration source position data, loaded via --db flag.
# Maps (module_name, decl_name) → (start_line, start_column).
_db_decl_positions: dict[tuple[str, str], tuple[int, int]] = {}


def load_db_decl_positions(db_path: Path) -> dict[tuple[str, str], tuple[int, int]]:
    """Load declaration source positions from the doc-gen4 SQLite database."""
    conn = sqlite3.connect(str(db_path))
    cursor = conn.execute(
        "SELECT n.module_name, n.name, r.start_line, r.start_column "
        "FROM name_info n JOIN declaration_ranges r "
        "USING (module_name, position)"
    )
    result = {}
    for module_name, name, start_line, start_column in cursor:
        result[(module_name, name)] = (start_line, start_column)
    conn.close()
    return result


def load_db_targets(db_path: Path) -> set[str]:
    """Build a set of valid link targets from the doc-gen4 SQLite database.

    Each target is "Module/Path.html#escaped_name", matching the format used
    by extract_link_targets and docLink values in declaration-data.bmp.
    """
    conn = sqlite3.connect(str(db_path))
    cursor = conn.execute("SELECT module_name, name FROM name_info")
    targets: set[str] = set()
    for module_name, name in cursor:
        file_path = module_name.replace(".", "/") + ".html"
        escaped_name = html.escape(name, quote=False)
        targets.add(f"{file_path}#{escaped_name}")
    conn.close()
    return targets


def _file_path_to_module(file_path: str) -> str:
    """Convert HTML file path to Lean module name (e.g. 'Init/Core.html' → 'Init.Core')."""
    return file_path.removesuffix(".html").replace("/", ".")


def _find_enclosing_decl(elem: Tag | None, ancestors: list[Tag]) -> Tag | None:
    """Find the enclosing <div class='decl'> from an element or its ancestors."""
    if elem and isinstance(elem, Tag) and elem.name == "div" and "decl" in elem.get("class", []):
        return elem
    for a in ancestors:
        if isinstance(a, Tag) and a.name == "div" and "decl" in a.get("class", []):
            return a
    return None


def _extract_decl_name_from_context(elem: Tag | None, ancestors: list[Tag]) -> str | None:
    """Extract declaration name from either a <div class='decl'> or an <a href='#name'> context."""
    # Check for enclosing <div class='decl'>
    decl = _find_enclosing_decl(elem, ancestors)
    if decl is not None:
        return decl.get("id")

    # Check if the element itself is (or contains) a link with a fragment-only href
    if elem and isinstance(elem, Tag):
        a = elem if elem.name == "a" else elem.find("a")
        if a:
            href = a.get("href", "")
            if isinstance(href, str) and href.startswith("#"):
                return href[1:]

    # Check ancestors for <a> with fragment href (handles children of nav links)
    for a in ancestors:
        if isinstance(a, Tag) and a.name == "a":
            href = a.get("href", "")
            if isinstance(href, str) and href.startswith("#"):
                return href[1:]

    return None


def allow_reorder_same_source_position(ctx: DiffContext) -> str | None:
    """Accept diffs caused by reordering declarations that share the same source position.

    Uses the doc-gen4 database (loaded via --db) to look up the actual source line/column
    for each declaration. If two swapped declarations originate from the same source position,
    their ordering is non-deterministic and the diff is acceptable.

    Handles both declaration bodies (<div class='decl'>) and navigation links (<a href='#name'>).
    """
    if not _db_decl_positions:
        return None

    old_name = _extract_decl_name_from_context(ctx.old_elem, ctx.old_ancestors)
    new_name = _extract_decl_name_from_context(ctx.new_elem, ctx.new_ancestors)

    if not old_name or not new_name:
        return None

    # Same declaration, not a reordering
    if old_name == new_name:
        return None

    module = _file_path_to_module(ctx.file_path)
    old_pos = _db_decl_positions.get((module, str(old_name)))
    new_pos = _db_decl_positions.get((module, str(new_name)))

    if not old_pos or not new_pos or old_pos != new_pos:
        return None

    # Verify both declarations exist in both old and new HTML.
    # A genuine reorder means A and B both appear in old and new, just at
    # different positions. Without this check the rule would also accept
    # content changes or additions/deletions that happen to share a position.
    old_target = f"{ctx.file_path}#{old_name}"
    new_target = f"{ctx.file_path}#{new_name}"
    if old_target not in ctx.old_targets or new_target not in ctx.old_targets:
        return None
    if old_target not in ctx.new_targets or new_target not in ctx.new_targets:
        return None

    return f"reordering of declarations at same source position (line {old_pos[0]})"


def allow_empty_equations_removal(ctx: DiffContext) -> str | None:
    """Accept diffs caused by removal of empty equations sections (no equation items).

    The empty <details><summary>Equations</summary><ul class="equations"></ul></details>
    gets removed in the new version. Because the diff tool matches elements positionally,
    this shows up as attribute/text diffs (old Equations paired with next sibling) rather
    than a clean element_removed.
    """

    def is_empty_equations_details(elem: Tag) -> bool:
        """Check if elem is <details> with Equations summary and no <li> items."""
        if not isinstance(elem, Tag) or elem.name != "details":
            return False
        summary = elem.find("summary")
        if not summary or summary.get_text(strip=True) != "Equations":
            return False
        eq_list = elem.find("ul", class_="equations")
        if eq_list is None:
            return True  # No list at all
        return len(eq_list.find_all("li", recursive=False)) == 0

    # Check old element and its ancestors for an empty equations <details>
    if ctx.old_elem and is_empty_equations_details(ctx.old_elem):
        return "removal of empty equations section"
    for ancestor in ctx.old_ancestors:
        if is_empty_equations_details(ancestor):
            return "removal of empty equations section"

    # Handle cascading positional shift: when an empty equations <details> is removed,
    # the next sibling shifts position and appears as "element_removed" at the end.
    # Check if any sibling in the old parent was an empty equations <details>.
    if ctx.diff_type == "element_removed" and ctx.old_ancestors:
        old_parent = ctx.old_ancestors[0]
        if isinstance(old_parent, Tag):
            for sibling in old_parent.children:
                if isinstance(sibling, Tag) and is_empty_equations_details(sibling):
                    return "cascading from removal of empty equations section"

    return None


def allow_duplicate_li_removal_in_imports(ctx: DiffContext) -> str | None:
    """Allow changes inside <div class='imports'> that remove duplicate <li> elements."""
    if not ctx.has_ancestor("div.imports"):
        return None

    # Find the <ul> or <ol> ancestor that contains <li> elements
    def find_list_ancestor(ancestors: list[Tag]) -> Tag | None:
        for ancestor in ancestors:
            if ancestor.name in ("ul", "ol"):
                return ancestor
        return None

    old_list = find_list_ancestor(ctx.old_ancestors)
    new_list = find_list_ancestor(ctx.new_ancestors)

    if old_list is None or new_list is None:
        return None

    # Get <li> contents from both lists
    def get_li_contents(parent: Tag) -> list[str]:
        return [str(li) for li in parent.find_all("li", recursive=False)]

    old_lis = get_li_contents(old_list)
    new_lis = get_li_contents(new_list)

    # Check if old had duplicates
    old_unique = set(old_lis)
    if len(old_lis) == len(old_unique):
        return None  # No duplicates in old

    # Verify new is strictly a deduplication of old:
    new_unique = set(new_lis)
    if not new_unique.issubset(old_unique):
        return None  # new has entries that weren't in old
    if not old_unique.issubset(new_unique):
        return None  # old entries were removed entirely, not just deduplicated
    if len(new_lis) != len(new_unique):
        return None  # new still has duplicates

    return "duplicate <li> removed from imports"


def allow_extends_id_wrapper(ctx: DiffContext) -> str | None:
    """Accept diffs caused by new <span id='...'>  wrappers around extends clause parents.

    The extends clause now wraps each parent type in <span id="StructName.toParent">
    to create link targets for parent projection names. The wrapper <span> is a
    child of <div class="decl_header">, as a sibling of <span class="decl_extends">.
    """
    def in_decl_header(ancestors: list[Tag]) -> bool:
        return any(
            isinstance(a, Tag) and a.name == "div" and "decl_header" in a.get("class", [])
            for a in ancestors
        )

    def has_projection_id_ancestor(ancestors: list[Tag]) -> bool:
        """Check if any ancestor is a <span id='...'> projection wrapper."""
        return any(
            isinstance(a, Tag) and a.name == "span" and a.get("id")
            and in_decl_header(ancestors[i+1:])
            for i, a in enumerate(ancestors)
        )

    if not in_decl_header(ctx.old_ancestors) and not in_decl_header(ctx.new_ancestors):
        return None

    # Find enclosing decl_headers and verify the overall text is preserved.
    # This gates all acceptance paths — without it, a buggy wrapper insertion
    # that changes header text would be silently accepted.
    def find_header(ancestors: list[Tag]) -> Tag | None:
        for a in ancestors:
            if isinstance(a, Tag) and a.name == "div" and "decl_header" in a.get("class", []):
                return a
        return None

    old_header = find_header(ctx.old_ancestors)
    new_header = find_header(ctx.new_ancestors)
    if old_header is None or new_header is None:
        return None
    if old_header.find("span", class_="decl_extends") is None:
        return None
    if old_header.get_text() != new_header.get_text():
        return None

    # The new <span id="..."> wrapper element itself
    if ctx.new_elem and isinstance(ctx.new_elem, Tag) and ctx.new_elem.name == "span" and ctx.new_elem.get("id"):
        return "extends clause parent projection id wrapper"

    # Any diff inside the new wrapper span
    if has_projection_id_ancestor(ctx.new_ancestors):
        return "inside extends clause parent projection id wrapper"

    # Positional shifts in decl_header caused by the wrapper insertion
    if in_decl_header(ctx.old_ancestors) and in_decl_header(ctx.new_ancestors):
        return "positional shift from extends clause id wrapper"

    return None


def allow_inherited_field_id(ctx: DiffContext) -> str | None:
    """Accept new id attributes on inherited structure field <li> elements.

    Inherited fields now get id attributes so they can be link targets,
    matching direct fields which already had them.
    """
    if ctx.diff_type != "attribute" or ctx.attribute_name != "id":
        return None
    # New element should be an <li> with class "inherited_field"
    if (
        ctx.new_elem
        and isinstance(ctx.new_elem, Tag)
        and ctx.new_elem.name == "li"
        and "inherited_field" in ctx.new_elem.get("class", [])
    ):
        return "inherited field now has id for linking"
    return None


# Default rules list
# Note: <code> elements are handled specially by compare_code_elements() in compare_trees()
# These rules handle differences outside of <code> elements
#
# Link insertion in doc-gen4 is heuristic — the same declaration text may
# legitimately resolve to different targets between the old and new pipelines.
# Rules and code comparison therefore validate that new link targets *exist*,
# not that they point to the *same* declaration as before.
RULES: list[Rule] = [
    allow_lean_file_href_change,
    allow_href_change_if_old_broken,
    allow_href_change_from_private,
    allow_href_change_same_anchor_valid_target,
    allow_a_to_span_if_broken,
    allow_span_fn_to_link,
    allow_unwrap_broken_link,
    allow_added_link_with_valid_target,
    allow_extends_id_wrapper,
    allow_empty_equations_removal,
    allow_duplicate_li_removal_in_imports,
    allow_reorder_same_source_position,
    allow_inherited_field_id,
]


# =============================================================================
# Directory Comparison
# =============================================================================


def collect_files(directory: Path) -> tuple[set[str], set[str]]:
    """
    Recursively collect all files in directory.

    Returns:
        Tuple of (html_files, other_files) as sets of relative paths
    """
    html_files: set[str] = set()
    other_files: set[str] = set()

    for path in directory.rglob("*"):
        if path.is_file():
            rel_path = str(path.relative_to(directory))
            if path.suffix.lower() in (".html", ".htm"):
                html_files.add(rel_path)
            else:
                other_files.add(rel_path)

    return html_files, other_files


def compare_directories(
    dir1: Path, dir2: Path
) -> tuple[set[str], set[str], set[str], set[str], set[str], set[str]]:
    """
    Compare file inventories of two directories.

    Returns:
        Tuple of:
        - HTML files only in dir1
        - HTML files only in dir2
        - HTML files in both
        - Other files only in dir1
        - Other files only in dir2
        - Other files in both
    """
    html1, other1 = collect_files(dir1)
    html2, other2 = collect_files(dir2)

    return (
        html1 - html2,
        html2 - html1,
        html1 & html2,
        other1 - other2,
        other2 - other1,
        other1 & other2,
    )


# =============================================================================
# Link Target Extraction
# =============================================================================


# Regex patterns for extracting link targets (faster than full HTML parsing).
# Note: these can match inside HTML comments, <script>, or <style> blocks,
# producing phantom targets. This is acceptable for doc-gen4 output which
# doesn't use id/name attributes in those contexts.
# Require attributes to be inside complete HTML tags: <tagname ... id="value" ...>
# Use non-greedy [^>]*? to match the first id/name attribute in the tag
# Two patterns per attribute: one for double-quoted, one for single-quoted values,
# so that a ' inside id="foo'" is captured correctly.
ID_PATTERN_DQ = re.compile(r'<\w[^>]*?\bid="([^"]+)"[^>]*>', re.IGNORECASE)
ID_PATTERN_SQ = re.compile(r"<\w[^>]*?\bid='([^']+)'[^>]*>", re.IGNORECASE)
NAME_PATTERN_DQ = re.compile(r'<\w[^>]*?\bname="([^"]+)"[^>]*>', re.IGNORECASE)
NAME_PATTERN_SQ = re.compile(r"<\w[^>]*?\bname='([^']+)'[^>]*>", re.IGNORECASE)


def extract_targets_from_file(directory: Path, rel_path: str) -> set[str]:
    """Extract link targets from a single HTML file using regex."""
    targets: set[str] = {rel_path}  # File itself is a target

    file_path = directory / rel_path
    try:
        content = file_path.read_text(encoding="utf-8", errors="replace")

        # Extract id attributes
        for match in ID_PATTERN_DQ.finditer(content):
            targets.add(f"{rel_path}#{match.group(1)}")
        for match in ID_PATTERN_SQ.finditer(content):
            targets.add(f"{rel_path}#{match.group(1)}")

        # Extract name attributes
        for match in NAME_PATTERN_DQ.finditer(content):
            targets.add(f"{rel_path}#{match.group(1)}")
        for match in NAME_PATTERN_SQ.finditer(content):
            targets.add(f"{rel_path}#{match.group(1)}")

    except Exception as e:
        print(f"Warning: Could not extract targets from {rel_path}: {e}", file=sys.stderr)

    return targets


def extract_link_targets(
    directory: Path, html_files: set[str], max_workers: int = 8
) -> set[str]:
    """
    Extract all valid link targets from HTML files using regex (parallel).

    Targets include:
    - File paths themselves (e.g., "path/to/file.html")
    - id attributes (e.g., "path/to/file.html#some-id")
    - name attributes on anchors (e.g., "path/to/file.html#anchor-name")
    """
    targets: set[str] = set()

    with ThreadPoolExecutor(max_workers=max_workers) as executor:
        futures = [
            executor.submit(extract_targets_from_file, directory, rel_path)
            for rel_path in html_files
        ]

        for future in as_completed(futures):
            targets.update(future.result())

    return targets


# =============================================================================
# Special Code Element Comparison
# =============================================================================


def compare_code_elements(
    old_code: Tag,
    new_code: Tag,
    file_path: str,
    old_targets: set[str],
    new_targets: set[str],
) -> tuple[bool, str | None]:
    """
    Compare two <code> elements with precise structural validation.

    Uses a stack-based algorithm to walk through children, tracking wrapper
    elements. Only allows specific transformations:
    - Text wrapped in new <a> with valid href
    - Broken <a> unwrapped (text remains)
    - <span class='fn'> → <a> with valid href
    - Broken <a> → <span class='fn'>
    - href change where both old and new are valid

    Returns (is_ok, reason) where is_ok=True if all changes are acceptable.
    """
    from collections import deque

    def is_wrapper(node: Tag) -> bool:
        """Check if a tag is a wrapper element (link or span)."""
        return node.name in ('a', 'span')

    def wrappers_compatible(old_w: Tag | None, new_w: Tag | None) -> bool:
        """Check if two wrappers are equivalent or the change is allowed."""
        if old_w is None and new_w is None:
            return True

        if old_w is not None and new_w is not None:
            if old_w.name == new_w.name:
                if old_w.name == 'a':
                    old_href = str(old_w.get('href', ''))
                    new_href = str(new_w.get('href', ''))
                    if old_href == new_href:
                        return True
                    # Allow file:/// .lean hrefs that match from .lake onwards
                    if _lean_file_hrefs_equivalent(old_href, new_href):
                        return True
                    # Allow href change if new link is valid (old can be valid or broken)
                    return _check_href_valid(new_href, file_path, new_targets)
                elif old_w.name == 'span':
                    return old_w.get('class') == new_w.get('class')

            # span.fn → a with valid target
            if (old_w.name == 'span' and 'fn' in old_w.get('class', []) and
                new_w.name == 'a'):
                return _check_href_valid(str(new_w.get('href', '')), file_path, new_targets)

            # a (broken) → span.fn
            if (old_w.name == 'a' and
                new_w.name == 'span' and 'fn' in new_w.get('class', [])):
                return not _check_href_valid(str(old_w.get('href', '')), file_path, old_targets)

            return False

        # Adding a link with valid target
        if old_w is None and new_w is not None and new_w.name == 'a':
            return _check_href_valid(str(new_w.get('href', '')), file_path, new_targets)

        # Adding a span.fn is OK
        if old_w is None and new_w is not None and new_w.name == 'span':
            return 'fn' in new_w.get('class', [])

        # Removing a broken link
        if old_w is not None and old_w.name == 'a' and new_w is None:
            return not _check_href_valid(str(old_w.get('href', '')), file_path, old_targets)

        # Removing a span.fn is OK
        if old_w is not None and old_w.name == 'span' and new_w is None:
            return 'fn' in old_w.get('class', [])

        return False

    # Stack-based iteration: each stack entry is (iterator, wrapper_or_none)
    old_stack: deque[tuple[list, int, Tag | None]] = deque()  # (children, index, wrapper)
    new_stack: deque[tuple[list, int, Tag | None]] = deque()

    old_stack.append((list(old_code.children), 0, None))
    new_stack.append((list(new_code.children), 0, None))

    old_text = ""  # Pending text from old side
    new_text = ""  # Pending text from new side

    def get_current_wrapper(stack: deque) -> Tag | None:
        """Get the innermost wrapper from the stack."""
        for children, idx, wrapper in reversed(stack):
            if wrapper is not None:
                return wrapper
        return None

    def advance(stack: deque) -> tuple[str | None, Tag | None, bool]:
        """
        Advance through the stack, returning next text chunk or wrapper tag.
        Returns (text, tag, done) where exactly one of text/tag is non-None,
        or both are None and done is True.
        """
        while stack:
            children, idx, wrapper = stack[-1]

            if idx >= len(children):
                stack.pop()
                continue

            child = children[idx]
            stack[-1] = (children, idx + 1, wrapper)

            if isinstance(child, NavigableString):
                text = str(child)
                if text:
                    return text, None, False
            elif isinstance(child, Tag):
                if is_wrapper(child):
                    # Push wrapper's children onto stack
                    stack.append((list(child.children), 0, child))
                else:
                    # Non-wrapper tag - just descend into children
                    stack.append((list(child.children), 0, None))

        return None, None, True

    # Main loop: consume text from both sides, verifying wrappers match
    while True:
        # Strip leading whitespace, then get more text if needed.
        # We loop because fetched text may be whitespace-only (e.g., " " between tags).
        old_text = old_text.lstrip()
        while not old_text and old_stack:
            text, tag, done = advance(old_stack)
            if text:
                old_text = text.lstrip()
            elif done:
                break

        new_text = new_text.lstrip()
        while not new_text and new_stack:
            text, tag, done = advance(new_stack)
            if text:
                new_text = text.lstrip()
            elif done:
                break

        # Check for completion
        if not old_text and not new_text:
            if not old_stack and not new_stack:
                return True, "code element with valid structural changes"
            # One side has more content - need to drain it
            continue

        # One side empty, other has content - text mismatch
        if not old_text or not new_text:
            # Check if remaining is just whitespace
            if not old_text:
                # Check if new side only has whitespace left
                remaining = new_text
                while new_stack:
                    text, _, done = advance(new_stack)
                    if text:
                        remaining += text
                    if done:
                        break
                if remaining.strip():
                    return False, None
                return True, "code element with valid structural changes"
            else:
                remaining = old_text
                while old_stack:
                    text, _, done = advance(old_stack)
                    if text:
                        remaining += text
                    if done:
                        break
                if remaining.strip():
                    return False, None
                return True, "code element with valid structural changes"

        # Find common prefix
        common_len = 0
        min_len = min(len(old_text), len(new_text))
        while common_len < min_len and old_text[common_len] == new_text[common_len]:
            common_len += 1

        if common_len == 0:
            # No common prefix - text mismatch
            return False, None

        # Check wrappers are compatible for this text
        old_wrapper = get_current_wrapper(old_stack)
        new_wrapper = get_current_wrapper(new_stack)

        if not wrappers_compatible(old_wrapper, new_wrapper):
            return False, None

        # Consume the common prefix
        old_text = old_text[common_len:]
        new_text = new_text[common_len:]


# =============================================================================
# HTML Tree Comparison
# =============================================================================


def get_significant_children(elem: Tag, in_pre: bool = False) -> list[Tag | NavigableString]:
    """Get children that are significant for comparison (elements and non-whitespace text).

    Inside <pre> blocks, all text (including whitespace-only) is preserved since
    whitespace is significant for code formatting.
    """
    result = []
    for child in elem.children:
        if isinstance(child, Tag):
            result.append(child)
        elif isinstance(child, NavigableString):
            if in_pre:
                result.append(child)
            else:
                text = str(child).strip()
                if text:
                    result.append(child)
    return result


def normalize_text(text: str) -> str:
    """Normalize text for comparison (collapse whitespace)."""
    return " ".join(text.split())


def normalize_attr_value(value: Any) -> Any:
    """Normalize attribute value for comparison."""
    if isinstance(value, list):
        # Class lists — sort for order-independent comparison.
        # CSS specificity doesn't depend on class attribute order; doc-gen4
        # JS also does not inspect classList ordering, so this is safe.
        return sorted(value)
    if isinstance(value, str):
        return normalize_text(value)
    return value


def compare_attributes(
    old_elem: Tag,
    new_elem: Tag,
    file_path: str,
    old_ancestors: list[Tag],
    new_ancestors: list[Tag],
    old_targets: set[str],
    new_targets: set[str],
) -> list[Difference]:
    """Compare attributes of two elements."""
    differences = []

    old_attrs = dict(old_elem.attrs)
    new_attrs = dict(new_elem.attrs)

    all_keys = set(old_attrs.keys()) | set(new_attrs.keys())

    for key in all_keys:
        old_val = normalize_attr_value(old_attrs.get(key))
        new_val = normalize_attr_value(new_attrs.get(key))

        if old_val != new_val:
            differences.append(
                Difference(
                    file_path=file_path,
                    diff_type="attribute",
                    old_elem=old_elem,
                    new_elem=new_elem,
                    old_ancestors=old_ancestors.copy(),
                    new_ancestors=new_ancestors.copy(),
                    attribute_name=key,
                    old_value=old_attrs.get(key),
                    new_value=new_attrs.get(key),
                )
            )

    return differences


def compare_trees(
    old_node: Tag | NavigableString | None,
    new_node: Tag | NavigableString | None,
    file_path: str,
    old_ancestors: list[Tag],
    new_ancestors: list[Tag],
    old_targets: set[str],
    new_targets: set[str],
    in_pre: bool = False,
) -> list[Difference]:
    """Recursively compare two DOM trees."""
    differences = []

    # Handle None cases
    if old_node is None and new_node is None:
        return differences

    if old_node is None and new_node is not None:
        if isinstance(new_node, Tag):
            differences.append(
                Difference(
                    file_path=file_path,
                    diff_type="element_added",
                    old_elem=None,
                    new_elem=new_node,
                    old_ancestors=old_ancestors.copy(),
                    new_ancestors=new_ancestors.copy(),
                )
            )
        return differences

    if old_node is not None and new_node is None:
        if isinstance(old_node, Tag):
            differences.append(
                Difference(
                    file_path=file_path,
                    diff_type="element_removed",
                    old_elem=old_node,
                    new_elem=None,
                    old_ancestors=old_ancestors.copy(),
                    new_ancestors=new_ancestors.copy(),
                )
            )
        return differences

    # Both nodes exist
    old_is_tag = isinstance(old_node, Tag)
    new_is_tag = isinstance(new_node, Tag)

    # Text vs Text
    if not old_is_tag and not new_is_tag:
        if in_pre:
            old_text = str(old_node)
            new_text = str(new_node)
        else:
            old_text = normalize_text(str(old_node))
            new_text = normalize_text(str(new_node))
        if old_text != new_text:
            differences.append(
                Difference(
                    file_path=file_path,
                    diff_type="text",
                    old_elem=old_ancestors[0] if old_ancestors else None,
                    new_elem=new_ancestors[0] if new_ancestors else None,
                    old_ancestors=old_ancestors.copy(),
                    new_ancestors=new_ancestors.copy(),
                    old_value=old_text,
                    new_value=new_text,
                )
            )
        return differences

    # Tag vs Text or Text vs Tag - structural mismatch
    if old_is_tag != new_is_tag:
        if old_is_tag:
            differences.append(
                Difference(
                    file_path=file_path,
                    diff_type="element_removed",
                    old_elem=old_node,
                    new_elem=None,
                    old_ancestors=old_ancestors.copy(),
                    new_ancestors=new_ancestors.copy(),
                )
            )
        else:
            differences.append(
                Difference(
                    file_path=file_path,
                    diff_type="element_added",
                    old_elem=None,
                    new_elem=new_node,
                    old_ancestors=old_ancestors.copy(),
                    new_ancestors=new_ancestors.copy(),
                )
            )
        return differences

    # Both are Tags
    assert isinstance(old_node, Tag) and isinstance(new_node, Tag)

    # Check if tag names differ
    if old_node.name != new_node.name:
        differences.append(
            Difference(
                file_path=file_path,
                diff_type="element_replaced",
                old_elem=old_node,
                new_elem=new_node,
                old_ancestors=old_ancestors.copy(),
                new_ancestors=new_ancestors.copy(),
            )
        )
        # Don't recurse into replaced elements - the whole subtree is different
        return differences

    # Special handling for <code> and <li class="equation"> elements
    # These use precise structural comparison that only allows specific link transformations
    def should_use_structural_comparison(node: Tag) -> bool:
        if node.name == "code":
            return True
        if node.name == "li" and "equation" in node.get("class", []):
            return True
        return False

    if should_use_structural_comparison(old_node):
        is_ok, reason = compare_code_elements(
            old_node, new_node, file_path, old_targets, new_targets
        )
        if is_ok:
            # All changes are acceptable - return a single accepted difference
            # (or no difference if they're identical)
            if str(old_node) != str(new_node):
                diff = Difference(
                    file_path=file_path,
                    diff_type="structural",
                    old_elem=old_node,
                    new_elem=new_node,
                    old_ancestors=old_ancestors.copy(),
                    new_ancestors=new_ancestors.copy(),
                    accepted=True,
                    reason=reason,
                    rule_name="compare_code_elements",
                )
                differences.append(diff)
            return differences
        # If not OK, fall through to normal comparison which will generate
        # specific differences that rules can evaluate

    # Same tag name - compare attributes
    differences.extend(
        compare_attributes(old_node, new_node, file_path, old_ancestors, new_ancestors, old_targets, new_targets)
    )

    # Compare children
    next_old_ancestors = [old_node] + old_ancestors
    next_new_ancestors = [new_node] + new_ancestors
    next_in_pre = in_pre or old_node.name == "pre"
    old_children = get_significant_children(old_node, next_in_pre)
    new_children = get_significant_children(new_node, next_in_pre)

    # Simple alignment: compare by index
    max_len = max(len(old_children), len(new_children))
    for i in range(max_len):
        old_child = old_children[i] if i < len(old_children) else None
        new_child = new_children[i] if i < len(new_children) else None
        differences.extend(
            compare_trees(old_child, new_child, file_path, next_old_ancestors, next_new_ancestors, old_targets, new_targets, in_pre=next_in_pre)
        )

    return differences


def compare_tactics_page(
    old_soup: BeautifulSoup,
    new_soup: BeautifulSoup,
    file_path: str,
    old_targets: set[str],
    new_targets: set[str],
) -> list[Difference] | None:
    """Semantic comparison for tactics.html.

    Instead of positional comparison (which produces spurious diffs when
    same-userName tactics are reordered), this:
    1. Matches tactic entries by internalName (the div id)
    2. Compares matched pairs with normal tree comparison + rules
    3. Verifies both pages are sorted by userName
    4. Ignores ordering within the same userName group

    Returns a list of Differences, or None to fall through to normal comparison.
    """
    old_main = (old_soup.body or old_soup).find("main")
    new_main = (new_soup.body or new_soup).find("main")
    if old_main is None or new_main is None:
        return None

    def extract_tactic_divs(main_elem: Tag) -> dict[str, Tag]:
        """Extract {internalName: div} from tactic divs."""
        result: dict[str, Tag] = {}
        for div in main_elem.find_all("div", id=True, recursive=False):
            if div.find("h2") is not None:
                result[div.get("id", "")] = div
        return result

    def get_user_names_in_order(main_elem: Tag) -> list[str]:
        """Get userName sequence from tactic divs."""
        return [h2.get_text(strip=True)
                for div in main_elem.find_all("div", id=True, recursive=False)
                if (h2 := div.find("h2")) is not None]

    old_divs = extract_tactic_divs(old_main)
    new_divs = extract_tactic_divs(new_main)

    # Both must be sorted by userName (ignoring order within same userName)
    old_names = get_user_names_in_order(old_main)
    new_names = get_user_names_in_order(new_main)
    if sorted(old_names) != old_names or sorted(new_names) != new_names:
        return None

    # Compare matched tactic entries by internalName using normal tree comparison
    differences: list[Difference] = []

    old_keys = set(old_divs.keys())
    new_keys = set(new_divs.keys())
    for key in sorted(old_keys - new_keys):
        differences.append(Difference(
            file_path=file_path, diff_type="element_removed",
            old_elem=old_divs[key], new_elem=None,
            old_ancestors=[old_main], new_ancestors=[new_main],
        ))
    for key in sorted(new_keys - old_keys):
        differences.append(Difference(
            file_path=file_path, diff_type="element_added",
            old_elem=None, new_elem=new_divs[key],
            old_ancestors=[old_main], new_ancestors=[new_main],
        ))

    for internal_name in sorted(old_keys & new_keys):
        diffs = compare_trees(
            old_divs[internal_name], new_divs[internal_name], file_path,
            [old_main], [new_main],
            old_targets, new_targets,
        )
        differences.extend(diffs)

    # Compare non-tactic children of <main> (the intro paragraph etc.) positionally
    old_non_tactic = [c for c in get_significant_children(old_main)
                      if not (isinstance(c, Tag) and c.name == "div" and c.get("id") and c.find("h2"))]
    new_non_tactic = [c for c in get_significant_children(new_main)
                      if not (isinstance(c, Tag) and c.name == "div" and c.get("id") and c.find("h2"))]
    for i in range(max(len(old_non_tactic), len(new_non_tactic))):
        old_child = old_non_tactic[i] if i < len(old_non_tactic) else None
        new_child = new_non_tactic[i] if i < len(new_non_tactic) else None
        differences.extend(compare_trees(
            old_child, new_child, file_path,
            [old_main], [new_main],
            old_targets, new_targets,
        ))

    # Compare nav section: match nav links by href target rather than position
    old_nav = (old_soup.body or old_soup).find("nav", class_="internal_nav")
    new_nav = (new_soup.body or new_soup).find("nav", class_="internal_nav")
    if old_nav and new_nav:
        def extract_nav_entries(nav: Tag) -> dict[str, Tag]:
            """Extract {href: <p> element} from nav links."""
            result: dict[str, Tag] = {}
            for p in nav.find_all("p", recursive=False):
                a = p.find("a")
                if a and a.get("href", "").startswith("#"):
                    result[a["href"]] = p
            return result

        old_nav_entries = extract_nav_entries(old_nav)
        new_nav_entries = extract_nav_entries(new_nav)

        # Compare matched nav entries
        for href in old_nav_entries:
            if href in new_nav_entries:
                diffs = compare_trees(
                    old_nav_entries[href], new_nav_entries[href], file_path,
                    [old_nav], [new_nav],
                    old_targets, new_targets,
                )
                differences.extend(diffs)

        # Report any entries only in one side
        for href in set(old_nav_entries) - set(new_nav_entries):
            differences.append(Difference(
                file_path=file_path, diff_type="element_removed",
                old_elem=old_nav_entries[href], new_elem=None,
                old_ancestors=[old_nav], new_ancestors=[new_nav],
            ))
        for href in set(new_nav_entries) - set(old_nav_entries):
            differences.append(Difference(
                file_path=file_path, diff_type="element_added",
                old_elem=None, new_elem=new_nav_entries[href],
                old_ancestors=[old_nav], new_ancestors=[new_nav],
            ))

        # Compare the "return to top" link and any other non-<p> children
        old_other = [c for c in get_significant_children(old_nav) if not (isinstance(c, Tag) and c.name == "p" and c.find("a"))]
        new_other = [c for c in get_significant_children(new_nav) if not (isinstance(c, Tag) and c.name == "p" and c.find("a"))]
        for i in range(max(len(old_other), len(new_other))):
            oc = old_other[i] if i < len(old_other) else None
            nc = new_other[i] if i < len(new_other) else None
            differences.extend(compare_trees(oc, nc, file_path, [old_nav], [new_nav], old_targets, new_targets))

    # Compare everything outside main and nav (rest of body)
    old_body = old_soup.body or old_soup
    new_body = new_soup.body or new_soup
    old_rest = [c for c in get_significant_children(old_body)
                if not (isinstance(c, Tag) and (c.name == "main" or (c.name == "nav" and "internal_nav" in c.get("class", []))))]
    new_rest = [c for c in get_significant_children(new_body)
                if not (isinstance(c, Tag) and (c.name == "main" or (c.name == "nav" and "internal_nav" in c.get("class", []))))]
    for i in range(max(len(old_rest), len(new_rest))):
        oc = old_rest[i] if i < len(old_rest) else None
        nc = new_rest[i] if i < len(new_rest) else None
        differences.extend(compare_trees(oc, nc, file_path, [], [], old_targets, new_targets))

    return differences


def compare_html_files(
    file_path: str,
    dir1: Path,
    dir2: Path,
    old_targets: set[str],
    new_targets: set[str],
) -> FileComparisonResult:
    """Compare two HTML files and return differences."""
    start_time = time.perf_counter()
    result = FileComparisonResult(file_path=file_path)

    try:
        old_content = (dir1 / file_path).read_text(encoding="utf-8", errors="replace")
        new_content = (dir2 / file_path).read_text(encoding="utf-8", errors="replace")

        old_soup = BeautifulSoup(old_content, PARSER)
        new_soup = BeautifulSoup(new_content, PARSER)

        # Compare <head> sections — no acceptance rules are applied to <head>
        # diffs, so any difference here is reported as a rejection.
        old_head = old_soup.head
        new_head = new_soup.head
        if old_head or new_head:
            head_diffs = compare_trees(old_head, new_head, file_path, [], [], old_targets, new_targets)
            for d in head_diffs:
                d.section = "head"
            result.differences.extend(head_diffs)

        # Semantic comparison for tactics.html
        if file_path == "tactics.html":
            tactics_result = compare_tactics_page(old_soup, new_soup, file_path, old_targets, new_targets)
            if tactics_result is not None:
                result.differences.extend(tactics_result)
                result.elapsed_ms = (time.perf_counter() - start_time) * 1000
                return result

        # Compare <body>
        old_body = old_soup.body or old_soup
        new_body = new_soup.body or new_soup

        result.differences.extend(
            compare_trees(old_body, new_body, file_path, [], [], old_targets, new_targets)
        )

    except Exception as e:
        result.error = str(e)

    result.elapsed_ms = (time.perf_counter() - start_time) * 1000
    return result


# =============================================================================
# Rule Evaluation
# =============================================================================


def evaluate_rules(
    differences: list[Difference],
    rules: list[Rule],
    old_targets: set[str],
    new_targets: set[str],
) -> list[Difference]:
    """Evaluate rules against differences, marking accepted ones.

    Diffs with section='head' are not evaluated — acceptance rules are designed
    for <body> content, so any <head> difference is reported as-is.
    """
    for diff in differences:
        if diff.section == "head":
            continue

        ctx = DiffContext(
            file_path=diff.file_path,
            old_elem=diff.old_elem,
            new_elem=diff.new_elem,
            old_ancestors=diff.old_ancestors,
            new_ancestors=diff.new_ancestors,
            diff_type=diff.diff_type,
            attribute_name=diff.attribute_name,
            old_value=diff.old_value,
            new_value=diff.new_value,
            old_targets=old_targets,
            new_targets=new_targets,
        )

        for rule in rules:
            try:
                reason = rule(ctx)
                if reason is not None:
                    diff.accepted = True
                    diff.reason = reason
                    diff.rule_name = rule.__name__
                    break
            except Exception as e:
                print(f"Warning: Rule {rule.__name__} raised exception: {e}", file=sys.stderr)

    return differences


# =============================================================================
# Reporting
# =============================================================================


def format_element(elem: Tag | None, max_length: int = 200) -> str:
    """Format an element for display."""
    if elem is None:
        return "(none)"

    html = str(elem)
    if len(html) > max_length:
        html = html[: max_length - 3] + "..."
    return html


def format_context(ancestors: list[Tag], max_depth: int = 2) -> str:
    """Format ancestor context for display."""
    if not ancestors:
        return "(root)"

    parts = []
    for ancestor in ancestors[:max_depth]:
        if isinstance(ancestor, Tag):
            classes = ancestor.get("class", [])
            class_str = "." + ".".join(classes) if classes else ""
            id_str = f"#{ancestor.get('id')}" if ancestor.get("id") else ""
            parts.append(f"<{ancestor.name}{id_str}{class_str}>")

    return " > ".join(reversed(parts))


def format_parent_with_children(parent: Tag | None, max_length: int = 500) -> str:
    """Format a parent element with all its children."""
    if parent is None:
        return "(root)"

    html = str(parent)
    if len(html) > max_length:
        html = html[: max_length - 3] + "..."
    return html


def format_char_diff(old_str: str, new_str: str) -> tuple[str, str]:
    """Generate character-level diff highlighting for two strings."""
    matcher = difflib.SequenceMatcher(None, old_str, new_str)
    old_result = []
    new_result = []

    for tag, i1, i2, j1, j2 in matcher.get_opcodes():
        if tag == "equal":
            old_result.append(old_str[i1:i2])
            new_result.append(new_str[j1:j2])
        elif tag == "replace":
            old_result.append(f"{RED_BG}{old_str[i1:i2]}{RESET}{RED}")
            new_result.append(f"{GREEN_BG}{new_str[j1:j2]}{RESET}{GREEN}")
        elif tag == "delete":
            old_result.append(f"{RED_BG}{old_str[i1:i2]}{RESET}{RED}")
        elif tag == "insert":
            new_result.append(f"{GREEN_BG}{new_str[j1:j2]}{RESET}{GREEN}")

    return "".join(old_result), "".join(new_result)


def format_unified_diff(old_str: str, new_str: str, context: int = 2) -> list[str]:
    """Generate a unified diff showing changes between two HTML strings with context."""
    # Split on tag boundaries to get more meaningful diffs
    def split_html(s: str) -> list[str]:
        parts = []
        current = ""
        for char in s:
            if char == "<" and current:
                parts.append(current)
                current = "<"
            elif char == ">" and current:
                current += ">"
                parts.append(current)
                current = ""
            else:
                current += char
        if current:
            parts.append(current)
        return parts

    old_parts = split_html(old_str)
    new_parts = split_html(new_str)

    # Use unified_diff with context lines
    diff_lines = list(difflib.unified_diff(old_parts, new_parts, lineterm="", n=context))
    # Skip the header lines (---, +++)
    result = []
    pending_old = []
    pending_new = []

    def flush_pending():
        """Flush pending +/- lines, applying character diff if paired."""
        nonlocal pending_old, pending_new
        if pending_old and pending_new and len(pending_old) == len(pending_new):
            # Pair up old/new lines and show character diff
            for old_line, new_line in zip(pending_old, pending_new):
                old_text = old_line[1:]  # Remove - prefix
                new_text = new_line[1:]  # Remove + prefix
                old_diff, new_diff = format_char_diff(old_text, new_text)
                result.append(f"{RED}-{old_diff}{RESET}")
                result.append(f"{GREEN}+{new_diff}{RESET}")
        else:
            # Can't pair, just colorize
            for line in pending_old:
                result.append(f"{RED}{line}{RESET}")
            for line in pending_new:
                result.append(f"{GREEN}{line}{RESET}")
        pending_old = []
        pending_new = []

    for line in diff_lines:
        if line.startswith("---") or line.startswith("+++"):
            continue
        if line.startswith("@@"):
            flush_pending()
            if result:  # Add separator between hunks
                result.append("...")
            continue
        if line.startswith("-"):
            pending_old.append(line)
        elif line.startswith("+"):
            pending_new.append(line)
        else:
            flush_pending()
            result.append(" " + line)

    flush_pending()
    return result


def print_difference(diff: Difference, verbose: bool = False) -> None:
    """Print a single difference."""
    status = "ACCEPTED" if diff.accepted else "REJECTED"
    reason = f" ({diff.reason})" if diff.reason else ""

    attr_str = f" '{diff.attribute_name}'" if diff.attribute_name else ""
    log(f"\n  {status}{reason}: {diff.diff_type}{attr_str}")

    # Get parent elements
    old_parent = diff.old_ancestors[0] if diff.old_ancestors else None
    new_parent = diff.new_ancestors[0] if diff.new_ancestors else None

    # Get FULL HTML for diff computation (not truncated)
    old_html_full = str(old_parent) if old_parent else "(root)"
    new_html_full = str(new_parent) if new_parent else "(root)"

    # Always show the diff first (most important info)
    if old_html_full != new_html_full and old_html_full != "(root)" and new_html_full != "(root)":
        diff_lines = format_unified_diff(old_html_full, new_html_full)
        if diff_lines:
            log("    Diff:")
            for line in diff_lines:
                log(f"      {line}")
        else:
            # Fallback: show truncated old/new if diff couldn't find changes
            log(f"    Old: {format_parent_with_children(old_parent)}")
            log(f"    New: {format_parent_with_children(new_parent)}")
    else:
        # No diff or root elements - show truncated versions
        log(f"    Old: {format_parent_with_children(old_parent)}")
        log(f"    New: {format_parent_with_children(new_parent)}")

    if verbose and len(diff.old_ancestors) > 1:
        log(f"    Full context: {format_context(diff.old_ancestors)}")


def print_file_report(
    result: FileComparisonResult,
    verbose: bool = False,
) -> tuple[int, int, bool]:
    """
    Print a single file's comparison report.

    Returns:
        Tuple of (rejected_count, accepted_count, has_error)
    """
    if result.error:
        log(f"\n{'=' * 60}")
        log(f"ERROR: {result.file_path} ({result.elapsed_ms:.1f}ms)")
        log("=" * 60)
        log(f"  {result.error}")
        return 0, 0, True

    if not result.differences:
        return 0, 0, False

    rejected = [d for d in result.differences if not d.accepted]
    accepted = [d for d in result.differences if d.accepted]

    if rejected or (verbose and accepted):
        log(f"\n{'=' * 60}")
        log(f"FILE: {result.file_path} ({result.elapsed_ms:.1f}ms)")
        log("=" * 60)

        if rejected:
            log(f"\n  --- REJECTED ({len(rejected)}) ---")
            for diff in rejected:
                print_difference(diff, verbose)

        if verbose and accepted:
            log(f"\n  --- ACCEPTED ({len(accepted)}) ---")
            for diff in accepted:
                print_difference(diff, verbose)

    return len(rejected), len(accepted), len(rejected) > 0


def compare_declaration_data(
    old_data: dict, new_data: dict,
    old_targets: set[str] | None = None, new_targets: set[str] | None = None,
) -> tuple[list[str], list[str], list[str]]:
    """Compare two declaration-data JSON structures with domain-specific rules.

    - modules: keys must match; 'url' compared by equality; 'importedBy' compared as sets.
    - declarations: keys must match; values compared by field equality.
      When docLink differs but both targets are valid HTML anchors, the difference
      is reported as a benign multi-module assignment rather than an error.
    - instances: keys must match; values (lists) compared as sets.
    - instancesFor: keys must match; values (lists) compared as sets.

    Returns (error_lines, info_lines, detail_lines).
    error_lines: real problems (missing keys, true mismatches)
    info_lines: benign differences (multi-module assignments with both valid)
    detail_lines: verbose details for both
    """
    errors: list[str] = []
    info: list[str] = []
    detail: list[str] = []

    for section in ("declarations", "instances", "instancesFor", "modules"):
        old_sec = old_data.get(section, {})
        new_sec = new_data.get(section, {})
        old_keys = set(old_sec.keys())
        new_keys = set(new_sec.keys())

        only_old = sorted(old_keys - new_keys)
        only_new = sorted(new_keys - old_keys)
        if only_old:
            errors.append(f"{section}: {len(only_old)} keys only in old")
            for k in only_old[:10]:
                detail.append(f"  {section} only in old: {k}")
            if len(only_old) > 10:
                detail.append(f"  ... and {len(only_old) - 10} more")
        if only_new:
            errors.append(f"{section}: {len(only_new)} keys only in new")
            for k in only_new[:10]:
                detail.append(f"  {section} only in new: {k}")
            if len(only_new) > 10:
                detail.append(f"  ... and {len(only_new) - 10} more")

        # Compare shared keys
        mismatches: list[str] = []
        benign: list[str] = []
        for k in sorted(old_keys & new_keys):
            old_val = old_sec[k]
            new_val = new_sec[k]

            if section == "modules":
                # url: equality
                if old_val.get("url") != new_val.get("url"):
                    mismatches.append(f"  {k}: url differs")
                # importedBy: set equivalence
                old_ib = set(old_val.get("importedBy", []))
                new_ib = set(new_val.get("importedBy", []))
                if old_ib != new_ib:
                    added = new_ib - old_ib
                    removed = old_ib - new_ib
                    parts = []
                    if added:
                        parts.append(f"+{len(added)}")
                    if removed:
                        parts.append(f"-{len(removed)}")
                    mismatches.append(f"  {k}: importedBy {', '.join(parts)}")

            elif section in ("instances", "instancesFor"):
                # Compare as sorted lists to detect both membership and duplicate changes
                old_sorted = sorted(str(x) for x in old_val)
                new_sorted = sorted(str(x) for x in new_val)
                if old_sorted != new_sorted:
                    old_set = set(str(x) for x in old_val)
                    new_set = set(str(x) for x in new_val)
                    added = new_set - old_set
                    removed = old_set - new_set
                    old_dupes = len(old_val) - len(old_set)
                    new_dupes = len(new_val) - len(new_set)
                    parts = []
                    if added:
                        parts.append(f"+{len(added)}")
                    if removed:
                        parts.append(f"-{len(removed)}")
                    if old_dupes != new_dupes:
                        parts.append(f"dupes: {old_dupes}\u2192{new_dupes}")
                    if not parts:
                        parts.append("duplicate count changed")
                    mismatches.append(f"  {k}: {', '.join(parts)}")

            else:
                # declarations: field equality
                if old_val != new_val:
                    # Check if this is just a multi-module docLink difference
                    # where both targets are valid HTML anchors
                    old_link = old_val.get("docLink", "") if isinstance(old_val, dict) else ""
                    new_link = new_val.get("docLink", "") if isinstance(new_val, dict) else ""
                    both_valid = False
                    if (
                        old_link and new_link and old_link != new_link
                        and old_targets is not None and new_targets is not None
                    ):
                        # docLink values look like "./Module/Path.html#Name"
                        # Targets are stored as "Module/Path.html#Name" (no leading ./)
                        old_normalized = old_link.lstrip("./")
                        new_normalized = new_link.lstrip("./")
                        if old_normalized in old_targets and new_normalized in new_targets:
                            # Check that the only difference is docLink
                            old_rest = {kk: vv for kk, vv in old_val.items() if kk != "docLink"}
                            new_rest = {kk: vv for kk, vv in new_val.items() if kk != "docLink"}
                            if old_rest == new_rest:
                                both_valid = True
                    if both_valid:
                        benign.append(f"      {k}: multi-module (both valid)\n        old: {old_link}\n        new: {new_link}")
                    else:
                        mismatches.append(f"      {k}:\n        old: {old_val}\n        new: {new_val}")

        if mismatches:
            errors.append(f"{section}: {len(mismatches)} values differ")
            for m in mismatches[:20]:
                detail.append(m)
            if len(mismatches) > 20:
                detail.append(f"  ... and {len(mismatches) - 20} more")
        if benign:
            info.append(f"{section}: {len(benign)} multi-module (both valid)")
            for m in benign[:10]:
                detail.append(m)
            if len(benign) > 10:
                detail.append(f"  ... and {len(benign) - 10} more")

    # Check for unknown top-level keys
    for k in sorted(set(old_data.keys()) | set(new_data.keys())):
        if k not in ("declarations", "instances", "instancesFor", "modules"):
            errors.append(f"unknown top-level key: {k}")

    return errors, info, detail


def compare_data_files(
    dir1: Path, dir2: Path, shared_files: set[str],
    old_targets: set[str] | None = None, new_targets: set[str] | None = None,
) -> tuple[int, list[tuple[str, str, str]], list[tuple[str, str, str]]]:
    """Compare JSON/BMP data files between directories.

    declaration-data.bmp gets domain-specific comparison.
    Other JSON files are compared by structural equality of their parsed
    Python representations.

    Returns (identical_count, error_tuples, info_tuples) where each tuple list
    contains (path, summary, verbose_detail) entries.  Error tuples are real
    differences; info tuples are benign (e.g. multi-module docLink).
    """
    identical = 0
    different: list[tuple[str, str, str]] = []
    info_items: list[tuple[str, str, str]] = []

    for f in sorted(shared_files):
        content1 = (dir1 / f).read_bytes()
        content2 = (dir2 / f).read_bytes()

        if content1 == content2:
            identical += 1
            continue

        try:
            j1 = json.loads(content1)
            j2 = json.loads(content2)
        except (json.JSONDecodeError, UnicodeDecodeError, ValueError):
            different.append((f, "not valid JSON", ""))
            continue

        if j1 == j2:
            identical += 1
            continue

        # Domain-specific comparison for declaration-data files
        if Path(f).name == "declaration-data.bmp":
            error_lines, info_lines, detail_lines = compare_declaration_data(j1, j2, old_targets, new_targets)
            if error_lines:
                summary = "; ".join(error_lines)
                verbose = "\n".join(detail_lines)
                different.append((f, summary, verbose))
            elif info_lines:
                summary = "; ".join(info_lines)
                verbose = "\n".join(detail_lines)
                info_items.append((f, summary, verbose))
            else:
                identical += 1
        else:
            different.append((f, "content differs", ""))

    return identical, different, info_items


def run_declaration_census(
    db_path: Path, new_targets: set[str]
) -> tuple[int, list[tuple[str, str]]]:
    """Verify every rendered declaration in the DB has an HTML anchor in dir2.

    Checks that for each (module_name, name) with render=1 in the database,
    the target "Module/Path.html#name" exists in new_targets.

    Returns (total_checked, list_of_missing_(module, name)_pairs).
    """
    conn = sqlite3.connect(str(db_path))
    cursor = conn.execute(
        "SELECT module_name, name FROM name_info WHERE render = 1"
    )
    total = 0
    missing: list[tuple[str, str]] = []
    for module_name, name in cursor:
        total += 1
        file_path = module_name.replace(".", "/") + ".html"
        # HTML id attributes have <, >, & escaped; DB stores them raw
        escaped_name = html.escape(name, quote=False)
        target = f"{file_path}#{escaped_name}"
        if target not in new_targets:
            missing.append((module_name, name))
    conn.close()
    return total, missing


# Extensions for static assets that should be compared byte-for-byte.
STATIC_ASSET_EXTS = {".css", ".js", ".svg", ".png", ".ico", ".woff", ".woff2"}


def compare_static_assets(
    dir1: Path, dir2: Path, shared_files: set[str]
) -> tuple[int, list[str]]:
    """Compare static asset files byte-for-byte.

    Returns (identical_count, list_of_different_file_paths).
    """
    identical = 0
    different: list[str] = []
    for f in sorted(shared_files):
        content1 = (dir1 / f).read_bytes()
        content2 = (dir2 / f).read_bytes()
        if content1 == content2:
            identical += 1
        else:
            different.append(f)
    return identical, different


def compare_target_coverage(
    old_targets: set[str], new_targets: set[str]
) -> tuple[int, int, list[str], list[str]]:
    """Compare link target coverage between old and new HTML.

    Checks for anchored targets (file.html#name) that exist in one
    direction but not the other. Targets only in old may indicate
    accidentally dropped declarations.

    Returns (old_count, new_count, only_in_old, only_in_new).
    """
    old_anchors = {t for t in old_targets if "#" in t}
    new_anchors = {t for t in new_targets if "#" in t}
    only_in_old = sorted(old_anchors - new_anchors)
    only_in_new = sorted(new_anchors - old_anchors)
    return len(old_anchors), len(new_anchors), only_in_old, only_in_new


def render_sample_diff(diff: Difference) -> list[str]:
    """Render a sample difference for the per-rule summary.

    Returns a list of lines (without indentation prefix).
    For reorder diffs, shows the reordered element names.
    For other diffs, shows the diff view as if the difference were rejected.
    """
    if diff.rule_name == "allow_reorder_same_source_position":
        old_name = _extract_decl_name_from_context(diff.old_elem, diff.old_ancestors)
        new_name = _extract_decl_name_from_context(diff.new_elem, diff.new_ancestors)
        line_info = ""
        if diff.reason and "line " in diff.reason:
            m = re.search(r"line (\d+)", diff.reason)
            if m:
                line_info = f" (line {m.group(1)})"
        return [f"{diff.file_path}: {old_name} \u2194 {new_name}{line_info}"]

    # Same format as print_difference for rejected diffs
    attr_str = f" '{diff.attribute_name}'" if diff.attribute_name else ""
    lines = [f"{diff.file_path}: {diff.diff_type}{attr_str}"]

    old_parent = diff.old_ancestors[0] if diff.old_ancestors else None
    new_parent = diff.new_ancestors[0] if diff.new_ancestors else None
    old_html = str(old_parent) if old_parent else "(root)"
    new_html = str(new_parent) if new_parent else "(root)"

    if old_html != new_html and old_html != "(root)" and new_html != "(root)":
        diff_lines = format_unified_diff(old_html, new_html)
        if diff_lines:
            for dl in diff_lines:
                lines.append(dl)
        else:
            lines.append(f"Old: {format_parent_with_children(old_parent)}")
            lines.append(f"New: {format_parent_with_children(new_parent)}")
    else:
        lines.append(f"Old: {format_parent_with_children(old_parent)}")
        lines.append(f"New: {format_parent_with_children(new_parent)}")

    return lines


def print_summary(
    total_files: int,
    files_with_diffs: int,
    total_rejected: int,
    total_accepted: int,
    files_only_in_dir1: int,
    files_only_in_dir2: int,
    total_elapsed_ms: float,
    data_files_identical: int = 0,
    data_files_different: int = 0,
    data_only_in_dir1: int = 0,
    data_only_in_dir2: int = 0,
    census_total: int = 0,
    census_missing: int = 0,
    static_identical: int = 0,
    static_different: int = 0,
    static_only_in_dir1: int = 0,
    static_only_in_dir2: int = 0,
    coverage_old: int = 0,
    coverage_new: int = 0,
    coverage_dropped: int = 0,
    coverage_added: int = 0,
    rule_stats: dict[str, int] | None = None,
    rule_samples: dict[str, list[list[str]]] | None = None,
) -> None:
    """Print the final summary."""
    log("\n" + "=" * 60)
    log(f"SUMMARY ({total_elapsed_ms:.1f}ms)")
    log("=" * 60)
    log(f"  HTML files compared: {total_files}")
    log(f"  Files with differences: {files_with_diffs}")
    log(f"  Total rejected differences: {total_rejected}")
    log(f"  Total accepted differences: {total_accepted}")
    log(f"  Files only in dir1: {files_only_in_dir1}")
    log(f"  Files only in dir2: {files_only_in_dir2}")
    log(f"  Data files compared: {data_files_identical + data_files_different}")
    log(f"  Data files identical: {data_files_identical}")
    log(f"  Data files different: {data_files_different}")
    log(f"  Data files only in dir1: {data_only_in_dir1}")
    log(f"  Data files only in dir2: {data_only_in_dir2}")
    log(f"  Static assets compared: {static_identical + static_different}")
    log(f"  Static assets identical: {static_identical}")
    log(f"  Static assets different: {static_different}")
    log(f"  Static assets only in dir1: {static_only_in_dir1}")
    log(f"  Static assets only in dir2: {static_only_in_dir2}")
    log(f"  Declaration census: {census_total} checked, {census_missing} missing")
    log(f"  Target coverage: {coverage_old} old anchors, {coverage_new} new anchors, {coverage_dropped} dropped, {coverage_added} added")
    if rule_stats:
        log(f"\n  Accepted differences by rule:")
        for rule_name, count in sorted(rule_stats.items(), key=lambda x: -x[1]):
            log(f"    {rule_name}: {count:,}")
            if rule_samples and rule_name in rule_samples:
                for sample_lines in rule_samples[rule_name]:
                    for i, line in enumerate(sample_lines):
                        prefix = "      e.g. " if i == 0 else "           "
                        log(f"{prefix}{line}")



# =============================================================================
# Tests for compare_code_elements
# =============================================================================


def run_tests() -> int:
    """Run unit tests for compare_code_elements."""
    passed = 0
    failed = 0

    def test(
        name: str,
        old_html: str,
        new_html: str,
        expected_ok: bool,
        old_targets: set[str] | None = None,
        new_targets: set[str] | None = None,
    ) -> None:
        nonlocal passed, failed

        if old_targets is None:
            old_targets = set()
        if new_targets is None:
            new_targets = set()

        old_code = BeautifulSoup(old_html, PARSER).find("code")
        new_code = BeautifulSoup(new_html, PARSER).find("code")

        if old_code is None or new_code is None:
            log(f"  {RED}FAIL{RESET}: {name}")
            log(f"    Could not parse code elements")
            failed += 1
            return

        is_ok, reason = compare_code_elements(
            old_code, new_code, "test.html", old_targets, new_targets
        )

        if is_ok == expected_ok:
            log(f"  {GREEN}PASS{RESET}: {name}")
            passed += 1
        else:
            log(f"  {RED}FAIL{RESET}: {name}")
            log(f"    Expected: {'OK' if expected_ok else 'REJECTED'}")
            log(f"    Got: {'OK' if is_ok else 'REJECTED'} ({reason})")
            log(f"    Old: {old_html}")
            log(f"    New: {new_html}")
            failed += 1

    log("Running compare_code_elements tests...\n")

    # Test 1: Identical content
    test(
        "identical content",
        "<code>foo bar</code>",
        "<code>foo bar</code>",
        expected_ok=True,
    )

    # Test 2: Different text - should fail
    test(
        "different text",
        "<code>foo bar</code>",
        "<code>foo baz</code>",
        expected_ok=False,
    )

    # Test 3: Adding a valid link
    test(
        "add valid link",
        "<code>Foo.bar</code>",
        '<code><a href="Foo.html#bar">Foo.bar</a></code>',
        expected_ok=True,
        new_targets={"Foo.html#bar"},
    )

    # Test 4: Adding an invalid link - should fail
    test(
        "add invalid link",
        "<code>Foo.bar</code>",
        '<code><a href="Foo.html#bar">Foo.bar</a></code>',
        expected_ok=False,
        new_targets=set(),  # Target doesn't exist
    )

    # Test 5: Removing a broken link
    test(
        "remove broken link",
        '<code><a href="Missing.html#gone">foo</a></code>',
        "<code>foo</code>",
        expected_ok=True,
        old_targets=set(),  # Link was broken
    )

    # Test 6: Removing a valid link - should fail
    test(
        "remove valid link",
        '<code><a href="Valid.html#exists">foo</a></code>',
        "<code>foo</code>",
        expected_ok=False,
        old_targets={"Valid.html#exists"},
    )

    # Test 7: span.fn to valid link
    test(
        "span.fn to valid link",
        '<code><span class="fn">foo</span></code>',
        '<code><a href="Foo.html">foo</a></code>',
        expected_ok=True,
        new_targets={"Foo.html"},
    )

    # Test 8: span.fn to invalid link - should fail
    test(
        "span.fn to invalid link",
        '<code><span class="fn">foo</span></code>',
        '<code><a href="Missing.html">foo</a></code>',
        expected_ok=False,
        new_targets=set(),
    )

    # Test 9: broken link to span.fn
    test(
        "broken link to span.fn",
        '<code><a href="Missing.html">foo</a></code>',
        '<code><span class="fn">foo</span></code>',
        expected_ok=True,
        old_targets=set(),
    )

    # Test 10: valid link to span.fn - should fail
    test(
        "valid link to span.fn",
        '<code><a href="Valid.html">foo</a></code>',
        '<code><span class="fn">foo</span></code>',
        expected_ok=False,
        old_targets={"Valid.html"},
    )

    # Test 11: Mixed content with valid link additions
    test(
        "mixed content with valid links",
        '<code>Foo.<span class="fn">bar</span>.baz</code>',
        '<code><a href="Foo.html">Foo</a>.<a href="Foo/bar.html">bar</a>.<a href="baz.html">baz</a></code>',
        expected_ok=True,
        new_targets={"Foo.html", "Foo/bar.html", "baz.html"},
    )

    # Test 12: href change - both valid
    test(
        "href change both valid",
        '<code><a href="Old.html#foo">text</a></code>',
        '<code><a href="New.html#bar">text</a></code>',
        expected_ok=True,
        old_targets={"Old.html#foo"},
        new_targets={"New.html#bar"},
    )

    # Test 13: href change - old broken, new valid
    test(
        "href change broken to valid",
        '<code><a href="Missing.html">text</a></code>',
        '<code><a href="Valid.html">text</a></code>',
        expected_ok=True,
        old_targets=set(),
        new_targets={"Valid.html"},
    )

    # Test 14: href change - both broken - should fail
    test(
        "href change both broken",
        '<code><a href="Missing1.html">text</a></code>',
        '<code><a href="Missing2.html">text</a></code>',
        expected_ok=False,
        old_targets=set(),
        new_targets=set(),
    )

    # Test 15: Adding span.fn (plain text to styled)
    test(
        "add span.fn",
        "<code>foo</code>",
        '<code><span class="fn">foo</span></code>',
        expected_ok=True,
    )

    # Test 16: Removing span.fn
    test(
        "remove span.fn",
        '<code><span class="fn">foo</span></code>',
        "<code>foo</code>",
        expected_ok=True,
    )

    # Test 17: Whitespace differences should be OK
    test(
        "whitespace differences",
        "<code>foo  bar</code>",
        "<code>foo bar</code>",
        expected_ok=True,
    )

    # Test 18: Complex nested structure
    test(
        "nested wrappers with link",
        '<code>A.<span class="fn">B</span>.C</code>',
        '<code>A.<a href="B.html"><span class="fn">B</span></a>.C</code>',
        expected_ok=True,
        new_targets={"B.html"},
    )

    # Test 19: Different wrapper class - should fail
    test(
        "different span class",
        '<code><span class="fn">foo</span></code>',
        '<code><span class="other">foo</span></code>',
        expected_ok=False,
    )

    # Test 20: Real-world example from doc comparison (simplified paths)
    test(
        "real-world: text wrapped in link",
        '<code>Lean.Data.<a href="Init/Data/RArray.html#Lean.RArray">RArray</a></code>',
        '<code><a href="Lean/Data/RArray.html">Lean.Data.RArray</a></code>',
        expected_ok=True,
        old_targets={"Init/Data/RArray.html#Lean.RArray"},
        new_targets={"Lean/Data/RArray.html"},
    )

    # Test 21: li.equation with broken link to nested span.fn
    test(
        "li.equation: broken link to nested span.fn",
        '<code>@<a href="Missing.html#foo">text</a>, bar</code>',
        '<code><span class="fn">@<span class="fn">text</span></span>, bar</code>',
        expected_ok=True,
        old_targets=set(),  # Link was broken
    )

    # Test 22: li.equation with valid link to nested span.fn - should fail
    test(
        "li.equation: valid link to nested span.fn",
        '<code>@<a href="Valid.html#foo">text</a>, bar</code>',
        '<code><span class="fn">@<span class="fn">text</span></span>, bar</code>',
        expected_ok=False,
        old_targets={"Valid.html#foo"},  # Link was valid
    )

    # Test 23: Multiple words becoming links (e.g., "forget₂ CommMonCat MonCat")
    test(
        "multiple words wrapped in links",
        "<code>forget₂ CommMonCat MonCat</code>",
        '<code>forget₂ <a href="CommMonCat.html#CommMonCat">CommMonCat</a> <a href="MonCat.html#MonCat">MonCat</a></code>',
        expected_ok=True,
        new_targets={"CommMonCat.html#CommMonCat", "MonCat.html#MonCat"},
    )

    log(f"\n{passed} passed, {failed} failed")

    # Run acceptance rule tests
    rp, rf = run_rule_tests()
    passed += rp
    failed += rf

    log(f"\nTotal: {passed} passed, {failed} failed")
    return 0 if failed == 0 else 1


def run_rule_tests() -> tuple[int, int]:
    """Run unit tests for acceptance rules."""
    passed = 0
    failed = 0

    def make_tag(html_str: str) -> Tag:
        """Parse HTML and return the first tag.
        Uses html.parser (not lxml) because lxml wraps fragments in <html><body>."""
        return BeautifulSoup(html_str, "html.parser").find(True)

    def test(name: str, rule: Rule, ctx: DiffContext, expected_accepts: bool) -> None:
        nonlocal passed, failed
        result = rule(ctx)
        if (result is not None) == expected_accepts:
            log(f"  {GREEN}PASS{RESET}: {name}")
            passed += 1
        else:
            log(f"  {RED}FAIL{RESET}: {name}")
            log(f"    Expected: {'accept' if expected_accepts else 'reject'}")
            log(f"    Got: {result!r}")
            failed += 1

    log("\nRunning acceptance rule tests...\n")

    # --- allow_lean_file_href_change ---

    test(
        "lean_file_href: same file different tmp dir",
        allow_lean_file_href_change,
        DiffContext(
            file_path="Foo.html", diff_type="attribute", attribute_name="href",
            old_elem=make_tag('<a href="file:///tmp/aaa/.lake/pkg/Foo.lean">src</a>'),
            new_elem=make_tag('<a href="file:///tmp/bbb/.lake/pkg/Foo.lean">src</a>'),
            old_ancestors=[], new_ancestors=[],
            old_value="file:///tmp/aaa/.lake/pkg/Foo.lean",
            new_value="file:///tmp/bbb/.lake/pkg/Foo.lean",
            old_targets=set(), new_targets=set(),
        ),
        expected_accepts=True,
    )
    test(
        "lean_file_href: different file after .lake",
        allow_lean_file_href_change,
        DiffContext(
            file_path="Foo.html", diff_type="attribute", attribute_name="href",
            old_elem=make_tag('<a href="file:///tmp/aaa/.lake/pkg/Foo.lean">src</a>'),
            new_elem=make_tag('<a href="file:///tmp/bbb/.lake/pkg/Bar.lean">src</a>'),
            old_ancestors=[], new_ancestors=[],
            old_value="file:///tmp/aaa/.lake/pkg/Foo.lean",
            new_value="file:///tmp/bbb/.lake/pkg/Bar.lean",
            old_targets=set(), new_targets=set(),
        ),
        expected_accepts=False,
    )

    # --- allow_href_change_if_old_broken ---

    test(
        "href_change_old_broken: old broken, accept",
        allow_href_change_if_old_broken,
        DiffContext(
            file_path="Foo.html", diff_type="attribute", attribute_name="href",
            old_elem=make_tag('<a href="Missing.html#x">text</a>'),
            new_elem=make_tag('<a href="Other.html#y">text</a>'),
            old_ancestors=[], new_ancestors=[],
            old_value="Missing.html#x", new_value="Other.html#y",
            old_targets=set(), new_targets={"Other.html#y"},
        ),
        expected_accepts=True,
    )
    test(
        "href_change_old_broken: old valid, reject",
        allow_href_change_if_old_broken,
        DiffContext(
            file_path="Foo.html", diff_type="attribute", attribute_name="href",
            old_elem=make_tag('<a href="Valid.html#x">text</a>'),
            new_elem=make_tag('<a href="Other.html#y">text</a>'),
            old_ancestors=[], new_ancestors=[],
            old_value="Valid.html#x", new_value="Other.html#y",
            old_targets={"Valid.html#x"}, new_targets={"Other.html#y"},
        ),
        expected_accepts=False,
    )

    # --- allow_href_change_from_private ---

    test(
        "href_from_private: old has _private, new valid",
        allow_href_change_from_private,
        DiffContext(
            file_path="Foo.html", diff_type="attribute", attribute_name="href",
            old_elem=make_tag('<a href="Foo.html#_private.X.Y">text</a>'),
            new_elem=make_tag('<a href="Foo.html#X">text</a>'),
            old_ancestors=[], new_ancestors=[],
            old_value="Foo.html#_private.X.Y", new_value="Foo.html#X",
            old_targets=set(), new_targets={"Foo.html#X"},
        ),
        expected_accepts=True,
    )
    test(
        "href_from_private: no _private in old, reject",
        allow_href_change_from_private,
        DiffContext(
            file_path="Foo.html", diff_type="attribute", attribute_name="href",
            old_elem=make_tag('<a href="Foo.html#X">text</a>'),
            new_elem=make_tag('<a href="Foo.html#Y">text</a>'),
            old_ancestors=[], new_ancestors=[],
            old_value="Foo.html#X", new_value="Foo.html#Y",
            old_targets=set(), new_targets={"Foo.html#Y"},
        ),
        expected_accepts=False,
    )

    # --- allow_href_change_same_anchor_valid_target ---

    test(
        "same_anchor: same fragment, new valid",
        allow_href_change_same_anchor_valid_target,
        DiffContext(
            file_path="Foo.html", diff_type="attribute", attribute_name="href",
            old_elem=make_tag('<a href="A.html#foo">text</a>'),
            new_elem=make_tag('<a href="B.html#foo">text</a>'),
            old_ancestors=[], new_ancestors=[],
            old_value="A.html#foo", new_value="B.html#foo",
            old_targets={"A.html#foo"}, new_targets={"B.html#foo"},
        ),
        expected_accepts=True,
    )
    test(
        "same_anchor: different fragment, reject",
        allow_href_change_same_anchor_valid_target,
        DiffContext(
            file_path="Foo.html", diff_type="attribute", attribute_name="href",
            old_elem=make_tag('<a href="A.html#foo">text</a>'),
            new_elem=make_tag('<a href="B.html#bar">text</a>'),
            old_ancestors=[], new_ancestors=[],
            old_value="A.html#foo", new_value="B.html#bar",
            old_targets={"A.html#foo"}, new_targets={"B.html#bar"},
        ),
        expected_accepts=False,
    )

    # --- allow_a_to_span_if_broken ---

    test(
        "a_to_span: broken link to span.fn",
        allow_a_to_span_if_broken,
        DiffContext(
            file_path="Foo.html", diff_type="element_replaced",
            old_elem=make_tag('<a href="Missing.html">text</a>'),
            new_elem=make_tag('<span class="fn">text</span>'),
            old_ancestors=[], new_ancestors=[],
            attribute_name=None, old_value=None, new_value=None,
            old_targets=set(), new_targets=set(),
        ),
        expected_accepts=True,
    )
    test(
        "a_to_span: valid link to span.fn, reject",
        allow_a_to_span_if_broken,
        DiffContext(
            file_path="Foo.html", diff_type="element_replaced",
            old_elem=make_tag('<a href="Valid.html">text</a>'),
            new_elem=make_tag('<span class="fn">text</span>'),
            old_ancestors=[], new_ancestors=[],
            attribute_name=None, old_value=None, new_value=None,
            old_targets={"Valid.html"}, new_targets=set(),
        ),
        expected_accepts=False,
    )

    # --- allow_unwrap_broken_link ---

    test(
        "unwrap_broken: broken <a> removed",
        allow_unwrap_broken_link,
        DiffContext(
            file_path="Foo.html", diff_type="element_removed",
            old_elem=make_tag('<a href="Missing.html">text</a>'),
            new_elem=None,
            old_ancestors=[], new_ancestors=[],
            attribute_name=None, old_value=None, new_value=None,
            old_targets=set(), new_targets=set(),
        ),
        expected_accepts=True,
    )
    test(
        "unwrap_broken: valid <a> removed, reject",
        allow_unwrap_broken_link,
        DiffContext(
            file_path="Foo.html", diff_type="element_removed",
            old_elem=make_tag('<a href="Valid.html">text</a>'),
            new_elem=None,
            old_ancestors=[], new_ancestors=[],
            attribute_name=None, old_value=None, new_value=None,
            old_targets={"Valid.html"}, new_targets=set(),
        ),
        expected_accepts=False,
    )

    # --- allow_span_fn_to_link ---

    test(
        "span_to_link: span.fn to valid <a>",
        allow_span_fn_to_link,
        DiffContext(
            file_path="Foo.html", diff_type="element_replaced",
            old_elem=make_tag('<span class="fn">text</span>'),
            new_elem=make_tag('<a href="Target.html">text</a>'),
            old_ancestors=[], new_ancestors=[],
            attribute_name=None, old_value=None, new_value=None,
            old_targets=set(), new_targets={"Target.html"},
        ),
        expected_accepts=True,
    )
    test(
        "span_to_link: span.fn to invalid <a>, reject",
        allow_span_fn_to_link,
        DiffContext(
            file_path="Foo.html", diff_type="element_replaced",
            old_elem=make_tag('<span class="fn">text</span>'),
            new_elem=make_tag('<a href="Missing.html">text</a>'),
            old_ancestors=[], new_ancestors=[],
            attribute_name=None, old_value=None, new_value=None,
            old_targets=set(), new_targets=set(),
        ),
        expected_accepts=False,
    )

    # --- allow_added_link_with_valid_target ---

    test(
        "added_link: valid target",
        allow_added_link_with_valid_target,
        DiffContext(
            file_path="Foo.html", diff_type="element_added",
            old_elem=None,
            new_elem=make_tag('<a href="Target.html#x">text</a>'),
            old_ancestors=[], new_ancestors=[],
            attribute_name=None, old_value=None, new_value=None,
            old_targets=set(), new_targets={"Target.html#x"},
        ),
        expected_accepts=True,
    )
    test(
        "added_link: invalid target, reject",
        allow_added_link_with_valid_target,
        DiffContext(
            file_path="Foo.html", diff_type="element_added",
            old_elem=None,
            new_elem=make_tag('<a href="Missing.html#x">text</a>'),
            old_ancestors=[], new_ancestors=[],
            attribute_name=None, old_value=None, new_value=None,
            old_targets=set(), new_targets=set(),
        ),
        expected_accepts=False,
    )

    # --- allow_inherited_field_id ---

    test(
        "inherited_field_id: li.inherited_field gets id",
        allow_inherited_field_id,
        DiffContext(
            file_path="Foo.html", diff_type="attribute", attribute_name="id",
            old_elem=make_tag('<li class="inherited_field">field</li>'),
            new_elem=make_tag('<li class="inherited_field" id="Struct.field">field</li>'),
            old_ancestors=[], new_ancestors=[],
            old_value=None, new_value="Struct.field",
            old_targets=set(), new_targets=set(),
        ),
        expected_accepts=True,
    )
    test(
        "inherited_field_id: plain li gets id, reject",
        allow_inherited_field_id,
        DiffContext(
            file_path="Foo.html", diff_type="attribute", attribute_name="id",
            old_elem=make_tag("<li>item</li>"),
            new_elem=make_tag('<li id="item">item</li>'),
            old_ancestors=[], new_ancestors=[],
            old_value=None, new_value="item",
            old_targets=set(), new_targets=set(),
        ),
        expected_accepts=False,
    )

    # --- allow_empty_equations_removal ---

    test(
        "empty_equations: empty equations details removed",
        allow_empty_equations_removal,
        DiffContext(
            file_path="Foo.html", diff_type="element_removed",
            old_elem=make_tag('<details><summary>Equations</summary><ul class="equations"></ul></details>'),
            new_elem=None,
            old_ancestors=[], new_ancestors=[],
            attribute_name=None, old_value=None, new_value=None,
            old_targets=set(), new_targets=set(),
        ),
        expected_accepts=True,
    )
    test(
        "empty_equations: non-empty equations, reject",
        allow_empty_equations_removal,
        DiffContext(
            file_path="Foo.html", diff_type="element_removed",
            old_elem=make_tag('<details><summary>Equations</summary><ul class="equations"><li>eq1</li></ul></details>'),
            new_elem=None,
            old_ancestors=[], new_ancestors=[],
            attribute_name=None, old_value=None, new_value=None,
            old_targets=set(), new_targets=set(),
        ),
        expected_accepts=False,
    )

    # --- allow_duplicate_li_removal_in_imports ---

    def make_imports_ctx(old_lis: list[str], new_lis: list[str]) -> DiffContext:
        """Helper: build a DiffContext inside div.imports > ul with given <li> contents."""
        old_ul_html = "<ul>" + "".join(f"<li>{x}</li>" for x in old_lis) + "</ul>"
        new_ul_html = "<ul>" + "".join(f"<li>{x}</li>" for x in new_lis) + "</ul>"
        old_imports = make_tag(f'<div class="imports">{old_ul_html}</div>')
        new_imports = make_tag(f'<div class="imports">{new_ul_html}</div>')
        old_ul = old_imports.find("ul")
        new_ul = new_imports.find("ul")
        return DiffContext(
            file_path="Foo.html", diff_type="element_removed",
            old_elem=old_ul.find("li"), new_elem=None,
            old_ancestors=[old_ul, old_imports],
            new_ancestors=[new_ul, new_imports],
            attribute_name=None, old_value=None, new_value=None,
            old_targets=set(), new_targets=set(),
        )

    test(
        "dedup_imports: old has dupes, new deduplicated",
        allow_duplicate_li_removal_in_imports,
        make_imports_ctx(["A", "A", "B"], ["A", "B"]),
        expected_accepts=True,
    )
    test(
        "dedup_imports: no dupes in old, reject",
        allow_duplicate_li_removal_in_imports,
        make_imports_ctx(["A", "B"], ["A"]),
        expected_accepts=False,
    )
    test(
        "dedup_imports: new adds entry not in old, reject",
        allow_duplicate_li_removal_in_imports,
        make_imports_ctx(["A", "A", "B"], ["A", "B", "C"]),
        expected_accepts=False,
    )

    # --- allow_extends_id_wrapper ---

    def make_extends_ctx(
        old_header_html: str, new_header_html: str,
        diff_type: str = "element_added",
        new_elem_html: str | None = None,
    ) -> DiffContext:
        """Helper: build a DiffContext inside decl_header with extends."""
        old_header = make_tag(old_header_html)
        new_header = make_tag(new_header_html)
        new_elem = make_tag(new_elem_html) if new_elem_html else None
        return DiffContext(
            file_path="Foo.html", diff_type=diff_type,
            old_elem=None, new_elem=new_elem,
            old_ancestors=[old_header], new_ancestors=[new_header],
            attribute_name=None, old_value=None, new_value=None,
            old_targets=set(), new_targets=set(),
        )

    test(
        "extends_id: wrapper span in header with extends, same text",
        allow_extends_id_wrapper,
        make_extends_ctx(
            '<div class="decl_header">Foo extends <span class="decl_extends">Bar</span></div>',
            '<div class="decl_header">Foo extends <span class="decl_extends"><span id="Foo.toBar">Bar</span></span></div>',
            new_elem_html='<span id="Foo.toBar">Bar</span>',
        ),
        expected_accepts=True,
    )
    test(
        "extends_id: text changed, reject",
        allow_extends_id_wrapper,
        make_extends_ctx(
            '<div class="decl_header">Foo extends <span class="decl_extends">Bar</span></div>',
            '<div class="decl_header">Foo extends <span class="decl_extends"><span id="Foo.toBar">Baz</span></span></div>',
            new_elem_html='<span id="Foo.toBar">Baz</span>',
        ),
        expected_accepts=False,
    )
    test(
        "extends_id: no decl_extends in old, reject",
        allow_extends_id_wrapper,
        make_extends_ctx(
            '<div class="decl_header">Foo : Type</div>',
            '<div class="decl_header">Foo : <span id="Foo.x">Type</span></div>',
            new_elem_html='<span id="Foo.x">Type</span>',
        ),
        expected_accepts=False,
    )

    # --- allow_reorder_same_source_position ---

    # Save and restore global state
    saved_positions = _db_decl_positions.copy()

    _db_decl_positions.clear()
    _db_decl_positions[("Test.Module", "declA")] = (42, 0)
    _db_decl_positions[("Test.Module", "declB")] = (42, 0)
    _db_decl_positions[("Test.Module", "declC")] = (99, 0)

    both_targets = {"Test/Module.html#declA", "Test/Module.html#declB", "Test/Module.html#declC"}

    test(
        "reorder: same position, both in old+new targets",
        allow_reorder_same_source_position,
        DiffContext(
            file_path="Test/Module.html", diff_type="attribute", attribute_name="id",
            old_elem=make_tag('<div class="decl" id="declA">A</div>'),
            new_elem=make_tag('<div class="decl" id="declB">B</div>'),
            old_ancestors=[], new_ancestors=[],
            old_value="declA", new_value="declB",
            old_targets=both_targets, new_targets=both_targets,
        ),
        expected_accepts=True,
    )
    test(
        "reorder: different positions, reject",
        allow_reorder_same_source_position,
        DiffContext(
            file_path="Test/Module.html", diff_type="attribute", attribute_name="id",
            old_elem=make_tag('<div class="decl" id="declA">A</div>'),
            new_elem=make_tag('<div class="decl" id="declC">C</div>'),
            old_ancestors=[], new_ancestors=[],
            old_value="declA", new_value="declC",
            old_targets=both_targets, new_targets=both_targets,
        ),
        expected_accepts=False,
    )
    test(
        "reorder: same position but target missing from new, reject",
        allow_reorder_same_source_position,
        DiffContext(
            file_path="Test/Module.html", diff_type="attribute", attribute_name="id",
            old_elem=make_tag('<div class="decl" id="declA">A</div>'),
            new_elem=make_tag('<div class="decl" id="declB">B</div>'),
            old_ancestors=[], new_ancestors=[],
            old_value="declA", new_value="declB",
            old_targets=both_targets, new_targets={"Test/Module.html#declA"},
        ),
        expected_accepts=False,
    )

    _db_decl_positions.clear()
    _db_decl_positions.update(saved_positions)

    log(f"\n{passed} passed, {failed} failed")
    return passed, failed


def main_cli() -> int:
    """Main CLI entry point with subcommand support."""
    # If first arg is not a known command or flag, assume it's a directory and prepend "compare"
    if len(sys.argv) > 1 and sys.argv[1] not in ("compare", "test", "json", "-h", "--help"):
        sys.argv.insert(1, "compare")

    parser = argparse.ArgumentParser(
        description="HTML Documentation Diff Comparison Tool",
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )

    subparsers = parser.add_subparsers(dest="command", help="Available commands")

    # Compare command (default)
    compare_parser = subparsers.add_parser(
        "compare", help="Compare two documentation directories"
    )
    compare_parser.add_argument("dir1", type=Path, help="First documentation directory (old)")
    compare_parser.add_argument("dir2", type=Path, help="Second documentation directory (new)")
    compare_parser.add_argument(
        "-v", "--verbose", action="store_true", help="Show accepted differences too"
    )
    compare_parser.add_argument(
        "-j",
        "--jobs",
        type=int,
        default=8,
        help="Number of parallel jobs (default: 8)",
    )
    compare_parser.add_argument(
        "--restrict",
        type=str,
        default=None,
        help="Only compare HTML files under this subdirectory (link targets still use full dirs)",
    )
    compare_parser.add_argument(
        "--cache-link-targets",
        type=Path,
        default=None,
        help="Cache file for link targets (loads if exists, creates otherwise)",
    )
    compare_parser.add_argument(
        "--db",
        type=Path,
        required=True,
        help="doc-gen4 SQLite database for source position lookups and declaration census",
    )

    # JSON comparison command
    json_parser = subparsers.add_parser(
        "json", help="Compare only JSON/BMP data files (fast iteration)"
    )
    json_parser.add_argument("dir1", type=Path, help="First documentation directory (old)")
    json_parser.add_argument("dir2", type=Path, help="Second documentation directory (new)")
    json_parser.add_argument(
        "--db",
        type=Path,
        default=None,
        help="doc-gen4 SQLite database for resolving multi-module declarations",
    )

    # Test command
    subparsers.add_parser("test", help="Run unit tests for compare_code_elements")

    args = parser.parse_args()

    if args.command == "test":
        return run_tests()
    elif args.command == "json":
        return run_json_compare(args)
    elif args.command == "compare":
        return main(args)
    else:
        parser.print_help()
        return 0


def run_json_compare(args: argparse.Namespace) -> int:
    """Compare only JSON/BMP data files between two directories."""
    if not args.dir1.is_dir():
        print(f"Error: {args.dir1} is not a directory", file=sys.stderr)
        return 1
    if not args.dir2.is_dir():
        print(f"Error: {args.dir2} is not a directory", file=sys.stderr)
        return 1

    start = time.perf_counter()
    log(f"Comparing JSON/BMP files in {args.dir1} vs {args.dir2}")

    # Load targets from DB if provided
    db_targets: set[str] | None = None
    if args.db:
        db_targets = load_db_targets(args.db)
        log(f"Loaded {len(db_targets)} targets from {args.db}")

    _, _, _, other_only_dir1, other_only_dir2, other_both = compare_directories(args.dir1, args.dir2)
    data_exts = {".json", ".bmp"}
    other_only_dir1 = {f for f in other_only_dir1 if Path(f).suffix in data_exts}
    other_only_dir2 = {f for f in other_only_dir2 if Path(f).suffix in data_exts}
    other_both = {f for f in other_both if Path(f).suffix in data_exts}

    has_errors = False

    if other_only_dir1:
        log(f"\nData files only in dir1: {len(other_only_dir1)}")
        for f in sorted(other_only_dir1):
            log(f"  {f}")
        has_errors = True

    if other_only_dir2:
        log(f"\nData files only in dir2: {len(other_only_dir2)}")
        for f in sorted(other_only_dir2):
            log(f"  {f}")
        has_errors = True

    data_identical, data_different, data_info = compare_data_files(
        args.dir1, args.dir2, other_both, db_targets, db_targets
    )
    elapsed = (time.perf_counter() - start) * 1000
    log(f"\nCompared {len(other_both)} data files ({elapsed:.1f}ms)")
    log(f"  Identical: {data_identical}")
    log(f"  Different: {len(data_different)}")
    if data_different:
        has_errors = True
        for f, summary, verbose in data_different:
            log(f"\n  {f}: {summary}")
            for line in verbose.splitlines():
                log(f"    {line}")
    if data_info:
        log(f"  Info (benign): {len(data_info)}")
        for f, summary, verbose in data_info:
            log(f"\n  {f}: {summary}")
            for line in verbose.splitlines():
                log(f"    {line}")

    return 1 if has_errors else 0


def main(args: argparse.Namespace | None = None) -> int:
    """Main comparison function."""
    if args is None:
        parser = argparse.ArgumentParser(
            description="Compare two HTML documentation directories",
            formatter_class=argparse.RawDescriptionHelpFormatter,
        )
        parser.add_argument("dir1", type=Path, help="First documentation directory (old)")
        parser.add_argument("dir2", type=Path, help="Second documentation directory (new)")
        parser.add_argument(
            "-v", "--verbose", action="store_true", help="Show accepted differences too"
        )
        parser.add_argument(
            "-j",
            "--jobs",
            type=int,
            default=8,
            help="Number of parallel jobs (default: 8)",
        )
        parser.add_argument(
            "--restrict",
            type=str,
            default=None,
            help="Only compare HTML files under this subdirectory (link targets still use full dirs)",
        )
        parser.add_argument(
            "--cache-link-targets",
            type=Path,
            default=None,
            help="Cache file for link targets (loads if exists, creates otherwise)",
        )
        parser.add_argument(
            "--db",
            type=Path,
            required=True,
            help="doc-gen4 SQLite database for source position lookups and declaration census",
        )
        args = parser.parse_args()

    if not args.dir1.is_dir():
        print(f"Error: {args.dir1} is not a directory", file=sys.stderr)
        return 1

    if not args.dir2.is_dir():
        print(f"Error: {args.dir2} is not a directory", file=sys.stderr)
        return 1

    if not args.db.exists():
        print(f"Error: database {args.db} not found", file=sys.stderr)
        return 1

    # Load DB declaration positions for reorder acceptance
    global _db_decl_positions
    _db_decl_positions = load_db_decl_positions(args.db)
    log(f"Loaded {len(_db_decl_positions)} declaration positions from {args.db}")

    total_start = time.perf_counter()
    log(f"Comparing {args.dir1} vs {args.dir2}")

    # Step 1: Compare directory contents
    step_start = time.perf_counter()
    html_only_dir1, html_only_dir2, html_both, other_only_dir1, other_only_dir2, other_both = compare_directories(
        args.dir1, args.dir2
    )
    # Split non-HTML files into data files (.json, .bmp) and static assets (.css, .js, etc.)
    data_exts = {".json", ".bmp"}
    data_only_dir1 = {f for f in other_only_dir1 if Path(f).suffix in data_exts}
    data_only_dir2 = {f for f in other_only_dir2 if Path(f).suffix in data_exts}
    data_both = {f for f in other_both if Path(f).suffix in data_exts}
    static_only_dir1 = {f for f in other_only_dir1 if Path(f).suffix in STATIC_ASSET_EXTS}
    static_only_dir2 = {f for f in other_only_dir2 if Path(f).suffix in STATIC_ASSET_EXTS}
    static_both = {f for f in other_both if Path(f).suffix in STATIC_ASSET_EXTS}
    step_elapsed = (time.perf_counter() - step_start) * 1000

    log(f"Scanning directories... ({step_elapsed:.1f}ms)")
    log(f"  HTML files in both: {len(html_both)}")
    log(f"  HTML files only in dir1: {len(html_only_dir1)}")
    log(f"  HTML files only in dir2: {len(html_only_dir2)}")
    log(f"  Data files in both: {len(data_both)}")
    log(f"  Data files only in dir1: {len(data_only_dir1)}")
    log(f"  Data files only in dir2: {len(data_only_dir2)}")
    log(f"  Static assets in both: {len(static_both)}")
    log(f"  Static assets only in dir1: {len(static_only_dir1)}")
    log(f"  Static assets only in dir2: {len(static_only_dir2)}")

    # Step 2: Extract link targets (or load from cache)
    step_start = time.perf_counter()
    cache_file = getattr(args, "cache_link_targets", None)
    if cache_file and cache_file.exists():
        import pickle
        with open(cache_file, "rb") as f:
            cached = pickle.load(f)
        old_targets = cached["old"]
        new_targets = cached["new"]
        step_elapsed = (time.perf_counter() - step_start) * 1000
        log(f"Loaded link targets from cache '{cache_file}' ({step_elapsed:.1f}ms)")
    else:
        all_html_dir1 = html_both | html_only_dir1
        all_html_dir2 = html_both | html_only_dir2
        old_targets = extract_link_targets(args.dir1, all_html_dir1, args.jobs)
        new_targets = extract_link_targets(args.dir2, all_html_dir2, args.jobs)
        step_elapsed = (time.perf_counter() - step_start) * 1000
        log(f"Extracting link targets... ({step_elapsed:.1f}ms)")
        if cache_file:
            import pickle
            with open(cache_file, "wb") as f:
                pickle.dump({"old": old_targets, "new": new_targets}, f)
            log(f"Saved link targets to cache '{cache_file}'")

    log(f"  Targets in dir1: {len(old_targets)}")
    log(f"  Targets in dir2: {len(new_targets)}")

    # Restrict comparison to subdirectory if requested
    restrict = getattr(args, "restrict", None)
    if restrict:
        prefix = restrict.rstrip("/") + "/"
        html_both = {f for f in html_both if f.startswith(prefix) or f == restrict}
        html_only_dir1 = {f for f in html_only_dir1 if f.startswith(prefix) or f == restrict}
        html_only_dir2 = {f for f in html_only_dir2 if f.startswith(prefix) or f == restrict}
        log(f"Restricted to '{restrict}': {len(html_both)} files to compare")

    # Report files only in one directory
    has_errors = False
    files_only_dir1 = sorted(html_only_dir1)
    files_only_dir2 = sorted(html_only_dir2)

    if files_only_dir1:
        log("\n" + "=" * 60)
        log("HTML FILES ONLY IN DIR1:")
        log("=" * 60)
        for f in files_only_dir1:
            log(f"  {f}")
        has_errors = True

    if files_only_dir2:
        log("\n" + "=" * 60)
        log("HTML FILES ONLY IN DIR2:")
        log("=" * 60)
        for f in files_only_dir2:
            log(f"  {f}")
        has_errors = True

    if data_only_dir1:
        log("\n" + "=" * 60)
        log("DATA FILES ONLY IN DIR1:")
        log("=" * 60)
        for f in sorted(data_only_dir1):
            log(f"  {f}")
        has_errors = True

    if data_only_dir2:
        log("\n" + "=" * 60)
        log("DATA FILES ONLY IN DIR2:")
        log("=" * 60)
        for f in sorted(data_only_dir2):
            log(f"  {f}")
        has_errors = True

    if static_only_dir1:
        log("\n" + "=" * 60)
        log("STATIC ASSETS ONLY IN DIR1:")
        log("=" * 60)
        for f in sorted(static_only_dir1):
            log(f"  {f}")
        has_errors = True

    if static_only_dir2:
        log("\n" + "=" * 60)
        log("STATIC ASSETS ONLY IN DIR2:")
        log("=" * 60)
        for f in sorted(static_only_dir2):
            log(f"  {f}")
        has_errors = True

    # Step 3: Compare data files
    step_start = time.perf_counter()
    data_identical, data_different, data_info = compare_data_files(args.dir1, args.dir2, data_both, old_targets, new_targets)
    step_elapsed = (time.perf_counter() - step_start) * 1000
    log(f"\nComparing {len(data_both)} data files... ({step_elapsed:.1f}ms)")
    log(f"  Identical: {data_identical}")
    log(f"  Different: {len(data_different)}")
    if data_different:
        has_errors = True
        for f, summary, verbose in data_different:
            log(f"\n  {f}: {summary}")
            for line in verbose.splitlines():
                log(f"    {line}")
    if data_info:
        for f, summary, verbose in data_info:
            log(f"\n  {f}: {summary}")
            for line in verbose.splitlines():
                log(f"    {line}")

    # Step 4: Declaration census
    step_start = time.perf_counter()
    census_total, census_missing = run_declaration_census(args.db, new_targets)
    step_elapsed = (time.perf_counter() - step_start) * 1000
    log(f"\nDeclaration census... ({step_elapsed:.1f}ms)")
    log(f"  Declarations checked: {census_total}")
    log(f"  Missing from HTML: {len(census_missing)}")
    if census_missing:
        has_errors = True
        log("\n" + "=" * 60)
        log("MISSING DECLARATIONS:")
        log("=" * 60)
        for module_name, name in census_missing[:50]:
            log(f"  {module_name}: {name}")
        if len(census_missing) > 50:
            log(f"  ... and {len(census_missing) - 50} more")

    # Step 5: Compare static assets
    step_start = time.perf_counter()
    static_identical, static_different = compare_static_assets(args.dir1, args.dir2, static_both)
    step_elapsed = (time.perf_counter() - step_start) * 1000
    log(f"\nComparing {len(static_both)} static assets... ({step_elapsed:.1f}ms)")
    log(f"  Identical: {static_identical}")
    log(f"  Different: {len(static_different)}")
    if static_different:
        has_errors = True
        log("\n" + "=" * 60)
        log("DIFFERENT STATIC ASSETS:")
        log("=" * 60)
        for f in static_different:
            log(f"  {f}")

    # Step 6: Bidirectional target coverage
    step_start = time.perf_counter()
    cov_old, cov_new, cov_dropped, cov_added = compare_target_coverage(old_targets, new_targets)
    step_elapsed = (time.perf_counter() - step_start) * 1000
    log(f"\nTarget coverage... ({step_elapsed:.1f}ms)")
    log(f"  Anchored targets in dir1: {cov_old}")
    log(f"  Anchored targets in dir2: {cov_new}")
    log(f"  Dropped (in old, not new): {len(cov_dropped)}")
    log(f"  Added (in new, not old): {len(cov_added)}")
    if cov_dropped:
        has_errors = True
        log("\n" + "=" * 60)
        log("DROPPED TARGETS (in old HTML but not new):")
        log("=" * 60)
        for t in cov_dropped[:50]:
            log(f"  {t}")
        if len(cov_dropped) > 50:
            log(f"  ... and {len(cov_dropped) - 50} more")

    # Step 7: Compare HTML files in parallel, printing results in deterministic order
    log(f"\nComparing {len(html_both)} HTML files...")

    total_rejected = 0
    total_accepted = 0
    files_with_diffs = 0
    files_compared = 0
    rule_stats: dict[str, int] = {}
    rule_samples: dict[str, list[list[str]]] = {}

    # Sort file paths for deterministic output order
    sorted_files = sorted(html_both)
    results_by_path: dict[str, FileComparisonResult] = {}
    next_to_print = 0  # Index into sorted_files of next file to print

    try:
        with ThreadPoolExecutor(max_workers=args.jobs) as executor:
            futures = {
                executor.submit(
                    compare_html_files, file_path, args.dir1, args.dir2, old_targets, new_targets
                ): file_path
                for file_path in sorted_files
            }

            for future in as_completed(futures):
                file_path = futures[future]
                result = future.result()
                # Evaluate rules
                result.differences = evaluate_rules(
                    result.differences, RULES, old_targets, new_targets
                )
                results_by_path[file_path] = result

                # Print all consecutive results that are ready, in sorted order
                while next_to_print < len(sorted_files):
                    next_path = sorted_files[next_to_print]
                    if next_path not in results_by_path:
                        break  # Next file in order isn't ready yet

                    res = results_by_path.pop(next_path)
                    rejected, accepted, has_error = print_file_report(res, args.verbose)
                    total_rejected += rejected
                    total_accepted += accepted
                    if rejected or has_error:
                        has_errors = True
                    if rejected or accepted:
                        files_with_diffs += 1
                    files_compared += 1
                    # Collect per-rule acceptance stats
                    for diff in res.differences:
                        if diff.accepted and diff.rule_name:
                            rn = diff.rule_name
                            rule_stats[rn] = rule_stats.get(rn, 0) + 1
                            if rn not in rule_samples:
                                rule_samples[rn] = []
                            # Reservoir sampling: keep 5 uniformly random samples per rule.
                            # Render immediately to avoid keeping soup objects alive.
                            n = rule_stats[rn]
                            if len(rule_samples[rn]) < 5:
                                rule_samples[rn].append(render_sample_diff(diff))
                            else:
                                j = random.randint(0, n - 1)
                                if j < 5:
                                    rule_samples[rn][j] = render_sample_diff(diff)
                    next_to_print += 1

    except KeyboardInterrupt:
        log("\n\nInterrupted! Cancelling pending tasks...")
        executor.shutdown(wait=False, cancel_futures=True)
        log(f"Processed {files_compared} of {len(sorted_files)} files before interrupt.")
        return 130  # Standard exit code for SIGINT

    # Step 8: Print summary
    total_elapsed = (time.perf_counter() - total_start) * 1000
    print_summary(
        total_files=len(html_both),
        files_with_diffs=files_with_diffs,
        total_rejected=total_rejected,
        total_accepted=total_accepted,
        files_only_in_dir1=len(files_only_dir1),
        files_only_in_dir2=len(files_only_dir2),
        total_elapsed_ms=total_elapsed,
        data_files_identical=data_identical,
        data_files_different=len(data_different),
        data_only_in_dir1=len(data_only_dir1),
        data_only_in_dir2=len(data_only_dir2),
        census_total=census_total,
        census_missing=len(census_missing),
        static_identical=static_identical,
        static_different=len(static_different),
        static_only_in_dir1=len(static_only_dir1),
        static_only_in_dir2=len(static_only_dir2),
        coverage_old=cov_old,
        coverage_new=cov_new,
        coverage_dropped=len(cov_dropped),
        coverage_added=len(cov_added),
        rule_stats=rule_stats,
        rule_samples=rule_samples,
    )

    return 1 if has_errors else 0


if __name__ == "__main__":
    sys.exit(main_cli())
