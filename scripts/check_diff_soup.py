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
import re
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
        if not href or href.startswith(("#", "javascript:", "mailto:", "data:")):
            # Fragment-only or special URLs
            if href.startswith("#"):
                return f"{self.file_path}{href}"
            return href

        # Handle external URLs
        if href.startswith(("http://", "https://", "//")):
            return href

        # Resolve relative path
        current_dir = str(Path(self.file_path).parent)
        if current_dir == ".":
            resolved = href
        else:
            resolved = urljoin(self.file_path, href)

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

        resolved = self._resolve_href(str(href))

        # Check if target exists
        if resolved in self.old_targets:
            return False

        # Check file without fragment
        path_only = resolved.split("#")[0]
        if path_only in self.old_targets:
            # File exists, but fragment might not
            if "#" in resolved:
                return resolved not in self.old_targets
            return False

        return True

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

        resolved = self._resolve_href(str(href))

        # Check if target exists
        if resolved in self.new_targets:
            return False

        # Check file without fragment
        path_only = resolved.split("#")[0]
        if path_only in self.new_targets:
            if "#" in resolved:
                return resolved not in self.new_targets
            return False

        return True

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


def _check_href_valid(href: str, file_path: str, targets: set[str]) -> bool:
    """Check if an href target exists in the given target set."""
    if not href:
        return False
    # Skip external links
    if href.startswith(("http://", "https://", "//", "javascript:", "mailto:", "#")):
        return True  # External/anchor links are considered valid

    # Resolve relative path
    from urllib.parse import unquote, urljoin

    current_dir = str(Path(file_path).parent)
    if current_dir == ".":
        resolved = href
    else:
        resolved = urljoin(file_path, href)

    resolved = unquote(resolved)
    parts = resolved.split("#", 1)
    path_part = parts[0]

    try:
        normalized = str(Path(path_part).as_posix())
        if normalized.startswith(".."):
            normalized = path_part
    except Exception:
        normalized = path_part

    if len(parts) > 1:
        resolved = f"{normalized}#{parts[1]}"
    else:
        resolved = normalized

    # Check if target exists
    if resolved in targets:
        return True
    # If there's a fragment, the exact target must exist
    # (don't fall back to just the file)
    if "#" in resolved:
        return False
    # No fragment - check if file exists
    return resolved in targets


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

    # Check if new is the deduplicated version of old
    new_unique = set(new_lis)
    if old_unique == new_unique and len(new_lis) == len(new_unique):
        return "duplicate <li> removed from imports"

    return None


# Default rules list
# Note: <code> elements are handled specially by compare_code_elements() in compare_trees()
# These rules handle differences outside of <code> elements
RULES: list[Rule] = [
    allow_href_change_if_old_broken,
    allow_a_to_span_if_broken,
    allow_span_fn_to_link,
    allow_unwrap_broken_link,
    allow_added_link_with_valid_target,
    allow_duplicate_li_removal_in_imports,
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
) -> tuple[set[str], set[str], set[str], set[str], set[str]]:
    """
    Compare file inventories of two directories.

    Returns:
        Tuple of:
        - HTML files only in dir1
        - HTML files only in dir2
        - HTML files in both
        - Other files only in dir1
        - Other files only in dir2
    """
    html1, other1 = collect_files(dir1)
    html2, other2 = collect_files(dir2)

    return (
        html1 - html2,
        html2 - html1,
        html1 & html2,
        other1 - other2,
        other2 - other1,
    )


# =============================================================================
# Link Target Extraction
# =============================================================================


# Regex patterns for extracting link targets (faster than full HTML parsing)
# Require attributes to be inside complete HTML tags: <tagname ... id="value" ...>
# Use non-greedy [^>]*? to match the first id/name attribute in the tag
ID_PATTERN = re.compile(r'<\w[^>]*?\bid=["\']([^"\']+)["\'][^>]*>', re.IGNORECASE)
NAME_PATTERN = re.compile(r'<\w[^>]*?\bname=["\']([^"\']+)["\'][^>]*>', re.IGNORECASE)


def extract_targets_from_file(directory: Path, rel_path: str) -> set[str]:
    """Extract link targets from a single HTML file using regex."""
    targets: set[str] = {rel_path}  # File itself is a target

    file_path = directory / rel_path
    try:
        content = file_path.read_text(encoding="utf-8", errors="replace")

        # Extract id attributes
        for match in ID_PATTERN.finditer(content):
            targets.add(f"{rel_path}#{match.group(1)}")

        # Extract name attributes
        for match in NAME_PATTERN.finditer(content):
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
        # Get more text if needed
        while not old_text and old_stack:
            text, tag, done = advance(old_stack)
            if text:
                old_text = text
            elif done:
                break

        while not new_text and new_stack:
            text, tag, done = advance(new_stack)
            if text:
                new_text = text
            elif done:
                break

        # Strip leading whitespace from both
        old_text = old_text.lstrip()
        new_text = new_text.lstrip()

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


def get_significant_children(elem: Tag) -> list[Tag | NavigableString]:
    """Get children that are significant for comparison (elements and non-whitespace text)."""
    result = []
    for child in elem.children:
        if isinstance(child, Tag):
            result.append(child)
        elif isinstance(child, NavigableString):
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
        # Class lists - sort for comparison
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
    old_children = get_significant_children(old_node)
    new_children = get_significant_children(new_node)

    # Simple alignment: compare by index
    max_len = max(len(old_children), len(new_children))
    for i in range(max_len):
        old_child = old_children[i] if i < len(old_children) else None
        new_child = new_children[i] if i < len(new_children) else None
        differences.extend(
            compare_trees(old_child, new_child, file_path, next_old_ancestors, next_new_ancestors, old_targets, new_targets)
        )

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

        # Compare from body if exists, otherwise from root
        old_body = old_soup.body or old_soup
        new_body = new_soup.body or new_soup

        result.differences = compare_trees(
            old_body, new_body, file_path, [], [], old_targets, new_targets
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
    """Evaluate rules against differences, marking accepted ones."""
    for diff in differences:
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


def print_summary(
    total_files: int,
    files_with_diffs: int,
    total_rejected: int,
    total_accepted: int,
    files_only_in_dir1: int,
    files_only_in_dir2: int,
    total_elapsed_ms: float,
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


# =============================================================================
# Main
# =============================================================================


def main() -> int:
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

    args = parser.parse_args()

    if not args.dir1.is_dir():
        print(f"Error: {args.dir1} is not a directory", file=sys.stderr)
        return 1

    if not args.dir2.is_dir():
        print(f"Error: {args.dir2} is not a directory", file=sys.stderr)
        return 1

    total_start = time.perf_counter()
    log(f"Comparing {args.dir1} vs {args.dir2}")

    # Step 1: Compare directory contents
    step_start = time.perf_counter()
    html_only_dir1, html_only_dir2, html_both, other_only_dir1, other_only_dir2 = compare_directories(
        args.dir1, args.dir2
    )
    step_elapsed = (time.perf_counter() - step_start) * 1000

    log(f"Scanning directories... ({step_elapsed:.1f}ms)")
    log(f"  HTML files in both: {len(html_both)}")
    log(f"  HTML files only in dir1: {len(html_only_dir1)}")
    log(f"  HTML files only in dir2: {len(html_only_dir2)}")

    # Step 2: Extract link targets
    step_start = time.perf_counter()
    all_html_dir1 = html_both | html_only_dir1
    all_html_dir2 = html_both | html_only_dir2
    old_targets = extract_link_targets(args.dir1, all_html_dir1, args.jobs)
    new_targets = extract_link_targets(args.dir2, all_html_dir2, args.jobs)
    step_elapsed = (time.perf_counter() - step_start) * 1000

    log(f"Extracting link targets... ({step_elapsed:.1f}ms)")
    log(f"  Targets in dir1: {len(old_targets)}")
    log(f"  Targets in dir2: {len(new_targets)}")

    # Report HTML files only in one directory (skip non-HTML files)
    has_errors = False
    files_only_dir1 = sorted(html_only_dir1)
    files_only_dir2 = sorted(html_only_dir2)

    if files_only_dir1:
        log("\n" + "=" * 60)
        log("FILES ONLY IN DIR1:")
        log("=" * 60)
        for f in files_only_dir1:
            log(f"  {f}")
        has_errors = True

    if files_only_dir2:
        log("\n" + "=" * 60)
        log("FILES ONLY IN DIR2:")
        log("=" * 60)
        for f in files_only_dir2:
            log(f"  {f}")
        has_errors = True

    # Step 3: Compare HTML files in parallel, printing results in deterministic order
    log(f"\nComparing {len(html_both)} HTML files...")

    total_rejected = 0
    total_accepted = 0
    files_with_diffs = 0
    files_compared = 0

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
                    next_to_print += 1

    except KeyboardInterrupt:
        log("\n\nInterrupted! Cancelling pending tasks...")
        executor.shutdown(wait=False, cancel_futures=True)
        log(f"Processed {files_compared} of {len(sorted_files)} files before interrupt.")
        return 130  # Standard exit code for SIGINT

    # Step 4: Print summary
    total_elapsed = (time.perf_counter() - total_start) * 1000
    print_summary(
        total_files=len(html_both),
        files_with_diffs=files_with_diffs,
        total_rejected=total_rejected,
        total_accepted=total_accepted,
        files_only_in_dir1=len(files_only_dir1),
        files_only_in_dir2=len(files_only_dir2),
        total_elapsed_ms=total_elapsed,
    )

    return 1 if has_errors else 0


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

    log(f"\n{passed} passed, {failed} failed")
    return 0 if failed == 0 else 1


def main_cli() -> int:
    """Main CLI entry point with subcommand support."""
    # If first arg is not a known command or flag, assume it's a directory and prepend "compare"
    if len(sys.argv) > 1 and sys.argv[1] not in ("compare", "test", "-h", "--help"):
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

    # Test command
    subparsers.add_parser("test", help="Run unit tests for compare_code_elements")

    args = parser.parse_args()

    if args.command == "test":
        return run_tests()
    elif args.command == "compare":
        return main(args)
    else:
        parser.print_help()
        return 0


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
        args = parser.parse_args()

    if not args.dir1.is_dir():
        print(f"Error: {args.dir1} is not a directory", file=sys.stderr)
        return 1

    if not args.dir2.is_dir():
        print(f"Error: {args.dir2} is not a directory", file=sys.stderr)
        return 1

    total_start = time.perf_counter()
    log(f"Comparing {args.dir1} vs {args.dir2}")

    # Step 1: Compare directory contents
    step_start = time.perf_counter()
    html_only_dir1, html_only_dir2, html_both, other_only_dir1, other_only_dir2 = compare_directories(
        args.dir1, args.dir2
    )
    step_elapsed = (time.perf_counter() - step_start) * 1000

    log(f"Scanning directories... ({step_elapsed:.1f}ms)")
    log(f"  HTML files in both: {len(html_both)}")
    log(f"  HTML files only in dir1: {len(html_only_dir1)}")
    log(f"  HTML files only in dir2: {len(html_only_dir2)}")

    # Step 2: Extract link targets
    step_start = time.perf_counter()
    all_html_dir1 = html_both | html_only_dir1
    all_html_dir2 = html_both | html_only_dir2
    old_targets = extract_link_targets(args.dir1, all_html_dir1, args.jobs)
    new_targets = extract_link_targets(args.dir2, all_html_dir2, args.jobs)
    step_elapsed = (time.perf_counter() - step_start) * 1000

    log(f"Extracting link targets... ({step_elapsed:.1f}ms)")
    log(f"  Targets in dir1: {len(old_targets)}")
    log(f"  Targets in dir2: {len(new_targets)}")

    # Report HTML files only in one directory (skip non-HTML files)
    has_errors = False
    files_only_dir1 = sorted(html_only_dir1)
    files_only_dir2 = sorted(html_only_dir2)

    if files_only_dir1:
        log("\n" + "=" * 60)
        log("FILES ONLY IN DIR1:")
        log("=" * 60)
        for f in files_only_dir1:
            log(f"  {f}")
        has_errors = True

    if files_only_dir2:
        log("\n" + "=" * 60)
        log("FILES ONLY IN DIR2:")
        log("=" * 60)
        for f in files_only_dir2:
            log(f"  {f}")
        has_errors = True

    # Step 3: Compare HTML files in parallel, printing results in deterministic order
    log(f"\nComparing {len(html_both)} HTML files...")

    total_rejected = 0
    total_accepted = 0
    files_with_diffs = 0
    files_compared = 0

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
                    next_to_print += 1

    except KeyboardInterrupt:
        log("\n\nInterrupted! Cancelling pending tasks...")
        executor.shutdown(wait=False, cancel_futures=True)
        log(f"Processed {files_compared} of {len(sorted_files)} files before interrupt.")
        return 130  # Standard exit code for SIGINT

    # Step 4: Print summary
    total_elapsed = (time.perf_counter() - total_start) * 1000
    print_summary(
        total_files=len(html_both),
        files_with_diffs=files_with_diffs,
        total_rejected=total_rejected,
        total_accepted=total_accepted,
        files_only_in_dir1=len(files_only_dir1),
        files_only_in_dir2=len(files_only_dir2),
        total_elapsed_ms=total_elapsed,
    )

    return 1 if has_errors else 0


if __name__ == "__main__":
    sys.exit(main_cli())
