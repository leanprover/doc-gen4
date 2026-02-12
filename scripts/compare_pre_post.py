# /// script
# dependencies = ["lxml", "xmldiff"]
# ///
"""
Compare two directories of HTML documentation output, producing tree-based diffs
and filtering for allowed vs non-allowed differences.
"""

import argparse
import re
import sys
from concurrent.futures import ProcessPoolExecutor, as_completed
from dataclasses import dataclass
from functools import partial
from pathlib import Path
from typing import Callable, Iterator

from lxml import html
from lxml.html import HtmlElement
from xmldiff import main as xmldiff_main
from xmldiff.actions import (
    DeleteAttrib,
    DeleteNode,
    InsertAttrib,
    InsertNode,
    MoveNode,
    RenameAttrib,
    RenameNode,
    UpdateAttrib,
    UpdateTextAfter,
    UpdateTextIn,
)


# =============================================================================
# LINK TARGET COLLECTION
# =============================================================================


@dataclass
class LinkTargets:
    """Valid link targets in a directory."""

    files: set[Path]  # All HTML files (relative paths)
    ids: dict[Path, set[str]]  # file -> set of IDs in that file

    def has_target(self, href: str, from_file: Path) -> bool:
        """Check if a link href has a valid target."""
        if not href or href.startswith(("http://", "https://", "mailto:", "javascript:")):
            return True  # External links assumed valid

        # Parse href into file and fragment parts
        if "#" in href:
            file_part, fragment = href.split("#", 1)
        else:
            file_part, fragment = href, None

        # Determine target file
        if file_part:
            # Relative file link
            target_file = (from_file.parent / file_part).resolve()
            # Normalize to relative path
            try:
                # Find matching file in our set
                for f in self.files:
                    if f.resolve() == target_file or str(f) == file_part:
                        target_file = f
                        break
                else:
                    # Try resolving relative to from_file
                    rel_target = from_file.parent / file_part
                    if rel_target in self.files:
                        target_file = rel_target
                    else:
                        return False  # File not found
            except Exception:
                return False
        else:
            # Same-file anchor link
            target_file = from_file

        # Check fragment if present
        if fragment:
            file_ids = self.ids.get(target_file, set())
            return fragment in file_ids

        return target_file in self.files


def _extract_ids(tree: HtmlElement) -> set[str]:
    """Extract all id attribute values from an HTML tree."""
    ids = set()
    try:
        for elem in tree.xpath("//*[@id]"):
            if isinstance(elem, HtmlElement):
                id_val = elem.get("id")
                if id_val:
                    ids.add(id_val)
    except Exception:
        pass
    return ids


def collect_link_targets(directory: Path, files: set[Path]) -> LinkTargets:
    """Collect all valid link targets from HTML files in a directory."""
    ids: dict[Path, set[str]] = {}

    for rel_path in files:
        file_path = directory / rel_path
        try:
            with open(file_path, "rb") as f:
                tree = html.parse(f).getroot()
                if tree is not None:
                    ids[rel_path] = _extract_ids(tree)
        except Exception:
            ids[rel_path] = set()

    return LinkTargets(files=files, ids=ids)


# =============================================================================
# RULE SYSTEM
# =============================================================================


@dataclass
class DiffContext:
    """Context for evaluating whether a difference is allowed."""

    action: object  # xmldiff action
    old_tree: HtmlElement | None
    new_tree: HtmlElement | None
    old_dir: Path
    new_dir: Path
    rel_path: Path  # relative path of the file being compared
    old_targets: LinkTargets | None = None  # Valid link targets in old directory
    new_targets: LinkTargets | None = None  # Valid link targets in new directory


class Rule:
    """Base class for composable diff rules."""

    def __init__(
        self,
        predicate: Callable[[DiffContext], bool],
        reason: str | None = None,
    ):
        self._predicate = predicate
        self._reason = reason

    def __call__(self, ctx: DiffContext) -> bool:
        return self._predicate(ctx)

    def check_with_reason(self, ctx: DiffContext) -> str | None:
        """Check if rule matches and return the reason if it does."""
        if self._predicate(ctx):
            return self._reason or "unknown reason"
        return None

    def __and__(self, other: "Rule") -> "Rule":
        def combined(ctx: DiffContext) -> bool:
            return self(ctx) and other(ctx)
        # Use the more specific rule's reason (the second one)
        return Rule(combined, other._reason or self._reason)

    def __or__(self, other: "Rule") -> "Rule":
        def combined(ctx: DiffContext) -> bool:
            return self(ctx) or other(ctx)
        # For OR, we need to check which one matched
        combined_rule = Rule(combined)
        def get_reason(ctx: DiffContext) -> str | None:
            if self(ctx):
                return self._reason
            if other(ctx):
                return other._reason
            return None
        combined_rule.check_with_reason = get_reason  # type: ignore
        return combined_rule

    def __invert__(self) -> "Rule":
        return Rule(lambda ctx: not self(ctx), f"not {self._reason}" if self._reason else None)

    def with_reason(self, reason: str) -> "Rule":
        """Return a copy of this rule with the given reason."""
        return Rule(self._predicate, reason)


def make_rule(predicate: Callable[[DiffContext], bool], reason: str | None = None) -> Rule:
    """Create a rule from a predicate function."""
    return Rule(predicate, reason)


# -----------------------------------------------------------------------------
# Base predicates
# -----------------------------------------------------------------------------


def _is_deletion(ctx: DiffContext) -> bool:
    """Check if action is a deletion."""
    return isinstance(ctx.action, (DeleteNode, DeleteAttrib))


def _is_insertion(ctx: DiffContext) -> bool:
    """Check if action is an insertion."""
    return isinstance(ctx.action, (InsertNode, InsertAttrib))


def _is_text_change(ctx: DiffContext) -> bool:
    """Check if action is a text change."""
    return isinstance(ctx.action, (UpdateTextIn, UpdateTextAfter))


is_deletion = Rule(_is_deletion, "deletion")
is_insertion = Rule(_is_insertion, "insertion")
is_text_change = Rule(_is_text_change, "text change")


def _get_node_by_xpath(tree: HtmlElement | None, xpath: str) -> HtmlElement | None:
    """Get a node from the tree by xpath."""
    if tree is None:
        return None
    try:
        results = tree.xpath(xpath)
        if results and len(results) > 0:
            return results[0]
    except Exception:
        pass
    return None


def _get_action_node(ctx: DiffContext) -> HtmlElement | None:
    """Get the node affected by the action."""
    action = ctx.action
    tree = ctx.old_tree if _is_deletion(ctx) else ctx.new_tree

    if tree is None:
        return None

    node_path = None
    if hasattr(action, "node"):
        node_path = action.node
    elif hasattr(action, "target"):
        node_path = action.target

    if node_path is None:
        return None

    return _get_node_by_xpath(tree, node_path)


def _whitespace_only(ctx: DiffContext) -> bool:
    """Check if the change is whitespace-only."""
    action = ctx.action

    if isinstance(action, UpdateTextIn):
        # For UpdateTextIn, we need to compare with what was there before
        # The action contains the new text; we need to check if normalizing both
        # old and new would result in the same content
        if ctx.old_tree is None:
            return False
        node = _get_node_by_xpath(ctx.old_tree, action.node)
        if node is None:
            return False
        old_content = node.text or ""
        new_content = action.text or ""
        return old_content.strip() == new_content.strip()

    if isinstance(action, UpdateTextAfter):
        if ctx.old_tree is None:
            return False
        node = _get_node_by_xpath(ctx.old_tree, action.node)
        if node is None:
            return False
        old_content = node.tail or ""
        new_content = action.text or ""
        return old_content.strip() == new_content.strip()

    return False


whitespace_only = Rule(_whitespace_only, "whitespace-only change")


def _is_no_change(ctx: DiffContext) -> bool:
    """Check if this is a false-positive change where old and new values are identical."""
    action = ctx.action

    if isinstance(action, UpdateAttrib):
        old_value = _get_attrib(ctx.old_tree, action.node, action.name)
        new_value = action.value
        return old_value == new_value

    if isinstance(action, InsertAttrib):
        # If attribute already exists with same value in old tree, it's not a real change
        old_value = _get_attrib(ctx.old_tree, action.node, action.name)
        return old_value == action.value

    if isinstance(action, DeleteAttrib):
        # If attribute doesn't exist in old tree, it's not a real change
        old_value = _get_attrib(ctx.old_tree, action.node, action.name)
        return old_value is None

    if isinstance(action, (UpdateTextIn, UpdateTextAfter)):
        if isinstance(action, UpdateTextIn):
            old_value = _get_element_text(ctx.old_tree, action.node)
        else:
            old_value = _get_element_tail(ctx.old_tree, action.node)
        new_value = action.text
        return old_value == new_value

    return False


no_actual_change = Rule(_is_no_change, "no actual change")


def _is_broken_link(ctx: DiffContext) -> bool:
    """
    Check if the deleted element is a link that was already broken in the old version.
    A link removal is allowed if the link didn't have a valid target in the old directory.
    Only applies to deletions.
    """
    if not _is_deletion(ctx):
        return False

    node = _get_action_node(ctx)
    if node is None:
        return False

    # Check if it's an <a> tag or contains one
    links = []
    if node.tag == "a":
        links = [node]
    else:
        links = node.xpath(".//a[@href]")

    if not links:
        return False

    # If we have pre-collected targets, use them
    if ctx.old_targets is not None:
        for link in links:
            href = link.get("href", "")
            if not href:
                continue
            # Check if this link had a valid target in the old directory
            if not ctx.old_targets.has_target(href, ctx.rel_path):
                return True  # Link was broken in old version
        return False

    # Fallback: check filesystem directly (less accurate)
    for link in links:
        href = link.get("href", "")
        if not href:
            continue

        # Skip external links
        if href.startswith(("http://", "https://", "mailto:", "javascript:")):
            continue

        # Check if the target exists in old directory
        if href.startswith("#"):
            # Anchor link - check if ID exists in the old document
            anchor_id = href[1:]
            if ctx.old_tree is not None:
                found = ctx.old_tree.xpath(f'//*[@id="{anchor_id}"]')
                if not found:
                    return True  # Broken anchor link
        else:
            # File link - check if file exists in old directory
            file_part = href.split("#")[0]
            if file_part:
                target_path = ctx.old_dir / ctx.rel_path.parent / file_part
                if not target_path.exists():
                    return True  # Broken file link

    return False


broken_link = Rule(_is_broken_link, "broken link in old version")


def _is_broken_link_href_change(ctx: DiffContext) -> bool:
    """
    Check if this is a change to an href attribute where the old href was broken.
    Allows fixing broken links by changing their href to a valid target.
    """
    action = ctx.action

    # Only applies to UpdateAttrib on href
    if not isinstance(action, UpdateAttrib):
        return False
    if action.name != "href":
        return False

    # Get the old href value
    old_href = _get_attrib(ctx.old_tree, action.node, "href")
    if not old_href:
        return False

    # Check if old href was broken
    if ctx.old_targets is not None:
        # Use pre-collected targets
        return not ctx.old_targets.has_target(old_href, ctx.rel_path)
    else:
        # Fallback: check filesystem
        if old_href.startswith(("http://", "https://", "mailto:", "javascript:")):
            return False
        if old_href.startswith("#"):
            anchor_id = old_href[1:]
            if ctx.old_tree is not None:
                found = ctx.old_tree.xpath(f'//*[@id="{anchor_id}"]')
                return not found
        else:
            file_part = old_href.split("#")[0]
            if file_part:
                target_path = ctx.old_dir / ctx.rel_path.parent / file_part
                return not target_path.exists()
    return False


broken_link_href_change = Rule(_is_broken_link_href_change, "href change on broken link")


def in_context(selector: str) -> Rule:
    """
    Create a rule that checks if the affected node is within a given context.
    Selector format: "tag.class" or "tag" or ".class"
    """

    def check_context(ctx: DiffContext) -> bool:
        node = _get_action_node(ctx)
        if node is None:
            return False

        # Parse selector
        parts = selector.split(".")
        tag = parts[0] if parts[0] else None
        classes = parts[1:] if len(parts) > 1 else []

        # Walk up the ancestor chain
        current = node.getparent()
        while current is not None:
            if not isinstance(current, HtmlElement):
                current = current.getparent() if hasattr(current, "getparent") else None
                continue

            # Check tag match
            if tag and current.tag != tag:
                current = current.getparent()
                continue

            # Check class match
            if classes:
                node_classes = (current.get("class") or "").split()
                if not all(c in node_classes for c in classes):
                    current = current.getparent()
                    continue

            return True

        return False

    return Rule(check_context)


# =============================================================================
# ALLOWED DIFFERENCE RULES
# =============================================================================

# Declare all allowed-diff rules here for easy auditing
ALLOWED_RULES: list[Rule] = [
    # False positives where old == new
    no_actual_change,
    # Whitespace-only text changes are always allowed
    whitespace_only,
    # Broken links may be removed
    (is_deletion & broken_link).with_reason("removal of broken link"),
    # Broken link hrefs may be changed
    broken_link_href_change,
]


def check_allowed_difference(ctx: DiffContext) -> str | None:
    """Check if a difference is allowed and return the reason if so."""
    for rule in ALLOWED_RULES:
        reason = rule.check_with_reason(ctx)
        if reason:
            return reason
    return None


def is_allowed_difference(ctx: DiffContext) -> bool:
    """Check if a difference is allowed by any of the rules."""
    return check_allowed_difference(ctx) is not None


# =============================================================================
# DIRECTORY COMPARISON
# =============================================================================


def find_html_files(directory: Path) -> set[Path]:
    """Find all .html files in a directory, returning relative paths."""
    return {p.relative_to(directory) for p in directory.rglob("*.html")}


@dataclass
class FileCategories:
    """Categorization of files between two directories."""

    only_in_old: set[Path]
    only_in_new: set[Path]
    in_both: set[Path]


def categorize_files(old_dir: Path, new_dir: Path) -> FileCategories:
    """Categorize files as only-in-old, only-in-new, or in-both."""
    old_files = find_html_files(old_dir)
    new_files = find_html_files(new_dir)

    return FileCategories(
        only_in_old=old_files - new_files,
        only_in_new=new_files - old_files,
        in_both=old_files & new_files,
    )


def parse_html(path: Path) -> HtmlElement | None:
    """Parse an HTML file into a tree."""
    try:
        with open(path, "rb") as f:
            return html.parse(f).getroot()
    except Exception as e:
        print(f"Warning: Failed to parse {path}: {e}", file=sys.stderr)
        return None


def _convert_moves_to_delete_insert(
    actions: list, old_tree: HtmlElement, new_tree: HtmlElement
) -> list:
    """Convert MoveNode actions to DeleteNode + InsertNode pairs.

    xmldiff detects moves even when nodes are far apart in the document,
    which isn't semantically meaningful for our use case. Converting to
    delete+insert makes the diff easier to understand.
    """
    result = []
    for action in actions:
        if isinstance(action, MoveNode):
            # Get the tag of the moved node from the new tree
            node = _get_node_by_xpath(new_tree, action.node)
            tag = node.tag if node is not None else "unknown"

            # Create DeleteNode for the old location
            result.append(DeleteNode(node=action.node))

            # Create InsertNode for the new location
            result.append(InsertNode(target=action.target, tag=tag, position=action.position))
        else:
            result.append(action)
    return result


def _xpath_sort_key(xpath: str) -> list[tuple[str, int]]:
    """Convert xpath to a sortable key with proper numeric ordering.

    E.g., "/html/body/div[10]/p[2]" -> [("html", 1), ("body", 1), ("div", 10), ("p", 2)]
    """
    result = []
    # Split by / and parse each segment
    for segment in xpath.split("/"):
        if not segment:
            continue
        # Match tag name and optional index
        match = re.match(r"([a-zA-Z0-9_-]+)(?:\[(\d+)\])?", segment)
        if match:
            tag = match.group(1)
            idx = int(match.group(2)) if match.group(2) else 1
            result.append((tag, idx))
    return result


def _get_action_xpath(action: object) -> str:
    """Get the primary xpath from an action for sorting purposes."""
    if isinstance(action, InsertNode):
        return action.target
    elif hasattr(action, "node"):
        return action.node
    return ""


def _sort_actions_by_xpath(actions: list) -> list:
    """Sort actions by their xpath position in document order."""
    return sorted(actions, key=lambda a: _xpath_sort_key(_get_action_xpath(a)))


@dataclass
class FormattedDiff:
    """A formatted difference for display."""
    description: str  # Multi-line formatted description
    reason: str | None = None  # Why this diff is allowed (if applicable)


@dataclass
class FileDiff:
    """Differences found in a single file."""

    rel_path: Path
    allowed: list[FormattedDiff]  # Allowed differences (formatted)
    not_allowed: list[FormattedDiff]  # Non-allowed differences (formatted)
    parse_error: bool = False


def diff_file(
    old_dir: Path,
    new_dir: Path,
    old_targets: LinkTargets | None,
    new_targets: LinkTargets | None,
    rel_path: Path,
) -> FileDiff:
    """Compute differences for a single file."""
    old_path = old_dir / rel_path
    new_path = new_dir / rel_path

    old_tree = parse_html(old_path)
    new_tree = parse_html(new_path)

    if old_tree is None or new_tree is None:
        return FileDiff(rel_path=rel_path, allowed=[], not_allowed=[], parse_error=True)

    try:
        actions = xmldiff_main.diff_trees(old_tree, new_tree)
    except Exception as e:
        print(f"Warning: Failed to diff {rel_path}: {e}", file=sys.stderr)
        return FileDiff(rel_path=rel_path, allowed=[], not_allowed=[], parse_error=True)

    # Convert MoveNode to DeleteNode + InsertNode for clearer semantics
    actions = _convert_moves_to_delete_insert(actions, old_tree, new_tree)

    # Sort by xpath for consistent document-order output
    actions = _sort_actions_by_xpath(actions)

    allowed = []
    not_allowed = []

    for action in actions:
        ctx = DiffContext(
            action=action,
            old_tree=old_tree,
            new_tree=new_tree,
            old_dir=old_dir,
            new_dir=new_dir,
            rel_path=rel_path,
            old_targets=old_targets,
            new_targets=new_targets,
        )

        # Format the action while we have the trees
        # Check if this diff is allowed and get the reason
        reason = check_allowed_difference(ctx)
        formatted = FormattedDiff(
            description=format_action(action, old_tree, new_tree),
            reason=reason,
        )

        if reason:
            allowed.append(formatted)
        else:
            not_allowed.append(formatted)

    return FileDiff(rel_path=rel_path, allowed=allowed, not_allowed=not_allowed)


def diff_all_files(
    old_dir: Path,
    new_dir: Path,
    files: set[Path],
    old_targets: LinkTargets | None = None,
    new_targets: LinkTargets | None = None,
) -> Iterator[FileDiff]:
    """Diff all files in parallel, yielding results in sorted order as available."""
    sorted_files = sorted(files)
    if not sorted_files:
        return

    diff_fn = partial(diff_file, old_dir, new_dir, old_targets, new_targets)
    file_to_idx = {f: i for i, f in enumerate(sorted_files)}
    results: list[FileDiff | None] = [None] * len(sorted_files)
    next_to_yield = 0

    with ProcessPoolExecutor() as executor:
        futures = {executor.submit(diff_fn, f): f for f in sorted_files}

        for future in as_completed(futures):
            f = futures[future]
            idx = file_to_idx[f]
            results[idx] = future.result()

            # Yield all consecutive ready results
            while next_to_yield < len(results) and results[next_to_yield] is not None:
                yield results[next_to_yield]  # type: ignore
                results[next_to_yield] = None  # Free memory
                next_to_yield += 1


# =============================================================================
# OUTPUT
# =============================================================================


def _get_parent_xpath(xpath: str, levels: int = 1) -> str:
    """Get the xpath of an ancestor by going up N levels."""
    parts = xpath.rsplit("/", levels)
    return parts[0] if parts[0] else "/"


def _render_tree_summary(
    elem: HtmlElement,
    max_depth: int = 2,
    max_children: int = 3,
    indent: int = 0,
) -> list[str]:
    """Render a tree structure as HTML-like output."""
    lines = []
    prefix = "  " * indent

    # Build opening tag with all attributes
    tag = elem.tag
    attr_strs = []
    for attr, value in elem.attrib.items():
        attr_strs.append(f'{attr}="{value}"')

    if attr_strs:
        open_tag = f"<{tag} {' '.join(attr_strs)}>"
    else:
        open_tag = f"<{tag}>"

    # Get text content (preserve whitespace to show real differences)
    text = elem.text or ""
    children = [c for c in elem if isinstance(c, HtmlElement)]

    # Format based on content
    if not children and not text:
        # Empty element
        lines.append(f"{prefix}{open_tag}</{tag}>")
    elif not children:
        # Text-only element
        truncated = _truncate_text(text, 60)
        lines.append(f"{prefix}{open_tag}{truncated}</{tag}>")
    else:
        # Element with children
        lines.append(f"{prefix}{open_tag}")
        if text.strip():
            # Text before first child (only show if non-whitespace)
            truncated = _truncate_text(text, 60)
            lines.append(f"{prefix}  {truncated}")

        # Recurse into children (limited)
        if max_depth > 0:
            for child in children[:max_children]:
                lines.extend(_render_tree_summary(child, max_depth - 1, max_children, indent + 1))
            if len(children) > max_children:
                lines.append(f"{prefix}  ... ({len(children) - max_children} more)")
        else:
            lines.append(f"{prefix}  ...")

        lines.append(f"{prefix}</{tag}>")

    return lines


def _get_element_html(tree: HtmlElement | None, xpath: str, max_len: int = 200) -> str | None:
    """Get the HTML string for an element at the given xpath."""
    if tree is None:
        return None
    try:
        results = tree.xpath(xpath)
        if results and len(results) > 0:
            elem = results[0]
            if isinstance(elem, HtmlElement):
                html_str = html.tostring(elem, encoding="unicode")
                if len(html_str) > max_len:
                    html_str = html_str[:max_len] + "..."
                return html_str
            else:
                # Text node or other
                return repr(elem)[:max_len]
    except Exception:
        pass
    return None


def _get_element_text(tree: HtmlElement | None, xpath: str) -> str | None:
    """Get the text content of an element at the given xpath."""
    if tree is None:
        return None
    try:
        results = tree.xpath(xpath)
        if results and len(results) > 0:
            elem = results[0]
            if isinstance(elem, HtmlElement):
                return elem.text
            return str(elem)
    except Exception:
        pass
    return None


def _get_element_tail(tree: HtmlElement | None, xpath: str) -> str | None:
    """Get the tail text of an element at the given xpath."""
    if tree is None:
        return None
    try:
        results = tree.xpath(xpath)
        if results and len(results) > 0:
            elem = results[0]
            if isinstance(elem, HtmlElement):
                return elem.tail
    except Exception:
        pass
    return None


def _get_attrib(tree: HtmlElement | None, xpath: str, attr: str) -> str | None:
    """Get an attribute value from an element."""
    if tree is None:
        return None
    try:
        results = tree.xpath(xpath)
        if results and len(results) > 0:
            elem = results[0]
            if isinstance(elem, HtmlElement):
                return elem.get(attr)
    except Exception:
        pass
    return None


def _truncate_text(s: str, max_len: int = 80) -> str:
    """Truncate text intelligently for display.

    - If no whitespace, don't truncate
    - If truncation needed, break at first whitespace after max_len
    """
    if len(s) <= max_len:
        return s
    # If no whitespace, don't truncate
    if " " not in s and "\t" not in s and "\n" not in s:
        return s
    # Find first whitespace after max_len
    for i in range(max_len, len(s)):
        if s[i] in " \t\n":
            return s[:i] + "..."
    # No whitespace found after max_len, return full string
    return s


def _truncate(s: str | None, max_len: int = 100) -> str:
    """Truncate a string for display."""
    if s is None:
        return "<none>"
    return _truncate_text(s, max_len)
    return s


def _add_element_context(
    lines: list[str],
    old_tree: HtmlElement | None,
    new_tree: HtmlElement | None,
    xpath: str,
) -> None:
    """Add before/after tree context for an element at xpath."""
    # Get parent xpath for context (1 level up)
    parent_xpath = _get_parent_xpath(xpath, 1)

    # Show old tree context
    if old_tree is not None:
        try:
            parents = old_tree.xpath(parent_xpath)
            if parents and isinstance(parents[0], HtmlElement):
                lines.append("  Before:")
                for line in _render_tree_summary(parents[0], max_depth=1, max_children=3):
                    lines.append(f"    - {line}")
        except Exception:
            pass

    # Show new tree context
    if new_tree is not None:
        try:
            parents = new_tree.xpath(parent_xpath)
            if parents and isinstance(parents[0], HtmlElement):
                lines.append("  After:")
                for line in _render_tree_summary(parents[0], max_depth=1, max_children=3):
                    lines.append(f"    + {line}")
        except Exception:
            pass


def format_action(
    action: object,
    old_tree: HtmlElement | None = None,
    new_tree: HtmlElement | None = None,
) -> str:
    """Format a diff action for display with before/after content."""
    action_type = type(action).__name__
    lines = []

    if isinstance(action, DeleteNode):
        xpath = action.node
        old_html = _get_element_html(old_tree, xpath)
        lines.append(f"{action_type} at {xpath}")
        if old_html:
            lines.append(f"  - {_truncate(old_html)}")

    elif isinstance(action, InsertNode):
        target = action.target
        tag = action.tag
        position = action.position
        lines.append(f"{action_type}: <{tag}> into {target} at position {position}")

        # Show the parent context with siblings around the insertion point
        if new_tree is not None:
            try:
                parents = new_tree.xpath(target)
                if parents and isinstance(parents[0], HtmlElement):
                    parent = parents[0]
                    children = [c for c in parent if isinstance(c, HtmlElement)]

                    # Build context showing siblings around insertion
                    lines.append("  Inserted into:")

                    # Show parent opening tag
                    attr_strs = [f'{a}="{v}"' for a, v in parent.attrib.items()]
                    if attr_strs:
                        lines.append(f"    <{parent.tag} {' '.join(attr_strs)}>")
                    else:
                        lines.append(f"    <{parent.tag}>")

                    # Show elided content before
                    if position > 1:
                        lines.append(f"      ... ({position - 1} siblings)")

                    # Show left sibling if exists
                    if position > 0 and position - 1 < len(children):
                        for cl in _render_tree_summary(children[position - 1], max_depth=0):
                            lines.append(f"      {cl}")

                    # Show inserted element (highlighted)
                    if position < len(children):
                        for cl in _render_tree_summary(children[position], max_depth=1):
                            lines.append(f"  >>> {cl}")

                    # Show right sibling if exists
                    if position + 1 < len(children):
                        for cl in _render_tree_summary(children[position + 1], max_depth=0):
                            lines.append(f"      {cl}")

                    # Show elided content after
                    remaining = len(children) - position - 2
                    if remaining > 0:
                        lines.append(f"      ... ({remaining} more siblings)")

                    lines.append(f"    </{parent.tag}>")
            except Exception:
                pass

    elif isinstance(action, MoveNode):
        xpath = action.node
        target = action.target
        lines.append(f"{action_type}: {xpath} -> {target}")

        # Show context from old tree (parent of moved node)
        old_parent_xpath = _get_parent_xpath(xpath, 1)
        if old_tree is not None:
            try:
                old_parents = old_tree.xpath(old_parent_xpath)
                if old_parents and isinstance(old_parents[0], HtmlElement):
                    lines.append("  Before:")
                    for line in _render_tree_summary(old_parents[0], max_depth=2):
                        lines.append(f"    - {line}")
            except Exception:
                pass

        # Show context from new tree (target location)
        new_parent_xpath = _get_parent_xpath(target, 1) if "/" in target else target
        if new_tree is not None:
            try:
                new_parents = new_tree.xpath(new_parent_xpath)
                if new_parents and isinstance(new_parents[0], HtmlElement):
                    lines.append("  After:")
                    for line in _render_tree_summary(new_parents[0], max_depth=2):
                        lines.append(f"    + {line}")
            except Exception:
                pass

    elif isinstance(action, UpdateTextIn):
        xpath = action.node
        old_text = _get_element_text(old_tree, xpath)
        new_text = action.text or ""
        lines.append(f"{action_type} at {xpath}")
        lines.append(f"  text: {_truncate(repr(old_text))} -> {_truncate(repr(new_text))}")
        # Show element context
        _add_element_context(lines, old_tree, new_tree, xpath)

    elif isinstance(action, UpdateTextAfter):
        xpath = action.node
        old_text = _get_element_tail(old_tree, xpath)
        new_text = action.text or ""
        lines.append(f"{action_type} at {xpath}")
        lines.append(f"  tail: {_truncate(repr(old_text))} -> {_truncate(repr(new_text))}")
        _add_element_context(lines, old_tree, new_tree, xpath)

    elif isinstance(action, InsertAttrib):
        xpath = action.node
        attr = action.name
        value = action.value
        lines.append(f"{action_type} at {xpath}")
        lines.append(f"  + @{attr}={_truncate(repr(value))}")
        _add_element_context(lines, old_tree, new_tree, xpath)

    elif isinstance(action, DeleteAttrib):
        xpath = action.node
        attr = action.name
        old_value = _get_attrib(old_tree, xpath, attr)
        lines.append(f"{action_type} at {xpath}")
        lines.append(f"  - @{attr}={_truncate(repr(old_value))}")
        _add_element_context(lines, old_tree, new_tree, xpath)

    elif isinstance(action, UpdateAttrib):
        xpath = action.node
        attr = action.name
        old_value = _get_attrib(old_tree, xpath, attr)
        new_value = action.value
        lines.append(f"{action_type} at {xpath}")
        lines.append(f"  @{attr}: {_truncate(repr(old_value))} -> {_truncate(repr(new_value))}")
        _add_element_context(lines, old_tree, new_tree, xpath)

    elif isinstance(action, RenameAttrib):
        xpath = action.node
        old_name = action.name
        new_name = action.new_name if hasattr(action, "new_name") else "?"
        lines.append(f"{action_type} at {xpath}")
        lines.append(f"  @{old_name} -> @{new_name}")
        _add_element_context(lines, old_tree, new_tree, xpath)

    elif isinstance(action, RenameNode):
        xpath = action.node
        new_tag = action.tag
        lines.append(f"{action_type} at {xpath} -> <{new_tag}>")
        _add_element_context(lines, old_tree, new_tree, xpath)

    else:
        lines.append(str(action))

    return "\n".join(lines)


@dataclass
class Summary:
    """Summary of comparison results."""

    only_in_old: int
    only_in_new: int
    identical: int
    allowed_only: int
    has_real_diff: int
    parse_errors: int


def print_file_diff(diff: FileDiff, verbose: bool) -> None:
    """Print details for a single file diff."""
    print(f"\n--- {diff.rel_path} ---")

    if diff.parse_error:
        print("  [PARSE ERROR]")
        return

    if diff.not_allowed:
        print(f"  Non-allowed differences ({len(diff.not_allowed)}):")
        for fd in diff.not_allowed[:10]:  # Limit output
            # Indent multi-line output
            indented = "\n".join("      " + line for line in fd.description.split("\n"))
            print(f"    ! {indented.strip()}")
        if len(diff.not_allowed) > 10:
            print(f"      ... and {len(diff.not_allowed) - 10} more")

    if verbose and diff.allowed:
        print(f"  Allowed differences ({len(diff.allowed)}):")
        for fd in diff.allowed[:10]:  # Limit output
            reason_str = f" [{fd.reason}]" if fd.reason else ""
            indented = "\n".join("      " + line for line in fd.description.split("\n"))
            print(f"    ~ {indented.strip()}{reason_str}")
        if len(diff.allowed) > 10:
            print(f"      ... and {len(diff.allowed) - 10} more")


def print_results(
    categories: FileCategories,
    diffs: Iterator[FileDiff],
    verbose: bool,
) -> Summary:
    """Stream file diffs as they arrive, then print summary at the end."""
    identical = 0
    allowed_only = 0
    has_real_diff = 0
    parse_errors = 0
    printed_header = False

    # Stream file details as they arrive
    for diff in diffs:
        # Categorize and count
        if diff.parse_error:
            parse_errors += 1
            should_print = True
        elif not diff.allowed and not diff.not_allowed:
            identical += 1
            should_print = False
        elif diff.not_allowed:
            has_real_diff += 1
            should_print = True
        else:
            allowed_only += 1
            should_print = verbose

        # Print if needed
        if should_print:
            if not printed_header:
                print("FILE DETAILS:")
                printed_header = True
            print_file_diff(diff, verbose)

    # Print files only in old/new
    if categories.only_in_old:
        print("\nFILES ONLY IN OLD:")
        for f in sorted(categories.only_in_old):
            print(f"  - {f}")

    if categories.only_in_new:
        print("\nFILES ONLY IN NEW:")
        for f in sorted(categories.only_in_new):
            print(f"  + {f}")

    # Print summary at the end
    print("\n" + "=" * 60)
    print("COMPARISON SUMMARY")
    print("=" * 60)
    print(f"Files only in old: {len(categories.only_in_old)}")
    print(f"Files only in new: {len(categories.only_in_new)}")
    print(f"Files in both:     {len(categories.in_both)}")
    print("-" * 40)
    print(f"  Identical:       {identical}")
    print(f"  Allowed diffs:   {allowed_only}")
    print(f"  Real diffs:      {has_real_diff}")
    print(f"  Parse errors:    {parse_errors}")
    print("=" * 60)

    return Summary(
        only_in_old=len(categories.only_in_old),
        only_in_new=len(categories.only_in_new),
        identical=identical,
        allowed_only=allowed_only,
        has_real_diff=has_real_diff,
        parse_errors=parse_errors,
    )


# =============================================================================
# MAIN
# =============================================================================


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Compare two directories of HTML documentation output"
    )
    parser.add_argument("old_dir", type=Path, help="Path to old/original documentation")
    parser.add_argument("new_dir", type=Path, help="Path to new documentation")
    parser.add_argument(
        "-v", "--verbose", action="store_true",
        help="Show details including allowed differences"
    )

    args = parser.parse_args()

    old_dir: Path = args.old_dir.resolve()
    new_dir: Path = args.new_dir.resolve()

    if not old_dir.is_dir():
        print(f"Error: {old_dir} is not a directory", file=sys.stderr)
        return 1

    if not new_dir.is_dir():
        print(f"Error: {new_dir} is not a directory", file=sys.stderr)
        return 1

    # Categorize files
    categories = categorize_files(old_dir, new_dir)

    # Collect valid link targets from both directories
    print("Collecting link targets...", file=sys.stderr)
    old_files = find_html_files(old_dir)
    new_files = find_html_files(new_dir)
    old_targets = collect_link_targets(old_dir, old_files)
    new_targets = collect_link_targets(new_dir, new_files)
    print(f"  Old: {len(old_files)} files, {sum(len(ids) for ids in old_targets.ids.values())} IDs", file=sys.stderr)
    print(f"  New: {len(new_files)} files, {sum(len(ids) for ids in new_targets.ids.values())} IDs", file=sys.stderr)

    # Diff files and stream results
    diffs = diff_all_files(old_dir, new_dir, categories.in_both, old_targets, new_targets)

    # Print results (streams as diffs arrive, summary at end)
    summary = print_results(categories, diffs, args.verbose)

    # Return exit code
    if summary.has_real_diff > 0 or summary.parse_errors > 0:
        return 1
    if summary.only_in_old > 0 or summary.only_in_new > 0:
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
