#!/usr/bin/env python3
"""
Compare HTML documentation output between traditional and DB-generated docs.

Usage:
    python3 scripts/compare_docs.py [traditional_dir] [db_generated_dir]

Defaults:
    traditional_dir: .lake/build/doc
    db_generated_dir: .lake/build/doc-from-db/doc
"""

import argparse
import subprocess
import sys
import tempfile
import difflib
from pathlib import Path
from typing import Optional
from dataclasses import dataclass, field


@dataclass
class ComparisonResult:
    """Result of comparing two directories."""
    only_in_traditional: list[str] = field(default_factory=list)
    only_in_db: list[str] = field(default_factory=list)
    identical: list[str] = field(default_factory=list)
    different: list[tuple[str, str]] = field(default_factory=list)  # (filename, diff_summary)
    errors: list[tuple[str, str]] = field(default_factory=list)  # (filename, error)


def get_all_html_files(directory: Path) -> set[str]:
    """Get all HTML files relative to the directory."""
    files = set()
    for f in directory.rglob("*.html"):
        files.add(str(f.relative_to(directory)))
    return files


def build_anchor_index(directory: Path, verbose: bool = False) -> set[str]:
    """
    Build an index of all valid link targets (file#anchor) in the documentation.

    Returns a set of strings like "path/to/file.html#anchor" for anchor links,
    and "path/to/file.html" for file-only links.
    """
    import re

    valid_targets = set()
    html_files = list(directory.rglob("*.html"))

    if verbose:
        print(f"Building anchor index from {len(html_files)} files...", file=sys.stderr)

    for i, html_file in enumerate(html_files):
        if verbose and i % 100 == 0:
            print(f"\rIndexing {i}/{len(html_files)}...", end="", file=sys.stderr)

        rel_path = str(html_file.relative_to(directory))
        # The file itself is a valid target
        valid_targets.add(rel_path)

        try:
            content = html_file.read_text(encoding='utf-8')
            # Find all id="..." attributes
            for match in re.finditer(r'\bid=["\']([^"\']+)["\']', content):
                anchor = match.group(1)
                valid_targets.add(f"{rel_path}#{anchor}")
            # Also find name="..." attributes (older style)
            for match in re.finditer(r'\bname=["\']([^"\']+)["\']', content):
                anchor = match.group(1)
                valid_targets.add(f"{rel_path}#{anchor}")
        except Exception:
            pass  # Skip files we can't read

    if verbose:
        print(f"\rIndexed {len(valid_targets)} targets from {len(html_files)} files", file=sys.stderr)

    return valid_targets


def tidy_html(html_content: str) -> Optional[str]:
    """
    Normalize HTML using tidy.
    Returns None if tidy fails.
    """
    try:
        result = subprocess.run(
            [
                "tidy",
                "-q",  # quiet
                "-i",  # indent
                "--wrap", "0",  # no line wrapping
                "--show-warnings", "no",
                "--show-info", "no",
                "-utf8",
            ],
            input=html_content,
            capture_output=True,
            text=True,
            timeout=30,
        )
        # tidy returns 0 for success, 1 for warnings, 2 for errors
        if result.returncode <= 1:
            return result.stdout
        return None
    except FileNotFoundError:
        print("Error: 'tidy' command not found. Please install html-tidy.", file=sys.stderr)
        print("  macOS: brew install tidy-html5", file=sys.stderr)
        print("  Ubuntu: apt install tidy", file=sys.stderr)
        sys.exit(1)
    except subprocess.TimeoutExpired:
        return None


def resolve_link_target(href: str, rel_file_path: str) -> Optional[str]:
    """
    Resolve a relative href to a normalized path relative to base_dir.
    Returns None for external links.
    """
    from urllib.parse import unquote

    # Skip external links (including special protocols)
    # Note: lean-manual:// may appear with relative prefix like .././lean-manual://
    if href.startswith('http://') or href.startswith('https://') or href.startswith('vscode://'):
        return None
    if 'lean-manual://' in href:
        return None

    href = unquote(href)

    # Parse the href - may have anchor
    if '#' in href:
        file_part, anchor = href.split('#', 1)
    else:
        file_part = href
        anchor = None

    # Special case: #top is always valid
    if not file_part and anchor == 'top':
        return None  # Don't need to validate

    # Resolve relative path
    if file_part:
        current_dir = Path(rel_file_path).parent
        target_rel = str((current_dir / file_part).as_posix())
        # Normalize path
        parts = []
        for part in target_rel.split('/'):
            if part == '..':
                if parts:
                    parts.pop()
            elif part and part != '.':
                parts.append(part)
        target_rel = '/'.join(parts)
    else:
        target_rel = rel_file_path

    if anchor:
        return f"{target_rel}#{anchor}"
    return target_rel


def get_internal_link_targets(content: str, base_dir: Path, file_path: Path) -> set[str]:
    """
    Extract all internal link targets from the content.
    Returns a set of resolved target paths (file.html#anchor format).
    """
    import re

    rel_file_path = str(file_path.relative_to(base_dir))
    targets = set()

    for match in re.finditer(r'<a\s+href="([^"]*)"[^>]*>', content):
        href = match.group(1)
        target = resolve_link_target(href, rel_file_path)
        if target is not None:
            targets.add(target)

    return targets


def find_broken_links(content: str, base_dir: Path, file_path: Path,
                      valid_targets: set[str]) -> list[str]:
    """
    Find all broken internal links in the content.
    Returns a list of broken href values.
    """
    import re

    rel_file_path = str(file_path.relative_to(base_dir))
    broken = []

    for match in re.finditer(r'<a\s+href="([^"]*)"[^>]*>', content):
        href = match.group(1)
        target = resolve_link_target(href, rel_file_path)
        if target is None:
            continue  # External or special link

        # Check if target exists
        if target not in valid_targets:
            # Also check file without anchor
            file_only = target.split('#')[0] if '#' in target else target
            if file_only not in valid_targets:
                broken.append(href)

    return broken


def find_missing_links(trad_content: str, db_content: str,
                       trad_base: Path, db_base: Path,
                       trad_file: Path, db_file: Path,
                       trad_valid_targets: set[str]) -> list[str]:
    """
    Find links that exist in traditional but are missing in DB.
    Only considers valid links in traditional (not broken ones).
    Returns list of missing target paths.
    """
    trad_targets = get_internal_link_targets(trad_content, trad_base, trad_file)
    db_targets = get_internal_link_targets(db_content, db_base, db_file)

    # Filter to only valid traditional links
    valid_trad_targets = {t for t in trad_targets if t in trad_valid_targets}

    # Find what's in traditional but not in DB
    missing = valid_trad_targets - db_targets
    return sorted(missing)


def get_link_pairs(content: str, base_dir: Path, file_path: Path) -> set[tuple[str, str]]:
    """
    Extract all (resolved_href, normalized_plain_text) pairs from content.
    Used to verify link text/boundaries match between versions.
    """
    import re

    rel_file_path = str(file_path.relative_to(base_dir))
    pairs = set()

    # Match <a ...>...</a>, then extract href from attributes
    for match in re.finditer(r'<a\s+([^>]*)>(.*?)</a>', content, re.DOTALL):
        attrs = match.group(1)
        inner_html = match.group(2)

        # Extract href from attributes
        href_match = re.search(r'href="([^"]*)"', attrs)
        if not href_match:
            continue
        href = href_match.group(1)

        # Skip external links
        resolved = resolve_link_target(href, rel_file_path)
        if resolved is None:
            continue

        # Normalize inner text: strip HTML tags, normalize whitespace
        plain_text = re.sub(r'<[^>]+>', '', inner_html)
        plain_text = ' '.join(plain_text.split())  # normalize whitespace

        pairs.add((resolved, plain_text))

    return pairs


def find_link_text_mismatches(trad_content: str, db_content: str,
                               trad_base: Path, db_base: Path,
                               trad_file: Path, db_file: Path,
                               trad_valid_targets: set[str]) -> list[str]:
    """
    Find links where traditional and DB have the same href but different link text.
    This catches link boundary changes like:
      Traditional: See <a href="x">Foo</a> Bar
      DB:          See <a href="x">Foo Bar</a>

    Only considers valid links in traditional (not broken ones).
    Returns list of mismatch descriptions.
    """
    trad_pairs = get_link_pairs(trad_content, trad_base, trad_file)
    db_pairs = get_link_pairs(db_content, db_base, db_file)

    # Filter to valid traditional links
    valid_trad_pairs = {(h, t) for h, t in trad_pairs if h in trad_valid_targets}

    mismatches = []
    for href, text in sorted(valid_trad_pairs):
        if (href, text) not in db_pairs:
            # Traditional has a link that's not exactly matched in DB
            # Check if DB has any link to the same target (with different text)
            db_texts = sorted(set(t for h, t in db_pairs if h == href))
            if db_texts:
                # DB links to same target but with different text - boundary changed
                mismatches.append(f"{href}: trad='{text}', db='{db_texts[0]}'")
            # If DB has no link to this target at all, that's caught by missing_links check

    return mismatches


def normalize_html(content: str, base_dir: Optional[Path] = None, file_path: Optional[Path] = None,
                   valid_targets: Optional[set[str]] = None, strip_all_internal_links: bool = False) -> str:
    """
    Apply additional normalizations beyond tidy.
    This is where we handle whitelisted differences.

    Args:
        content: The HTML content to normalize
        base_dir: The base directory of the documentation (for checking link targets)
        file_path: The path of the current file (for resolving relative links)
        valid_targets: Pre-built set of valid link targets (from build_anchor_index)
        strip_all_internal_links: If True, convert ALL internal links to plain text.
            Used to ignore link presence differences between traditional and DB.
    """
    import re

    # Whitelist 1: Deduplicate import list items (only within <div class="imports">)
    # Both versions are correct - traditional may have duplicates, DB deduplicates
    def dedupe_list_items(match: re.Match) -> str:
        """Remove duplicate consecutive list items within a <ul> block."""
        ul_content = match.group(0)
        # Find all <li>...</li> items
        li_pattern = r'<li[^>]*>.*?</li>'
        items = re.findall(li_pattern, ul_content, re.DOTALL)
        # Remove consecutive duplicates
        deduped = []
        for item in items:
            if not deduped or item != deduped[-1]:
                deduped.append(item)
        # Reconstruct - preserve the ul tags
        ul_start = ul_content[:ul_content.find('>') + 1]
        ul_end = '</ul>'
        return ul_start + '\n'.join(deduped) + ul_end

    def dedupe_imports_section(match: re.Match) -> str:
        """Apply deduplication only within imports div."""
        imports_content = match.group(0)
        # Apply deduplication to <ul> blocks within this imports section
        return re.sub(r'<ul[^>]*>.*?</ul>', dedupe_list_items, imports_content, flags=re.DOTALL)

    # Apply deduplication only within <div class="imports"> sections
    content = re.sub(r'<div\s+class="imports"[^>]*>.*?</div>\s*</nav>', dedupe_imports_section, content, flags=re.DOTALL)

    # Whitelist 2: Strip all internal links to ignore link presence differences
    # DB may have more links than traditional (it has complete knowledge)
    # By stripping all internal links from both, we compare only content
    if strip_all_internal_links:
        # Signature context classes where <a> should become <span class="fn">
        signature_classes = ['decl_header', 'decl_args', 'signature', 'struct_field', 'constructor']

        def strip_links_in_section(section_html: str, in_signature: bool) -> str:
            """Strip internal links, using fn wrapper only in signature contexts.

            In signature contexts:
              - <a href="internal">X</a> → <span class="fn">X</span>
              - <span class="fn">X</span> → unchanged

            In non-signature contexts (docstrings, equations, etc.):
              - <a href="internal">X</a> → X
              - <span class="fn">X</span> → X (also strip these for equivalence)
            """
            def strip_link(match: re.Match) -> str:
                href = match.group(1)
                inner_html = match.group(2)

                # Keep external links
                if href.startswith('http://') or href.startswith('https://') or href.startswith('vscode://'):
                    return match.group(0)

                # In signature context: wrap in <span class="fn">
                # Elsewhere: just keep the inner content
                if in_signature:
                    return f'<span class="fn">{inner_html}</span>'
                else:
                    return inner_html

            link_pattern = r'<a\s+href="([^"]*)"[^>]*>(.*?)</a>'
            result = re.sub(link_pattern, strip_link, section_html, flags=re.DOTALL)

            # In non-signature contexts, also strip <span class="fn"> wrappers
            # This makes <a href>X</a> equivalent to <span class="fn">X</span>
            # Both represent "a styled const name" - just different representations
            if not in_signature:
                # Repeatedly strip outermost <span class="fn"> until none remain
                # This handles nested spans like <span class="fn"><span class="fn">X</span></span>
                fn_pattern = r'<span class="fn">(.*?)</span>'
                prev_result = None
                while prev_result != result:
                    prev_result = result
                    result = re.sub(fn_pattern, r'\1', result, flags=re.DOTALL)

            return result

        # Build pattern to match signature context divs
        # Match <div class="...decl_header...">...</div> (including nested divs)
        sig_class_pattern = '|'.join(re.escape(c) for c in signature_classes)

        # Process content by finding signature regions and non-signature regions
        # We need to handle nested divs properly, so use a different approach:
        # Find all positions of signature div starts and ends

        result_parts = []
        pos = 0

        # Pattern to find opening div with signature class
        open_pattern = re.compile(rf'<div\s+[^>]*class="[^"]*\b({sig_class_pattern})\b[^"]*"[^>]*>', re.IGNORECASE)

        while pos < len(content):
            # Find next signature div
            match = open_pattern.search(content, pos)
            if not match:
                # No more signature divs, process rest as non-signature
                result_parts.append(strip_links_in_section(content[pos:], in_signature=False))
                break

            # Process non-signature content before this div
            if match.start() > pos:
                result_parts.append(strip_links_in_section(content[pos:match.start()], in_signature=False))

            # Find the matching closing </div> (handling nesting)
            div_start = match.start()
            div_content_start = match.end()
            depth = 1
            search_pos = div_content_start

            while depth > 0 and search_pos < len(content):
                next_open = content.find('<div', search_pos)
                next_close = content.find('</div>', search_pos)

                if next_close == -1:
                    # No closing tag found, take rest of content
                    search_pos = len(content)
                    break

                if next_open != -1 and next_open < next_close:
                    # Found nested opening div
                    depth += 1
                    search_pos = next_open + 4
                else:
                    # Found closing div
                    depth -= 1
                    if depth == 0:
                        div_end = next_close + 6  # len('</div>')
                    search_pos = next_close + 6

            # Extract and process the signature div
            sig_div = content[div_start:div_end] if depth == 0 else content[div_start:]
            result_parts.append(strip_links_in_section(sig_div, in_signature=True))
            pos = div_end if depth == 0 else len(content)

        content = ''.join(result_parts)

    return content


def compare_html_files(file1: Path, file2: Path, base_dir1: Path, base_dir2: Path, rel_path: str,
                       valid_targets1: Optional[set[str]] = None,
                       valid_targets2: Optional[set[str]] = None) -> tuple[bool, str]:
    """
    Compare two HTML files.
    Returns (are_equal, diff_summary).

    Args:
        file1: Path to first file (traditional)
        file2: Path to second file (DB-generated)
        base_dir1: Base directory of traditional docs
        base_dir2: Base directory of DB-generated docs
        rel_path: Relative path of the file within the docs
        valid_targets1: Pre-built anchor index for traditional docs
        valid_targets2: Pre-built anchor index for DB-generated docs
    """
    try:
        content1 = file1.read_text(encoding='utf-8')
        content2 = file2.read_text(encoding='utf-8')
    except Exception as e:
        return False, f"Error reading files: {e}"

    # First try exact match
    if content1 == content2:
        return True, ""

    # Normalize with tidy
    tidy1 = tidy_html(content1)
    tidy2 = tidy_html(content2)

    if tidy1 is None or tidy2 is None:
        return False, "tidy failed to process one or both files"

    rel_path_obj = Path(rel_path)

    # Step 1: Check for broken links that are unique to DB version
    # If traditional also has the same broken links, that's not a DB-specific issue
    broken_in_db = set(find_broken_links(tidy2, base_dir2, base_dir2 / rel_path_obj, valid_targets2))
    broken_in_trad = set(find_broken_links(tidy1, base_dir1, base_dir1 / rel_path_obj, valid_targets1))
    db_only_broken = broken_in_db - broken_in_trad
    if db_only_broken:
        return False, f"DB has {len(db_only_broken)} broken link(s): {', '.join(sorted(db_only_broken)[:3])}"

    # Step 2: Check that DB has all valid links from traditional
    missing_links = find_missing_links(
        tidy1, tidy2,
        base_dir1, base_dir2,
        base_dir1 / rel_path_obj, base_dir2 / rel_path_obj,
        valid_targets1
    )
    if missing_links:
        return False, f"DB missing {len(missing_links)} link(s): {', '.join(missing_links[:3])}"

    # Step 2.5: Check that link text matches (catches boundary changes)
    text_mismatches = find_link_text_mismatches(
        tidy1, tidy2,
        base_dir1, base_dir2,
        base_dir1 / rel_path_obj, base_dir2 / rel_path_obj,
        valid_targets1
    )
    if text_mismatches:
        return False, f"Link text mismatch: {text_mismatches[0]}"

    # Step 3: Normalize both by stripping all internal links
    # This makes comparison ignore link presence differences (DB having more links is fine)
    norm1 = normalize_html(tidy1, base_dir1, base_dir1 / rel_path_obj, valid_targets1, strip_all_internal_links=True)
    norm2 = normalize_html(tidy2, base_dir2, base_dir2 / rel_path_obj, valid_targets2, strip_all_internal_links=True)

    if norm1 == norm2:
        return True, ""

    # Generate a summary of differences
    diff = list(difflib.unified_diff(
        norm1.splitlines(keepends=True),
        norm2.splitlines(keepends=True),
        fromfile='traditional',
        tofile='db-generated',
        n=1,  # context lines
    ))

    if not diff:
        return True, ""

    # Count changed lines
    additions = sum(1 for line in diff if line.startswith('+') and not line.startswith('+++'))
    deletions = sum(1 for line in diff if line.startswith('-') and not line.startswith('---'))

    return False, f"+{additions}/-{deletions} lines"


def compare_directories(traditional_dir: Path, db_dir: Path, verbose: bool = False) -> ComparisonResult:
    """Compare all HTML files in two directories."""
    result = ComparisonResult()

    traditional_files = get_all_html_files(traditional_dir)
    db_files = get_all_html_files(db_dir)

    # Find files only in one directory
    result.only_in_traditional = sorted(traditional_files - db_files)
    result.only_in_db = sorted(db_files - traditional_files)

    # Build anchor indices for both directories (for link validation)
    if verbose:
        print("Building anchor index for traditional docs...", file=sys.stderr)
    valid_targets1 = build_anchor_index(traditional_dir, verbose)
    if verbose:
        print("Building anchor index for DB-generated docs...", file=sys.stderr)
    valid_targets2 = build_anchor_index(db_dir, verbose)

    # Compare common files
    common_files = sorted(traditional_files & db_files)
    total = len(common_files)

    for i, rel_path in enumerate(common_files, 1):
        if verbose:
            print(f"\rComparing {i}/{total}: {rel_path[:60]:<60}", end="", file=sys.stderr)

        file1 = traditional_dir / rel_path
        file2 = db_dir / rel_path

        try:
            are_equal, diff_summary = compare_html_files(
                file1, file2, traditional_dir, db_dir, rel_path,
                valid_targets1, valid_targets2
            )
            if are_equal:
                result.identical.append(rel_path)
            else:
                result.different.append((rel_path, diff_summary))
        except Exception as e:
            result.errors.append((rel_path, str(e)))

    if verbose:
        print("\r" + " " * 80 + "\r", end="", file=sys.stderr)

    return result


def print_report(result: ComparisonResult, show_identical: bool = False):
    """Print a comparison report."""
    print("=" * 70)
    print("HTML Documentation Comparison Report")
    print("=" * 70)
    print()

    # Summary
    total_files = (
        len(result.identical) +
        len(result.different) +
        len(result.only_in_traditional) +
        len(result.only_in_db)
    )
    print(f"Total files examined: {total_files}")
    print(f"  Identical:              {len(result.identical)}")
    print(f"  Different:              {len(result.different)}")
    print(f"  Only in traditional:    {len(result.only_in_traditional)}")
    print(f"  Only in DB-generated:   {len(result.only_in_db)}")
    print(f"  Errors:                 {len(result.errors)}")
    print()

    # Files only in traditional
    if result.only_in_traditional:
        print("-" * 70)
        print(f"Files only in traditional ({len(result.only_in_traditional)}):")
        for f in result.only_in_traditional[:20]:
            print(f"  {f}")
        if len(result.only_in_traditional) > 20:
            print(f"  ... and {len(result.only_in_traditional) - 20} more")
        print()

    # Files only in DB-generated
    if result.only_in_db:
        print("-" * 70)
        print(f"Files only in DB-generated ({len(result.only_in_db)}):")
        for f in result.only_in_db[:20]:
            print(f"  {f}")
        if len(result.only_in_db) > 20:
            print(f"  ... and {len(result.only_in_db) - 20} more")
        print()

    # Different files
    if result.different:
        print("-" * 70)
        print(f"Different files ({len(result.different)}):")
        for f, summary in result.different[:50]:
            print(f"  {f}: {summary}")
        if len(result.different) > 50:
            print(f"  ... and {len(result.different) - 50} more")
        print()

    # Errors
    if result.errors:
        print("-" * 70)
        print(f"Errors ({len(result.errors)}):")
        for f, err in result.errors[:20]:
            print(f"  {f}: {err}")
        if len(result.errors) > 20:
            print(f"  ... and {len(result.errors) - 20} more")
        print()

    # Identical files (if requested)
    if show_identical and result.identical:
        print("-" * 70)
        print(f"Identical files ({len(result.identical)}):")
        for f in result.identical[:50]:
            print(f"  {f}")
        if len(result.identical) > 50:
            print(f"  ... and {len(result.identical) - 50} more")
        print()

    # Final verdict
    print("=" * 70)
    if not result.different and not result.only_in_traditional and not result.only_in_db and not result.errors:
        print("PASS: All files are identical!")
        return 0
    else:
        print("FAIL: Differences found")
        return 1


def show_diff(traditional_dir: Path, db_dir: Path, rel_path: str):
    """Show detailed diff for a specific file."""
    file1 = traditional_dir / rel_path
    file2 = db_dir / rel_path

    if not file1.exists():
        print(f"File not found in traditional: {rel_path}")
        return
    if not file2.exists():
        print(f"File not found in DB-generated: {rel_path}")
        return

    # Build anchor indices for link validation
    print("Building anchor indices...", file=sys.stderr)
    valid_targets1 = build_anchor_index(traditional_dir)
    valid_targets2 = build_anchor_index(db_dir)

    content1 = file1.read_text(encoding='utf-8')
    content2 = file2.read_text(encoding='utf-8')

    tidy1 = tidy_html(content1) or content1
    tidy2 = tidy_html(content2) or content2

    rel_path_obj = Path(rel_path)

    # Check for broken links in DB version
    broken_links = find_broken_links(tidy2, db_dir, db_dir / rel_path_obj, valid_targets2)
    if broken_links:
        print(f"WARNING: DB has {len(broken_links)} broken link(s):", file=sys.stderr)
        for link in broken_links[:10]:
            print(f"  {link}", file=sys.stderr)

    # Check for link text mismatches
    text_mismatches = find_link_text_mismatches(
        tidy1, tidy2,
        traditional_dir, db_dir,
        traditional_dir / rel_path_obj, db_dir / rel_path_obj,
        valid_targets1
    )
    if text_mismatches:
        print(f"WARNING: {len(text_mismatches)} link text mismatch(es):", file=sys.stderr)
        for mismatch in text_mismatches[:10]:
            print(f"  {mismatch}", file=sys.stderr)

    # Normalize both by stripping internal links
    norm1 = normalize_html(tidy1, traditional_dir, traditional_dir / rel_path_obj, valid_targets1, strip_all_internal_links=True)
    norm2 = normalize_html(tidy2, db_dir, db_dir / rel_path_obj, valid_targets2, strip_all_internal_links=True)

    diff = difflib.unified_diff(
        norm1.splitlines(keepends=True),
        norm2.splitlines(keepends=True),
        fromfile=f'traditional/{rel_path}',
        tofile=f'db-generated/{rel_path}',
        n=3,
    )

    for line in diff:
        print(line, end='')


def main():
    parser = argparse.ArgumentParser(
        description="Compare HTML documentation output between traditional and DB-generated docs."
    )
    parser.add_argument(
        "traditional_dir",
        nargs="?",
        default=".lake/build/doc",
        help="Directory containing traditional docs (default: .lake/build/doc)",
    )
    parser.add_argument(
        "db_dir",
        nargs="?",
        default=".lake/build/doc-from-db/doc",
        help="Directory containing DB-generated docs (default: .lake/build/doc-from-db/doc)",
    )
    parser.add_argument(
        "-v", "--verbose",
        action="store_true",
        help="Show progress during comparison",
    )
    parser.add_argument(
        "--show-identical",
        action="store_true",
        help="Also list identical files in the report",
    )
    parser.add_argument(
        "--diff",
        metavar="FILE",
        help="Show detailed diff for a specific file (relative path)",
    )

    args = parser.parse_args()

    traditional_dir = Path(args.traditional_dir)
    db_dir = Path(args.db_dir)

    if not traditional_dir.exists():
        print(f"Error: Traditional docs directory not found: {traditional_dir}", file=sys.stderr)
        sys.exit(1)

    if not db_dir.exists():
        print(f"Error: DB-generated docs directory not found: {db_dir}", file=sys.stderr)
        sys.exit(1)

    if args.diff:
        show_diff(traditional_dir, db_dir, args.diff)
        return

    result = compare_directories(traditional_dir, db_dir, verbose=args.verbose)
    exit_code = print_report(result, show_identical=args.show_identical)
    sys.exit(exit_code)


if __name__ == "__main__":
    main()
