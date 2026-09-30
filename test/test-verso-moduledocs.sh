#!/usr/bin/env bash
#
# Regression test: verify that Verso module docstrings are rendered to HTML in
# source order alongside declarations, and that Markdown module docstrings are
# also rendered.
#
# Usage: run from the doc-gen4 repo root (or pass it as $1).
#   ./test/test-verso-moduledocs.sh
#   ./test/test-verso-moduledocs.sh /path/to/doc-gen4

set -euo pipefail

DOCGEN4_DIR="$(cd "${1:-$(dirname "$0")/..}" && pwd)"
TEST_DIR="$(mktemp -d)"

cleanup() { rm -rf "$TEST_DIR"; }
trap cleanup EXIT

echo "doc-gen4: $DOCGEN4_DIR"
echo "test project: $TEST_DIR"

# --- Setup ---

cp "$DOCGEN4_DIR/lean-toolchain" "$TEST_DIR/"

cat > "$TEST_DIR/lakefile.lean" << EOF
import Lake
open Lake DSL

package test

require «doc-gen4» from "$DOCGEN4_DIR"

lean_lib VersoMod
lean_lib MarkdownMod
EOF

cat > "$TEST_DIR/VersoMod.lean" << 'EOF'
set_option doc.verso true

/-!
# Verso Section

Some _emphasized_ text.

## Verso Subsection

* verso item one
* verso item two
-/

/-- The first definition. -/
def first : Nat := 1

/-!
Continuation text after the first definition.

# Second Verso Section
-/

/-- The second definition. -/
def second : Nat := 2
EOF

cat > "$TEST_DIR/MarkdownMod.lean" << 'EOF'
/-!
# Markdown Section

Some *emphasized* Markdown text.
-/

/-- A definition. -/
def markdownDef : Nat := 1
EOF

export LEAN_ABORT_ON_PANIC=1
export DOCGEN_SRC=file
DOC_DIR="$TEST_DIR/.lake/build/doc"

(cd "$TEST_DIR" && lake build VersoMod:docs MarkdownMod:docs)

FAIL=0

# Prints the byte offset of the first occurrence of $2 in file $1, or nothing.
offset_of() {
  { grep -boF -- "$2" "$1" || true; } | head -n 1 | cut -d: -f1
}

# Checks that each of the given strings occurs in file $1, in the given order.
check_in_order() {
  local file="$1"
  shift
  local prev=-1
  local prevNeedle=""
  for needle in "$@"; do
    local off
    off="$(offset_of "$file" "$needle")"
    if [ -z "$off" ]; then
      echo "FAIL: $(basename "$file") does not contain: $needle"
      FAIL=1
    elif [ "$off" -le "$prev" ]; then
      echo "FAIL: $(basename "$file"): '$needle' appears before '$prevNeedle'"
      FAIL=1
    else
      echo "OK: $(basename "$file") contains: $needle"
      prev="$off"
      prevNeedle="$needle"
    fi
  done
}

check_in_order "$DOC_DIR/VersoMod.html" \
  'id="Verso-Section"' \
  '<em>emphasized</em>' \
  'id="Verso-Subsection"' \
  'verso item two' \
  'id="first"' \
  'Continuation text after the first definition.' \
  'id="Second-Verso-Section"' \
  'id="second"'

check_in_order "$DOC_DIR/MarkdownMod.html" \
  'id="Markdown-Section"' \
  '<em>emphasized</em>' \
  'id="markdownDef"'

if [ "$FAIL" -eq 1 ]; then
  exit 1
fi

echo "SUCCESS: Verso and Markdown module docstrings are rendered"
