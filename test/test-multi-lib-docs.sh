#!/usr/bin/env bash
#
# Regression test: verify that building docs for multiple libraries in one
# `lake build` produces HTML for all of them, and that an incremental build
# of a third library doesn't remove the first two.
#
# Usage: run from the doc-gen4 repo root (or pass it as $1).
#   ./test/test-multi-lib-docs.sh
#   ./test/test-multi-lib-docs.sh /path/to/doc-gen4

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

lean_lib LibA
lean_lib LibB
lean_lib LibC
EOF

cat > "$TEST_DIR/LibA.lean" << 'EOF'
/-- A greeting from LibA -/
def libAGreeting := "hello from A"
EOF

cat > "$TEST_DIR/LibB.lean" << 'EOF'
/-- A greeting from LibB -/
def libBGreeting := "hello from B"
EOF

cat > "$TEST_DIR/LibC.lean" << 'EOF'
/-- A greeting from LibC -/
def libCGreeting := "hello from C"
EOF

export LEAN_ABORT_ON_PANIC=1
export DOCGEN_SRC=file
DOC_DIR="$TEST_DIR/.lake/build/doc"

check_html() {
  local fail=0
  for mod in "$@"; do
    if [ ! -f "$DOC_DIR/$mod.html" ]; then
      echo "FAIL: $mod.html was not generated"
      fail=1
    else
      echo "OK: $mod.html exists"
    fi
  done
  if [ "$fail" -eq 1 ]; then
    echo "Listing $DOC_DIR/:"
    find "$DOC_DIR" -name '*.html' | sort
    exit 1
  fi
}

# --- Phase 1: build LibA and LibB concurrently ---

echo "=== Building LibA:docs and LibB:docs ==="
(cd "$TEST_DIR" && lake build LibA:docs LibB:docs)
check_html LibA LibB

# --- Phase 2: add LibC incrementally, verify A and B survive ---

echo "=== Building LibC:docs incrementally ==="
(cd "$TEST_DIR" && lake build LibC:docs)
check_html LibA LibB LibC

echo "SUCCESS: All three libraries have HTML documentation"
