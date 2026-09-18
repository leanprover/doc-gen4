#!/usr/bin/env bash
#
# Regression test: verify that building docs for multiple libraries in one
# `lake build` produces HTML for all of them, that an incremental build of a
# third library doesn't remove the first two, and that a link resolves only to
# a page that exists after the run.
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
lean_lib LibD
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
import LibD

/-- A greeting from LibC. See also `libAGreeting`, `libDThm` and `libDGone`. -/
def libCGreeting := "hello from C"
EOF

mkdir -p "$TEST_DIR/LibD"
cat > "$TEST_DIR/LibD/Old.lean" << 'EOF'
/-- A theorem of LibD. Phase 3 moves it to `LibD.New`. -/
def libDThm := "theorem of D"

/-- A declaration of LibD. Phase 3 removes it. -/
def libDGone := "gone from D"
EOF

cat > "$TEST_DIR/LibD.lean" << 'EOF'
import LibD.Old
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
check_html LibA LibB LibC LibD LibD/Old

# The docstring of LibC names a declaration of LibA. LibC does not import LibA, but the page of
# LibA is in the output directory, so the name links to it.
if grep -q 'LibA.html#libAGreeting' "$DOC_DIR/LibC.html"; then
  echo "OK: LibC.html links to libAGreeting on the page of LibA"
else
  echo "FAIL: LibC.html does not link to libAGreeting"
  exit 1
fi
if grep -q 'LibD/Old.html#libDThm' "$DOC_DIR/LibC.html"; then
  echo "OK: LibC.html links to libDThm on the page of LibD.Old"
else
  echo "FAIL: LibC.html does not link to libDThm"
  exit 1
fi

# --- Phase 3: a declaration moves to another module, and its old module disappears ---

echo "=== LibD moves libDThm to LibD.New and deletes LibD.Old ==="
cat > "$TEST_DIR/LibD/New.lean" << 'EOF'
/-- A theorem of LibD, moved here from `LibD.Old`. -/
def libDThm := "theorem of D"
EOF
rm "$TEST_DIR/LibD/Old.lean"
printf 'import LibD.New\n' > "$TEST_DIR/LibD.lean"
# No analysis runs for LibD.Old, so its rows stay in the database. A build that does not keep the
# pages between runs, such as a CI job that caches the database, has no page for it either.
rm "$DOC_DIR/LibD/Old.html"
# The HTML phase does not run again on its own. The markers of the doc steps are empty files, so
# their Lake trace does not change when the database changes, and Lake keeps the `docs` step up to
# date. Removing the markers forces the phase to run.
# TODO(#418): drop this once the doc markers carry their dependency trace.
rm "$TEST_DIR/.lake/build/doc-data/"*.docs_built

BUILD_LOG="$TEST_DIR/build-libd-move.log"
(cd "$TEST_DIR" && lake build LibC:docs 2>&1 | tee "$BUILD_LOG")
grep -q 'LibD.New:docInfo' "$BUILD_LOG" || { echo "FAIL: Lake did not analyze LibD.New"; exit 1; }
check_html LibC LibD/New
if grep -q 'LibD/New.html#libDThm' "$DOC_DIR/LibC.html"; then
  echo "OK: libDThm links to its new module"
else
  echo "FAIL: LibC.html does not link libDThm to LibD/New.html"
  exit 1
fi
if grep -q 'LibD/Old.html' "$DOC_DIR/LibC.html"; then
  echo "FAIL: LibC.html links to LibD/Old.html, which does not exist"
  exit 1
else
  echo "OK: no link points to the removed module"
fi

echo "SUCCESS: All three libraries have HTML documentation"
