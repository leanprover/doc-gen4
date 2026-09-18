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

mkdir -p "$TEST_DIR/LibA"
cat > "$TEST_DIR/LibA/Helper.lean" << 'EOF'
/-- A helper in LibA -/
def libAHelper := "helper from A"
EOF

cat > "$TEST_DIR/LibA.lean" << 'EOF'
import LibA.Helper

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

# --- Phase 4: prune the modules outside the closure of LibA and LibB, then build LibC again ---

PRUNE="lake exe doc-gen4 prune --build .lake/build"
DOC_DATA="$TEST_DIR/.lake/build/doc-data"
SEARCH_INDEX="$DOC_DIR/declarations/declaration-data.bmp"
# LibD.Old.doc is the marker of the module that Phase 3 removed. Nothing has deleted it yet.
ALL_MARKERS="LibA.doc LibA.Helper.doc LibB.doc LibC.doc LibD.doc LibD.Old.doc LibD.New.doc core-Init.doc"

# expect_markers present|absent MARKER...
expect_markers() {
  local state="$1"; shift
  for marker in "$@"; do
    if [ "$state" = present ] && [ ! -e "$DOC_DATA/$marker" ]; then
      echo "FAIL: $marker is missing"; exit 1
    fi
    if [ "$state" = absent ] && [ -e "$DOC_DATA/$marker" ]; then
      echo "FAIL: $marker is still present"; exit 1
    fi
  done
}

# The search index takes the declarations of a module that no run rendered from its
# `declaration-data-<module>.bmp` file, so the removed declaration is still searchable.
grep -aq 'libDGone' "$SEARCH_INDEX" || { echo "FAIL: the search index does not hold libDGone before the prune"; exit 1; }

echo "=== prune refuses an unknown root and changes nothing ==="
if (cd "$TEST_DIR" && $PRUNE api-docs.db LibA Nope); then
  echo "FAIL: prune accepted an unknown root"; exit 1
fi
expect_markers present $ALL_MARKERS
echo "OK: prune refused the unknown root"

echo "=== prune refuses an empty root list and changes nothing ==="
if (cd "$TEST_DIR" && $PRUNE api-docs.db); then
  echo "FAIL: prune accepted an empty root list"; exit 1
fi
expect_markers present $ALL_MARKERS
echo "OK: prune refused the empty root list"

echo "=== a dry run names LibC and LibD.Old and changes nothing ==="
DRY_LOG="$TEST_DIR/prune-dry.log"
(cd "$TEST_DIR" && $PRUNE --dryRun api-docs.db LibA LibB | tee "$DRY_LOG")
grep -q '^  LibC$' "$DRY_LOG" || { echo "FAIL: the dry run does not name LibC"; exit 1; }
grep -q '^  LibD.Old$' "$DRY_LOG" || { echo "FAIL: the dry run does not name LibD.Old"; exit 1; }
expect_markers present $ALL_MARKERS
echo "OK: the dry run named LibC and LibD.Old and kept every marker"

echo "=== prune removes LibC and LibD and keeps the closure of LibA and LibB ==="
(cd "$TEST_DIR" && $PRUNE api-docs.db LibA LibB)
expect_markers absent LibC.doc LibC.doc.trace LibC.doc.hash declaration-data-LibC.bmp backrefs-LibC.json \
  LibD.doc LibD.Old.doc LibD.New.doc declaration-data-LibD.Old.bmp
expect_markers present LibA.doc LibA.Helper.doc LibB.doc core-Init.doc declaration-data-LibA.bmp declaration-data-LibA.Helper.bmp
if [ -e "$DOC_DIR/LibC.html" ]; then echo "FAIL: LibC.html survived the prune"; exit 1; fi
if [ -e "$DOC_DIR/LibD/New.html" ]; then echo "FAIL: LibD/New.html survived the prune"; exit 1; fi
if compgen -G "$DOC_DATA/*.docs_built*" > /dev/null; then
  echo "FAIL: a .docs_built marker survived the prune"; ls "$DOC_DATA"; exit 1
fi
check_html LibA LibA/Helper LibB
echo "OK: the prune removed the files of LibC and LibD and kept the others, including the import LibA.Helper"

echo "=== a second prune finds nothing to remove ==="
(cd "$TEST_DIR" && $PRUNE api-docs.db LibA LibB | grep -q '^No modules') \
  || { echo "FAIL: the second prune did not report an empty result"; exit 1; }
echo "OK: the prune is idempotent"

echo "=== the kept modules still have their rows, and LibC is analyzed again ==="
# The prune deleted the .docs_built markers, so the build writes the pages again from the database.
rm "$DOC_DIR/LibA.html" "$DOC_DIR/LibA/Helper.html"
BUILD_LOG="$TEST_DIR/build-after-prune.log"
(cd "$TEST_DIR" && lake build LibA:docs LibC:docs 2>&1 | tee "$BUILD_LOG")
grep -q 'LibC:docInfo' "$BUILD_LOG" || { echo "FAIL: Lake did not analyze LibC again after the prune"; exit 1; }
if grep -qE 'LibA:docInfo|LibA.Helper:docInfo' "$BUILD_LOG"; then
  echo "FAIL: Lake analyzed LibA again, so the prune touched its rows or markers"; exit 1
fi
check_html LibA LibA/Helper LibB LibC LibD LibD/New
grep -q 'libAGreeting' "$DOC_DIR/LibA.html" || { echo "FAIL: LibA.html lost libAGreeting"; exit 1; }
grep -q 'libAHelper' "$DOC_DIR/LibA/Helper.html" || { echo "FAIL: LibA/Helper.html lost libAHelper"; exit 1; }
grep -q 'libCGreeting' "$DOC_DIR/LibC.html" || { echo "FAIL: LibC.html lost libCGreeting"; exit 1; }
if grep -aq 'libDGone' "$SEARCH_INDEX"; then
  echo "FAIL: the search index still holds libDGone after the prune"; exit 1
fi
echo "OK: the kept modules kept their rows, LibC came back, and the search index dropped libDGone"

echo "SUCCESS: All three libraries have HTML documentation"
