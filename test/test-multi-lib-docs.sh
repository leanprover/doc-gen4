#!/usr/bin/env bash
#
# Regression test for the `docs` facet. It verifies that:
#   * one `lake build` of several libraries produces HTML for all of them;
#   * an incremental build of a third library keeps the pages of the first two;
#   * a change in a module reaches the HTML, whether the module is a library
#     root or an import of one;
#   * a removed declaration and a changed docstring reach the HTML;
#   * a rebuild with no change leaves the build up to date;
#   * a change in one library leaves the docs of the other libraries up to date;
#   * building the docInfo facet again for unchanged modules leaves the HTML
#     up to date;
#   * documenting a project without its dependencies:
#     - concurrent and incremental builds retain every generated library;
#     - local-only builds omit dependency pages and link each dependency to its own docs site;
#     - incomplete external documentation mappings fail instead of producing broken links;
#     - a Lake build with local roots analyzes only the local modules, yet still links the rest;
#     - incremental builds with local roots forget removed modules and survive toggling the roots.
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
lean_lib DepA
lean_lib DepB
lean_lib Project
EOF

mkdir -p "$TEST_DIR/LibA"
cat > "$TEST_DIR/LibA.lean" << 'EOF'
import LibA.Basic

/-- A greeting from LibA -/
def libAGreeting := "hello from A"
EOF

cat > "$TEST_DIR/LibA/Basic.lean" << 'EOF'
/-- A greeting from LibA.Basic -/
def libABasicGreeting := "hello from A.Basic"
EOF

cat > "$TEST_DIR/LibB.lean" << 'EOF'
/-- A greeting from LibB -/
def libBGreeting := "hello from B"
EOF

cat > "$TEST_DIR/LibC.lean" << 'EOF'
/-- A greeting from LibC -/
def libCGreeting := "hello from C"
EOF

cat > "$TEST_DIR/DepA.lean" << 'EOF'
/-- A greeting from the first dependency -/
def depAGreeting := "hello from dependency A"
EOF

cat > "$TEST_DIR/DepB.lean" << 'EOF'
/-- A greeting from the second dependency -/
def depBGreeting := "hello from dependency B"
EOF

cat > "$TEST_DIR/Project.lean" << 'EOF'
import DepA
import DepB

/-- A declaration whose type refers to both dependencies. -/
theorem projectUsesDeps : depAGreeting = depAGreeting ∧ depBGreeting = depBGreeting := ⟨rfl, rfl⟩

/-- A declaration whose type uses the fallback documentation site. -/
def projectString : String := "project"
EOF

export LEAN_ABORT_ON_PANIC=1
export DOCGEN_SRC=file
DOC_DIR="$TEST_DIR/.lake/build/doc"
DOC_DATA_DIR="$TEST_DIR/.lake/build/doc-data"

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

check_up_to_date() {
  for lib in "$@"; do
    if (cd "$TEST_DIR" && lake build "$lib:docs" --no-build); then
      echo "OK: $lib:docs is up to date"
    else
      echo "FAIL: $lib:docs is out of date"
      exit 1
    fi
  done
}

check_no_html() {
  local doc_dir="$1"
  shift
  for mod in "$@"; do
    if [ -f "$doc_dir/$mod.html" ]; then
      echo "FAIL: $mod.html should not have been generated"
      exit 1
    else
      echo "OK: $mod.html was not generated"
    fi
  done
}

check_contains() {
  local file="$1"
  local expected="$2"
  if ! grep -Fq "$expected" "$file"; then
    echo "FAIL: $file does not contain: $expected"
    exit 1
  fi
  echo "OK: $file contains: $expected"
}

# --- Phase 1: build LibA and LibB concurrently ---

echo "=== Building LibA:docs and LibB:docs ==="
(cd "$TEST_DIR" && lake build LibA:docs LibB:docs)
check_html LibA LibB

# --- Phase 2: add LibC incrementally, verify A and B survive ---

echo "=== Building LibC:docs incrementally ==="
(cd "$TEST_DIR" && lake build LibC:docs)
check_html LibA LibB LibC

# --- Phase 3: modify LibA, ensure that the change shows up in the HTML ---

echo "=== Adding a declaration to LibA and building LibA:docs again ==="
cat >> "$TEST_DIR/LibA.lean" << 'EOF'

/-- A second greeting from LibA -/
def libAGreetingAgain := "hello again from A"
EOF
(cd "$TEST_DIR" && lake build LibA:docs)
if grep -q 'libAGreetingAgain' "$DOC_DIR/LibA.html"; then
  echo "OK: the page of LibA shows the new declaration"
else
  echo "FAIL: the page of LibA does not show libAGreetingAgain"
  exit 1
fi

# --- Phase 4: ensure that all three libraries are up to date now that LibA:docs is rebuilt ---

echo "=== Checking that no library needs a rebuild ==="
check_up_to_date LibA LibB LibC

# --- Phase 5: ensure that changes in non-root modules are reflected in HTML ---

echo "=== Adding a declaration to LibA/Basic.lean and building LibA:docs again ==="
cat >> "$TEST_DIR/LibA/Basic.lean" << 'EOF'

/-- A second greeting from LibA.Basic -/
def libABasicGreetingAgain := "hello again from A.Basic"
EOF
(cd "$TEST_DIR" && lake build LibA:docs)
if grep -q 'libABasicGreetingAgain' "$DOC_DIR/LibA/Basic.html"; then
  echo "OK: the page of LibA.Basic shows the new declaration"
else
  echo "FAIL: the page of LibA.Basic does not show libABasicGreetingAgain"
  exit 1
fi
check_html LibA LibB LibC

# --- Phase 6: remove a declaration and change a docstring, ensure that the HTML is updated ---

echo "=== Removing a declaration from LibA, changing a docstring, and building LibA:docs again ==="
cat > "$TEST_DIR/LibA.lean" << 'EOF'
import LibA.Basic

/-- A revised greeting from LibA -/
def libAGreeting := "hello from A"
EOF
(cd "$TEST_DIR" && lake build LibA:docs)
if grep -q 'libAGreetingAgain' "$DOC_DIR/LibA.html"; then
  echo "FAIL: the page of LibA still shows the removed libAGreetingAgain"
  exit 1
else
  echo "OK: the page of LibA omits the removed declaration"
fi
if grep -q 'A revised greeting from LibA' "$DOC_DIR/LibA.html"; then
  echo "OK: the page of LibA shows the new docstring"
else
  echo "FAIL: the page of LibA does not show the new docstring"
  exit 1
fi
if grep -q 'A greeting from LibA' "$DOC_DIR/LibA.html"; then
  echo "FAIL: the page of LibA still shows the old docstring"
  exit 1
else
  echo "OK: the page of LibA omits the old docstring"
fi

# --- Phase 7: build the docInfo facet again for unchanged modules, ensure that the HTML stays up to date ---

echo "=== Removing the docInfo markers of LibA and building LibA:docInfo again ==="
rm "$DOC_DATA_DIR/LibA.doc" "$DOC_DATA_DIR/LibA.Basic.doc"
(cd "$TEST_DIR" && lake build LibA:docInfo)
for marker in LibA.doc LibA.Basic.doc; do
  if [ ! -f "$DOC_DATA_DIR/$marker" ]; then
    echo "FAIL: $marker was not written again"
    exit 1
  fi
done
check_up_to_date LibA

# --- Phase 8: generate only local project docs with per-dependency URLs ---

echo "=== Building Project doc info ==="
(cd "$TEST_DIR" && lake build Project:docInfo)

INTERPROJECT_BUILD="$TEST_DIR/interproject-build"
INTERPROJECT_DOC_DIR="$INTERPROJECT_BUILD/doc"
DOCGEN4_BIN="$DOCGEN4_DIR/.lake/build/bin/doc-gen4"

echo "=== Generating local-only docs with per-dependency URLs ==="
env \
  DOCGEN_LOCAL_MODULE_ROOTS=Project \
  DOCGEN_DEPS_DOCS_URL=https://deps.example/fallback/ \
  DOCGEN_DEPS_DOCS_URLS='DepA=https://deps.example/a/,DepB=https://deps.example/b' \
  "$DOCGEN4_BIN" fromDb \
    --build "$INTERPROJECT_BUILD" \
    --manifest "$INTERPROJECT_BUILD/manifest.json" \
    "$TEST_DIR/.lake/build/api-docs.db" Project

check_html_file="$INTERPROJECT_DOC_DIR/Project.html"
if [ ! -f "$check_html_file" ]; then
  echo "FAIL: Project.html was not generated"
  exit 1
fi
echo "OK: Project.html exists"
check_no_html "$INTERPROJECT_DOC_DIR" DepA DepB Init
check_contains "$check_html_file" 'https://deps.example/a/find/?pattern=depAGreeting#doc'
check_contains "$check_html_file" 'https://deps.example/b/find/?pattern=depBGreeting#doc'
check_contains "$check_html_file" 'https://deps.example/a/DepA.html'
check_contains "$check_html_file" 'https://deps.example/b/DepB.html'
check_contains "$check_html_file" 'https://deps.example/fallback/find/?pattern=String#doc'

# --- Phase 9: reject incomplete URL mappings ---

echo "=== Checking incomplete dependency URL configuration ==="
INCOMPLETE_BUILD="$TEST_DIR/incomplete-build"
INCOMPLETE_LOG="$TEST_DIR/incomplete.log"
if env \
    -u DOCGEN_DEPS_DOCS_URL \
    DOCGEN_LOCAL_MODULE_ROOTS=Project \
    DOCGEN_DEPS_DOCS_URLS='DepA=https://deps.example/a,DepB=https://deps.example/b' \
    "$DOCGEN4_BIN" fromDb \
      --build "$INCOMPLETE_BUILD" \
      "$TEST_DIR/.lake/build/api-docs.db" Project >"$INCOMPLETE_LOG" 2>&1; then
  echo "FAIL: incomplete dependency URL configuration unexpectedly succeeded"
  exit 1
fi
check_contains "$INCOMPLETE_LOG" 'No dependency documentation URL configured for external module roots:'

# --- Phase 10: a Lake build with local roots analyzes only the local modules ---

echo "=== Building local-only docs through Lake ==="
LOCAL_DIR="$TEST_DIR/local-only"
mkdir -p "$LOCAL_DIR"
cp "$TEST_DIR/lean-toolchain" "$TEST_DIR"/*.lean "$LOCAL_DIR/"
(cd "$LOCAL_DIR" && env \
  DOCGEN_LOCAL_MODULE_ROOTS=Project \
  DOCGEN_DEPS_DOCS_URL=https://deps.example/fallback/ \
  DOCGEN_DEPS_DOCS_URLS='DepA=https://deps.example/a/,DepB=https://deps.example/b' \
  lake build Project:docs)

LOCAL_BUILD="$LOCAL_DIR/.lake/build"
local_html="$LOCAL_BUILD/doc/Project.html"
if [ ! -f "$local_html" ]; then
  echo "FAIL: Project.html was not generated"
  exit 1
fi
echo "OK: Project.html exists"
check_no_html "$LOCAL_BUILD/doc" DepA DepB Init
for marker in Project.doc; do
  if [ ! -f "$LOCAL_BUILD/doc-data/$marker" ]; then
    echo "FAIL: the local module was not analyzed ($marker is missing)"
    exit 1
  fi
done
echo "OK: the local module was analyzed"
for marker in DepA.doc DepB.doc core-Init.doc; do
  if [ -e "$LOCAL_BUILD/doc-data/$marker" ]; then
    echo "FAIL: an external module was analyzed ($marker exists)"
    exit 1
  fi
done
echo "OK: the external modules and Lean core were not analyzed"
check_contains "$local_html" 'https://deps.example/a/find/?pattern=depAGreeting#doc'
check_contains "$local_html" 'https://deps.example/b/find/?pattern=depBGreeting#doc'
check_contains "$local_html" 'https://deps.example/fallback/find/?pattern=String#doc'
check_contains "$local_html" 'https://deps.example/fallback/find/?pattern=And#doc'
# Tactics from external modules are still listed.
check_contains "$LOCAL_BUILD/doc/tactics.html" 'simp'

# --- Phase 11: an external aggregator root, and a local module removed between builds ---

echo "=== Building local-only docs from an external aggregator root ==="
AGG_DIR="$TEST_DIR/aggregator"
mkdir -p "$AGG_DIR/Project"
cp "$TEST_DIR/lean-toolchain" "$TEST_DIR"/*.lean "$AGG_DIR/"
cat "$TEST_DIR/lakefile.lean" > "$AGG_DIR/lakefile.lean"
echo 'lean_lib Agg' >> "$AGG_DIR/lakefile.lean"
cat > "$AGG_DIR/Project/Old.lean" << 'EOF'
/-- A declaration in a module that a later build removes. -/
def projectOldGreeting := "soon gone"
EOF
printf 'import Project\nimport Project.Old\n' > "$AGG_DIR/Agg.lean"

agg_build() {
  (cd "$AGG_DIR" && env \
    DOCGEN_LOCAL_MODULE_ROOTS=Project \
    DOCGEN_DEPS_DOCS_URL=https://deps.example/fallback/ \
    lake build Agg:docs)
}

# A clean build: `Agg` itself is external, so nothing else asks for its olean.
agg_build
AGG_BUILD="$AGG_DIR/.lake/build"
check_html_file="$AGG_BUILD/doc/Project/Old.html"
if [ ! -f "$check_html_file" ]; then
  echo "FAIL: Project/Old.html was not generated"
  exit 1
fi
echo "OK: Project/Old.html exists"
check_contains "$check_html_file" 'projectOldGreeting'
check_no_html "$AGG_BUILD/doc" Agg DepA

echo "=== Removing a local module and rebuilding incrementally ==="
rm "$AGG_DIR/Project/Old.lean"
printf 'import Project\n' > "$AGG_DIR/Agg.lean"
agg_build
check_no_html "$AGG_BUILD/doc" Project/Old
if [ -e "$AGG_BUILD/doc-data/Project.Old.doc" ]; then
  echo "FAIL: the removed module's marker was kept"
  exit 1
fi
echo "OK: the removed module's marker was deleted"
for data in declaration-data-Project.Old.bmp backrefs-Project.Old.json; do
  if [ -e "$AGG_BUILD/doc-data/$data" ]; then
    echo "FAIL: the removed module's $data was kept"
    exit 1
  fi
done
if grep -qF projectOldGreeting "$AGG_BUILD/doc/declarations/declaration-data.bmp"; then
  echo "FAIL: the search index still lists the removed module's declarations"
  exit 1
fi
echo "OK: the removed module's search data is gone"
if command -v sqlite3 >/dev/null; then
  if [ -n "$(sqlite3 "$AGG_BUILD/api-docs.db" "SELECT name FROM modules WHERE name = 'Project.Old'")" ]; then
    echo "FAIL: the removed module is still in the database"
    exit 1
  fi
  echo "OK: the removed module is no longer in the database"
else
  echo "SKIP: sqlite3 is not installed, so the database was not inspected"
fi

# --- Phase 12: making a module external and then local again re-analyzes it ---

echo "=== Making a module external and then local again ==="
TOGGLE_DIR="$TEST_DIR/toggle"
mkdir -p "$TOGGLE_DIR"
cp "$TEST_DIR/lean-toolchain" "$TEST_DIR"/*.lean "$TOGGLE_DIR/"
toggle_build() {
  (cd "$TOGGLE_DIR" && env DOCGEN_DEPS_DOCS_URL=https://deps.example/fallback/ \
    DOCGEN_LOCAL_MODULE_ROOTS="$1" lake build Project:docs)
}
# DepA is local, then external, then local again.
toggle_build Project,DepA
check_contains "$TOGGLE_DIR/.lake/build/doc/DepA.html" 'A greeting from the first dependency'
toggle_build Project
# As in a clean build with these roots, nothing of DepA's local documentation is left.
check_no_html "$TOGGLE_DIR/.lake/build/doc" DepA
if [ -e "$TOGGLE_DIR/.lake/build/doc-data/declaration-data-DepA.bmp" ]; then
  echo "FAIL: DepA's search data outlived its becoming external"
  exit 1
fi
echo "OK: DepA's search data went with its page"
toggle_build Project,DepA
# Documented in full again, rather than left with the name-only record of an external module.
check_contains "$TOGGLE_DIR/.lake/build/doc/DepA.html" 'A greeting from the first dependency'

echo "SUCCESS: Multi-library and interproject documentation tests passed"
