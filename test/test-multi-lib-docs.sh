#!/usr/bin/env bash
#
# Regression tests for multi-library and interproject documentation generation:
# - concurrent and incremental builds retain every generated library;
# - local-only builds omit dependency pages and link each dependency to its own docs site;
# - incomplete external documentation mappings fail instead of producing broken links.
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

# --- Phase 3: generate only local project docs with per-dependency URLs ---

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

# --- Phase 4: reject incomplete URL mappings ---

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

echo "SUCCESS: Multi-library and interproject documentation tests passed"
