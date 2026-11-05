#!/usr/bin/env bash
#
# Regression test: links to internal names that have no anchor of their own,
# such as `Eq.ndrec` in the equation of a derived `DecidableEq` instance, point
# to the anchor of the declaration they belong to, and every link into the
# library's page and into `Init/Prelude.html` names an anchor that exists.
#
# Usage: run from the doc-gen4 repo root (or pass it as $1).
#   ./test/test-internal-name-links.sh
#   ./test/test-internal-name-links.sh /path/to/doc-gen4

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

lean_lib Repro
EOF

cat > "$TEST_DIR/Repro.lean" << 'EOF'
/-- A type whose constructors have fields, with a derived decidable equality. -/
inductive Shape where
  | circle (radius : Nat)
  | square (side : Nat)
  deriving DecidableEq

/-- A structure, whose fields keep their own anchors. -/
structure Point where
  x : Nat
  y : Nat

/-- Uses a field of `Point`. -/
def Point.sum (p : Point) : Nat := p.x + p.y
EOF

export LEAN_ABORT_ON_PANIC=1
export DOCGEN_SRC=file
DOC_DIR="$TEST_DIR/.lake/build/doc"

(cd "$TEST_DIR" && lake build Repro:docs)

FAIL=0
PAGE="$DOC_DIR/Repro.html"
PRELUDE="$DOC_DIR/Init/Prelude.html"

# Checks that every link from Repro.html to page $2 (as written in the href)
# names an anchor that exists in file $1.
check_anchors() {
  local target="$1" href="$2" count=0
  while IFS= read -r frag; do
    count=$((count + 1))
    if grep -qF -- "id=\"$frag\"" "$target"; then
      echo "OK: $href#$frag"
    else
      echo "FAIL: $href#$frag has no anchor in $(basename "$target")"
      FAIL=1
    fi
  done < <({ grep -o -- "href=\"$href#[^\"]*\"" "$PAGE" || true; } |
             sed -e "s|^href=\"$href#||" -e 's|"$||' | sort -u)
  echo "$count distinct anchors linked from Repro.html to $href"
}

if grep -qF -- '#Eq.ndrec"' "$PAGE"; then
  echo "FAIL: Repro.html links to the internal name Eq.ndrec as an anchor"
  FAIL=1
else
  echo "OK: Repro.html has no link to an Eq.ndrec anchor"
fi

if grep -qF -- 'Init/Prelude.html#Eq"' "$PAGE"; then
  echo "OK: Repro.html links to the Eq anchor"
else
  echo "FAIL: Repro.html has no link to the Eq anchor"
  FAIL=1
fi

if grep -qF -- 'Repro.html#Point.x"' "$PAGE"; then
  echo "OK: Repro.html links to the Point.x field anchor"
else
  echo "FAIL: Repro.html has no link to the Point.x field anchor"
  FAIL=1
fi

check_anchors "$PAGE" "./Repro.html"
check_anchors "$PRELUDE" "./Init/Prelude.html"

if [ "$FAIL" -eq 1 ]; then
  exit 1
fi

echo "SUCCESS: links to internal names point to existing anchors"
