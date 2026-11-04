#!/usr/bin/env bash
#
# Regression test: verify that equations are generated for definitions whose bodies are exposed
# to importers, and not for definitions whose bodies a module system module seals.
#
# Usage: run from the doc-gen4 repo root (or pass it as $1).
#   ./test/test-module-equations.sh
#   ./test/test-module-equations.sh /path/to/doc-gen4

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

lean_lib ModuleMod
lean_lib ClassicMod
EOF

cat > "$TEST_DIR/ModuleMod.lean" << 'EOF'
module

/-- A definition whose body is sealed. -/
public def sealedDef (n : Nat) : Nat := n + 1

/-- A definition whose body is exposed. -/
@[expose] public def exposedDef (n : Nat) : Nat := n + 2
EOF

cat > "$TEST_DIR/ClassicMod.lean" << 'EOF'
/-- A definition outside the module system. -/
def classicDef (n : Nat) : Nat := n + 3
EOF

export LEAN_ABORT_ON_PANIC=1
export DOCGEN_SRC=file
DB="$TEST_DIR/.lake/build/api-docs.db"

(cd "$TEST_DIR" && lake build ModuleMod:docs ClassicMod:docs)

FAIL=0

# Checks that declaration $1 has exactly $2 equations in the database.
check_equations() {
  local count
  count="$(sqlite3 "$DB" "SELECT count(e.sequence) FROM name_info n
    LEFT JOIN definition_equations e ON e.module_name = n.module_name AND e.position = n.position
    WHERE n.name = '$1'")"
  if [ "$count" -eq "$2" ]; then
    echo "OK: $1 has $2 equation(s)"
  else
    echo "FAIL: $1 has $count equation(s), expected $2"
    FAIL=1
  fi
}

check_equations sealedDef 0
check_equations exposedDef 1
check_equations classicDef 1

if [ "$FAIL" -eq 1 ]; then
  exit 1
fi

echo "SUCCESS: equations are generated only for exposed definitions"
