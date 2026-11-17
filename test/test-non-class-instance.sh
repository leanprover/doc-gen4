#!/usr/bin/env bash
#
# Regression test for https://github.com/leanprover/doc-gen4/issues/348:
# doc-gen4 used to panic in `InstanceInfo.ofDefinitionInfo` when `Meta.isClass?`
# returned `none` for an instance, which happens for instances proving
# structure predicates (a `structure ... : Prop`, not a `class`) such as
# Mathlib's `Continuous`.
#
# Note: current core Lean rejects non-class instances at declaration time
# (`checkNonClassInstance`), except when the type contains `sorryAx`.
# The test instance below uses that escape hatch to stand in for real-world
# occurrences of such instances (e.g. `@[fun_prop] instance`s in projects
# built before that check was introduced).
#
# Usage: run from the doc-gen4 repo root (or pass it as $1).
#   ./test/test-non-class-instance.sh
#   ./test/test-non-class-instance.sh /path/to/doc-gen4

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

lean_lib NonClassInst
EOF

cat > "$TEST_DIR/NonClassInst.lean" << 'EOF'
/-- A Prop-valued structure predicate, not a class (like Mathlib's `Continuous`). -/
structure MyPred (f : Nat → Nat) : Prop where
  trivial : True

/--
An instance whose target is not a class. `Meta.isClass?` returns `none` for its type,
which used to make doc-gen4 panic (issue #348). The `sorryAx` in the type is the only
way to register such an instance on current Lean; see the top of
`test/test-non-class-instance.sh`.
-/
instance instMyPredSorry : MyPred (fun _ => sorryAx Nat true) := ⟨True.intro⟩

/-- A normal class instance for comparison. -/
instance : Add Nat := inferInstance
EOF

export LEAN_ABORT_ON_PANIC=1
export DOCGEN_SRC=file
DOC_DIR="$TEST_DIR/.lake/build/doc"

# With LEAN_ABORT_ON_PANIC=1 a panic aborts the build, so a successful build
# already proves that the instance no longer crashes doc-gen4.
(cd "$TEST_DIR" && lake build NonClassInst:docs)

FAIL=0

check_contains() {
  local file="$1"
  shift
  for needle in "$@"; do
    if grep -qF -- "$needle" "$file"; then
      echo "OK: $(basename "$file") contains: $needle"
    else
      echo "FAIL: $(basename "$file") does not contain: $needle"
      FAIL=1
    fi
  done
}

# The non-class instance is documented like any other declaration ...
check_contains "$DOC_DIR/NonClassInst.html" \
  'id="instMyPredSorry"' \
  'id="MyPred"'

# ... and indexed under the head constant of its target type.
check_contains "$DOC_DIR/declarations/declaration-data.bmp" \
  '"MyPred":["instMyPredSorry"]'

if [ "$FAIL" -eq 1 ]; then
  exit 1
fi

echo "SUCCESS: instances of non-class structure predicates are documented"
