#!/usr/bin/env bash
#
# Regression test for the `srcUri.github` facet's parsing of git remote URLs
# (issue #376): scp-like SSH remotes are accepted with and without the `.git`
# suffix, and HTTPS remotes keep working.
#
# Usage: run from the doc-gen4 repo root (or pass it as $1).
#   bash test/test-src-uri.sh
#   bash test/test-src-uri.sh /path/to/doc-gen4

set -euo pipefail

DOCGEN4_DIR="$(cd "${1:-$(dirname "$0")/..}" && pwd)"
TEST_DIR="$(mktemp -d)"

cleanup() { rm -rf "$TEST_DIR"; }
trap cleanup EXIT

echo "doc-gen4: $DOCGEN4_DIR"
echo "test project: $TEST_DIR"

cp "$DOCGEN4_DIR/lean-toolchain" "$TEST_DIR/"

cat > "$TEST_DIR/lakefile.lean" << EOF
import Lake
open Lake DSL

package test

require «doc-gen4» from "$DOCGEN4_DIR"

lean_lib Test
EOF

cat > "$TEST_DIR/Test.lean" << 'EOF'
/-- A trivial declaration. -/
def greeting := "hello"
EOF

# The facet reads the project's git remote, so the test project must be a git
# repository with a commit.
git -C "$TEST_DIR" init -q
git -C "$TEST_DIR" add -A
git -C "$TEST_DIR" -c user.name=test -c user.email=test@example.com commit -qm init

check_remote() {
  local remote="$1" expected="$2"
  if git -C "$TEST_DIR" remote get-url origin > /dev/null 2>&1; then
    git -C "$TEST_DIR" remote set-url origin "$remote"
  else
    git -C "$TEST_DIR" remote add origin "$remote"
  fi
  echo "=== remote: $remote ==="
  local out
  if out="$(cd "$TEST_DIR" && lake build :srcUri.github 2>&1)"; then
    if [[ "$out" == *"at $expected @"* ]]; then
      echo "OK: $remote resolves to $expected"
    else
      echo "FAIL: expected base URL $expected in the output:"
      echo "$out"
      exit 1
    fi
  else
    echo "FAIL: \`lake build :srcUri.github\` failed for remote $remote:"
    echo "$out"
    exit 1
  fi
}

# scp-like SSH remote without the `.git` suffix (the form from issue #376)
check_remote "git@github.com:some-org/some-repo" "https://github.com/some-org/some-repo"
# scp-like SSH remote with the `.git` suffix
check_remote "git@github.com:some-org/some-repo.git" "https://github.com/some-org/some-repo"
# HTTPS remotes, without and with the `.git` suffix
check_remote "https://github.com/some-org/some-repo" "https://github.com/some-org/some-repo"
check_remote "https://github.com/some-org/some-repo.git" "https://github.com/some-org/some-repo"

echo "SUCCESS: all remote URL forms resolve to the expected base URLs"
