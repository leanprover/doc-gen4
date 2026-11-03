#!/usr/bin/env bash
# Compare docInfo's blocking/composed Lake Job patterns (issue #434).
# This holds a real dependency unresolved; it does not run doc-gen4, measure
# peak memory, or establish the cause of a full documentation build's OOM.
# The marker cases exercise Lake's actual buildFileUnlessUpToDate' helper.
#
# Usage: bash test/test-src-uri-job-dependency.sh [/path/to/doc-gen4]
set -euo pipefail

DOCGEN4_DIR="$(cd "${1:-$(dirname "$0")/..}" && pwd)"
TEST_DIR="$(mktemp -d "${TMPDIR:-/tmp}/docgen-srcuri-jobs.XXXXXX")"
cleanup() { rm -rf "$TEST_DIR"; }
trap cleanup EXIT

cp "$DOCGEN4_DIR/lean-toolchain" "$TEST_DIR/"
cp "$DOCGEN4_DIR/test/fixtures/src-uri-jobs.lean" "$TEST_DIR/lakefile.lean"
export DOCGEN_SRCURI_PROBE_DIR="$TEST_DIR"

python3 - <<'PY'
import json
import os
from pathlib import Path
import subprocess
import sys

root = Path(os.environ["DOCGEN_SRCURI_PROBE_DIR"])
env = {**os.environ, "LEAN_NUM_THREADS": "2"}


def run(target, *, failure=False):
    task_env = dict(env)
    task_env.pop("PROBE_URI_FAIL", None)
    if failure:
        task_env["PROBE_URI_FAIL"] = "1"
    result = subprocess.run(
        ["lake", "--no-ansi", "build", target], cwd=root, env=task_env,
        text=True, capture_output=True, timeout=20,
    )
    output = result.stdout + result.stderr
    print(output, end="")
    return result.returncode, output


if sys.platform == "linux":
    for repetition in range(3):
        growth = {}
        for mode in ("blocking", "composed"):
            code, output = run(mode)
            assert code == 0, output
            line = next(line for line in output.splitlines() if line.startswith("PROBE mode="))
            sample = dict(item.split("=", 1) for item in line.split()[1:])
            assert sample["entered"] == "8" and sample["doneBefore"] == "0"
            assert sample["pending"] == "true", sample
            growth[mode] = int(sample["waiting"]) - int(sample["baseline"])
            assert f"PROBE completed mode={mode} count=8" in output
        assert growth["blocking"] >= growth["composed"] + 4, growth
        print("THREAD_GROWTH", json.dumps({"repetition": repetition, **growth}))
    for target in ("blockingFailure", "composedFailure"):
        code, output = run(target)
        assert code != 0 and "URI dependency failed" in output
        assert not list(root.glob("marker-*-true-*"))
else:
    print("Skipping /proc process-thread snapshots on this platform")


def events(name):
    file = root / name
    return len(file.read_text().splitlines()) if file.exists() else 0


results = []
for mode, target in (("blocking", "cacheBlocking"), ("composed", "cacheComposed")):
    for index, phase in enumerate(("cold", "cached", "cached_uri_failure", "stale_uri_failure")):
        marker = root / f"cached-marker-{mode}"
        if phase == "stale_uri_failure":
            marker.unlink()
        failed_uri = index >= 2
        code, output = run(target, failure=failed_uri)
        expected = 0 if not failed_uri or (mode == "blocking" and index == 2) else 1
        assert code == expected, (mode, phase, output)
        assert events(f"generations-{mode}") == 1
        queries = events(f"queries-{mode}")
        assert queries == ([1, 1, 1, 2] if mode == "blocking" else [1, 2, 3, 4])[index]
        assert marker.exists() == (index != 3)
        if marker.exists():
            assert marker.read_text() == "https://example.org/blob/frozen/Module.lean"
        results.append({"mode": mode, "phase": phase, "exit": code, "queries": queries})
print("MARKER_RESULTS", json.dumps(results))
print("SUCCESS: dependency scheduling, URI values/failures and cached-marker tradeoff checked")
PY
