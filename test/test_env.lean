/-
Test script to verify DOCGEN_MAX_HEARTBEATS environment variable handling.

Run with:
  DOCGEN_MAX_HEARTBEATS=5_000_000_000 lake env lean test/test_env.lean

Or without the env var:
  lake env lean test/test_env.lean
-/

def defaultMaxHeartbeats : Nat := 100_000_000

/-- Parse a natural number, allowing underscores as separators (e.g., "100_000_000"). -/
def parseNatWithUnderscores (s : String) : Option Nat :=
  (s.replace "_" "").toNat?

def main : IO Unit := do
  IO.println "=== DOCGEN_MAX_HEARTBEATS Environment Variable Test ==="
  IO.println ""

  -- Test 1: Check if environment variable is visible
  let envValue ← IO.getEnv "DOCGEN_MAX_HEARTBEATS"
  match envValue with
  | none =>
    IO.println "❌ DOCGEN_MAX_HEARTBEATS is NOT set"
    IO.println s!"   Using default: {defaultMaxHeartbeats}"
  | some value =>
    IO.println s!"✅ DOCGEN_MAX_HEARTBEATS is set to: \"{value}\""

    -- Test 2: Parse the value
    match parseNatWithUnderscores value with
    | none =>
      IO.println s!"❌ Failed to parse \"{value}\" as Nat"
      IO.println s!"   Using default: {defaultMaxHeartbeats}"
    | some n =>
      IO.println s!"✅ Parsed successfully as: {n}"

  IO.println ""

  -- Test 3: Show all DOCGEN_* environment variables
  IO.println "=== All DOCGEN_* environment variables ==="
  for name in ["DOCGEN_MAX_HEARTBEATS", "DOCGEN_SRC", "DISABLE_EQUATIONS"] do
    let value ← IO.getEnv name
    match value with
    | none => IO.println s!"   {name}: (not set)"
    | some v => IO.println s!"   {name}: \"{v}\""

  IO.println ""
  IO.println "=== Test parseNatWithUnderscores ==="
  let testCases := [
    ("100000000", some 100000000),
    ("100_000_000", some 100000000),
    ("5_000_000_000", some 5000000000),
    ("1_2_3", some 123),
    ("abc", none),
    ("", none),
    ("123abc", none)
  ]
  for (input, expected) in testCases do
    let result := parseNatWithUnderscores input
    let status := if result == expected then "✅" else "❌"
    IO.println s!"   {status} parseNatWithUnderscores \"{input}\" = {result} (expected {expected})"

  IO.println ""
  IO.println "=== Done ==="
