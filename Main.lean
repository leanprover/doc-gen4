import DocGen4
import Lean

open DocGen4 Lean

def main : IO Unit := do
  -- This should be set by lake at some point
  setSearchPath ["/home/nix/Desktop/formal_verification/lean/mathlib4/build/lib", "/home/nix/.elan/toolchains/leanprover--lean4---nightly-2021-11-24/lib/lean"]
  let doc ← load [`Mathlib]
  IO.println s!"Processed {doc.size} declarations"
  return ()
