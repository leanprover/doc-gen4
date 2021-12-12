import DocGen4
import Lean

open DocGen4 Lean IO

def main (args : List String) : IO Unit := do
  let modules := args
  let path ← lakeSetupSearchPath (←getLakePath) modules.toArray
  IO.println s!"Loading modules from: {path}"
  let doc ← load $ modules.map Name.mkSimple
  htmlOutput doc
