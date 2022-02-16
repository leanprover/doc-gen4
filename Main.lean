import DocGen4
import Lean

open DocGen4 Lean IO

def main (modules : List String) : IO Unit := do
  if modules.isEmpty then
    IO.println "Usage: doc-gen4 Module1 Module2 ..."
    IO.Process.exit 1
    return
  let path ← lakeSetupSearchPath (←getLakePath) modules.toArray
  IO.println s!"Loading modules from: {path}"
  let doc ← load $ modules.map Name.mkSimple
  IO.println "Outputting HTML"
  htmlOutput doc
