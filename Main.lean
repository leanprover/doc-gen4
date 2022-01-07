import DocGen4
import Lean

open DocGen4 Lean IO

def main (args : List String) : IO Unit := do
  let root := args.head!
  let modules := args.tail!
  let path ← lakeSetupSearchPath (←getLakePath) modules.toArray
  IO.println s!"Loading modules from: {path}"
  let doc ← load $ modules.map Name.mkSimple
  IO.println "Outputting HTML"
  htmlOutput doc root
