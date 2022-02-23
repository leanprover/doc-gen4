import DocGen4
import Lean
import Cli

open DocGen4 Lean Cli

def runDocGenCmd (p : Parsed) : IO UInt32 := do
  let root := p.positionalArg! "root" |>.as! String
  let modules : List String := p.variableArgsAs! String |>.toList
  let path ← lakeSetupSearchPath (←getLakePath) modules
  IO.println s!"Loading modules from: {path}"
  let doc ← load $ modules.map Name.mkSimple
  IO.println "Outputting HTML"
  htmlOutput doc root
  pure 0

def docGenCmd : Cmd := `[Cli|
  "doc-gen4" VIA runDocGenCmd; ["0.0.1"]
  "A documentation generator for Lean 4."

  ARGS:
    root : String; "The root URL to generate the HTML for (will be relative in the future)"
    ...modules : String; "The modules to generate the HTML for"
]

def main (args : List String) : IO UInt32 :=
  docGenCmd.validate args
