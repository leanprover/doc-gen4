import DocGen4
import Lean
import Cli

open DocGen4 Lean Cli

def runDocGenCmd (p : Parsed) : IO UInt32 := do
  IO.println s!"{p}"
  let modules : List String := p.variableArgsAs! String |>.toList
  let res ← lakeSetup modules
  match res with
  | Except.ok (ws, leanHash) =>
    IO.println s!"Loading modules from: {←searchPathRef.get}"
    let doc ← load $ modules.map Name.mkSimple
    IO.println "Outputting HTML"
    match p.flag? "ink" with
    | some ink =>
      let inkPath := System.FilePath.mk ink.value
      if ←inkPath.pathExists then
         htmlOutput doc ws leanHash inkPath
      else
        throw $ IO.userError "Invalid path to LeanInk binary provided"
    | none => htmlOutput doc ws leanHash none
    pure 0
  | Except.error rc => pure rc

def docGenCmd : Cmd := `[Cli|
  "doc-gen4" VIA runDocGenCmd; ["0.0.1"]
  "A documentation generator for Lean 4."

  FLAGS:
    ink : String; "Path to a LeanInk binary to use for rendering the Lean sources."

  ARGS:
    ...modules : String; "The modules to generate the HTML for"
]

def main (args : List String) : IO UInt32 :=
  docGenCmd.validate args
