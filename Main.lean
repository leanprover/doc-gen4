import DocGen4
import Lean
import Cli

open DocGen4 Lean Cli

def findLeanInk? (p : Parsed) : IO (Option System.FilePath) := do
  match p.flag? "ink" with
  | some ink =>
    let inkPath := System.FilePath.mk ink.value
    if ←inkPath.pathExists then
      pure $ some inkPath
    else
      throw $ IO.userError "Invalid path to LeanInk binary provided"
  | none => pure none

def runDocGenCmd (p : Parsed) : IO UInt32 := do
  let modules : List String := p.variableArgsAs! String |>.toList
  if p.hasFlag "single" && p.hasFlag "setup" then
    throw $ IO.userError "Can't have single and setup at the same time"
  else
    let res ← lakeSetup modules
    let modules := modules.map Name.mkSimple
    match res with
    | Except.ok (ws, leanHash) =>
        IO.println s!"Loading modules from: {←searchPathRef.get}"
      --if p.hasFlag "single" then
      --  if modules.length ≠ 1 then
      --    throw $ IO.userError "Called single with more than a single module"
      --  else
      --    let (doc, hierarchy) ← load modules false
      --    IO.println "Outputting HTML"
      --    let baseConfig := getSimpleBaseContext hierarchy
      --    htmlOutputResults baseConfig doc ws leanHash (←findLeanInk? p)
      --    pure 0
      --else if p.hasFlag "setup" then
      --  let config := {
      --    fileName := default,
      --    fileMap := default,
      --  }
      --  let env ← importModules (List.map (Import.mk · false) modules) Options.empty
      --  let relevantModules ← Prod.fst <$> Meta.MetaM.toIO (Process.getRelevantModules modules) config { env := env } {}
      --  let hierarchy := Hierarchy.fromArray relevantModules.toArray
      --  let baseConfig := getSimpleBaseContext hierarchy
      --  htmlOutputSetup baseConfig
      --  pure 0
      --else
        let (doc, hierarchy) ← load modules true
        IO.println "Outputting HTML"
        htmlOutput doc hierarchy ws leanHash (←findLeanInk? p)
        pure 0
    | Except.error rc => pure rc

def docGenCmd : Cmd := `[Cli|
  "doc-gen4" VIA runDocGenCmd; ["0.0.1"]
  "A documentation generator for Lean 4."

  FLAGS:
    ink : String; "Path to a LeanInk binary to use for rendering the Lean sources."
    --single; "Generate documentation only for a single module, will cause broken links if there are others"
    --setup; "Only output the files that are always required"

  ARGS:
    ...modules : String; "The modules to generate the HTML for"
]

def main (args : List String) : IO UInt32 :=
  docGenCmd.validate args
