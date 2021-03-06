import DocGen4
import Lean
import Cli

open DocGen4 Lean Cli

def findLeanInk? (p : Parsed) : IO (Option System.FilePath) := do
  match p.flag? "ink" with
  | some ink =>
    let inkPath := System.FilePath.mk ink.value
    if ←inkPath.pathExists then
      pure <| some inkPath
    else
      throw <| IO.userError "Invalid path to LeanInk binary provided"
  | none => pure none

def getTopLevelModules (p : Parsed) : IO (List String) :=  do
  let topLevelModules := p.variableArgsAs! String |>.toList
  if topLevelModules.length == 0 then
    throw <| IO.userError "No topLevelModules provided."
  pure topLevelModules

def runSingleCmd (p : Parsed) : IO UInt32 := do
    let relevantModules := [p.positionalArg! "module" |>.as! String]
    let res ← lakeSetup (relevantModules)
    match res with
    | Except.ok ws =>
      let relevantModules := relevantModules.map String.toName
      let (doc, hierarchy) ← load (.loadAllLimitAnalysis relevantModules)
      IO.println "Outputting HTML"
      let baseConfig := getSimpleBaseContext hierarchy
      htmlOutputResults baseConfig doc ws (←findLeanInk? p)
      pure 0
    | Except.error rc => pure rc

def runIndexCmd (_p : Parsed) : IO UInt32 := do
  let hierarchy ← Hierarchy.fromDirectory basePath
  let baseConfig := getSimpleBaseContext hierarchy
  htmlOutputIndex baseConfig
  pure 0

def runDocGenCmd (p : Parsed) : IO UInt32 := do
  let modules : List String := p.variableArgsAs! String |>.toList
  if modules.length == 0 then
    throw <| IO.userError "No modules provided."

  let res ← lakeSetup modules
  match res with
  | Except.ok ws =>
    IO.println s!"Loading modules from: {←searchPathRef.get}"
    let modules := modules.map String.toName
    let (doc, hierarchy) ← load (.loadAll modules)
    IO.println "Outputting HTML"
    htmlOutput doc hierarchy ws (←findLeanInk? p)
    pure 0
  | Except.error rc => pure rc

def singleCmd := `[Cli|
  single VIA runSingleCmd;
  "Only generate the documentation for the module it was given, might contain broken links unless all documentation is generated."

  FLAGS:
    ink : String; "Path to a LeanInk binary to use for rendering the Lean sources."

  ARGS:
    module : String; "The module to generate the HTML for. Does not have to be part of topLevelModules."
]

def indexCmd := `[Cli|
  index VIA runIndexCmd;
  "Index the documentation that has been generated by single."
  ARGS:
    ...topLevelModule : String; "The top level modules this documentation will be for."
]

def docGenCmd : Cmd := `[Cli|
  "doc-gen4" VIA runDocGenCmd; ["0.0.1"]
  "A documentation generator for Lean 4."

  FLAGS:
    ink : String; "Path to a LeanInk binary to use for rendering the Lean sources."

  ARGS:
    ...modules : String; "The modules to generate the HTML for."

  SUBCOMMANDS:
    singleCmd;
    indexCmd
]

def main (args : List String) : IO UInt32 :=
  docGenCmd.validate args
