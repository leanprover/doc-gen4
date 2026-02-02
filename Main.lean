import DocGen4
import Lean
import Cli

open DocGen4 DocGen4.DB DocGen4.Output Lean Cli

def getTopLevelModules (p : Parsed) : IO (List String) :=  do
  let topLevelModules := p.variableArgsAs! String |>.toList
  if topLevelModules.length == 0 then
    throw <| IO.userError "No topLevelModules provided."
  return topLevelModules

def runHeaderDataCmd (p : Parsed) : IO UInt32 := do
  let buildDir := match p.flag? "build" with
    | some dir => dir.as! String
    | none => ".lake/build"
  headerDataOutput buildDir
  return 0

def runSingleCmd (p : Parsed) : IO UInt32 := do
  let buildDir := match p.flag? "build" with
    | some dir => dir.as! String
    | none => ".lake/build"
  let dbFile := p.positionalArg! "db" |>.as! String
  let relevantModules := #[p.positionalArg! "module" |>.as! String |> String.toName]
  let sourceUri := p.positionalArg! "sourceUri" |>.as! String
  let doc ← load <| .analyzeConcreteModules relevantModules
  updateModuleDb doc buildDir dbFile (some sourceUri)
  return 0

def runIndexCmd (p : Parsed) : IO UInt32 := do
  let buildDir := match p.flag? "build" with
    | some dir => dir.as! String
    | none => ".lake/build"
  let hierarchy ← Hierarchy.fromDirectory (Output.basePath buildDir)
  let baseConfig ← getSimpleBaseContext buildDir hierarchy
  htmlOutputIndex baseConfig
  return 0

def runGenCoreCmd (p : Parsed) : IO UInt32 := do
  let buildDir := match p.flag? "build" with
    | some dir => dir.as! String
    | none => ".lake/build"
  let dbFile := p.positionalArg! "db" |>.as! String
  let module := p.positionalArg! "module" |>.as! String |> String.toName
  let doc ← load <| .analyzePrefixModules module
  updateModuleDb doc buildDir dbFile none
  return 0

def runDocGenCmd (_p : Parsed) : IO UInt32 := do
  IO.println "You most likely want to use me via Lake now, check my README on Github on how to:"
  IO.println "https://github.com/leanprover/doc-gen4"
  return 0

/-- A source linker that uses URLs from the database, falling back to core module URLs -/
def dbSourceLinker (sourceUrls : Std.HashMap Name String) (_gitUrl? : Option String) (module : Name) : Option DeclarationRange → String :=
  let root := module.getRoot
  let leanHash := Lean.githash
  if root == `Lean ∨ root == `Init ∨ root == `Std then
    let parts := module.components.map (Name.toString (escape := false))
    let path := "/".intercalate parts
    Output.SourceLinker.mkGithubSourceLinker s!"https://github.com/leanprover/lean4/blob/{leanHash}/src/{path}.lean"
  else if root == `Lake then
    let parts := module.components.map (Name.toString (escape := false))
    let path := "/".intercalate parts
    Output.SourceLinker.mkGithubSourceLinker s!"https://github.com/leanprover/lean4/blob/{leanHash}/src/lake/{path}.lean"
  else
    -- Look up source URL from database
    match sourceUrls[module]? with
    | some url =>
      if url.startsWith "vscode://file/" then
        Output.SourceLinker.mkVscodeSourceLinker url
      else if url.startsWith "https://github.com" then
        Output.SourceLinker.mkGithubSourceLinker url
      else
        fun _ => url
    | none =>
      -- Fallback for modules without source URL
      fun _ => "#"

def runFromDbCmd (p : Parsed) : IO UInt32 := do
  let buildDir := match p.flag? "build" with
    | some dir => dir.as! String
    | none => ".lake/build"
  let dbPath := p.positionalArg! "db" |>.as! String
  let manifestOutput? := (p.flag? "manifest").map (·.as! String)

  -- Phase 1: Load shared index (fast - just names and cross-references)
  let start ← IO.monoMsNow
  IO.println s!"Loading shared index from database: {dbPath}"
  let db ← openDbForReading dbPath
  let shared ← loadSharedIndex db
  IO.println s!"Index loaded in {(← IO.monoMsNow) - start}ms ({shared.name2ModIdx.size} declarations, {shared.moduleNames.size} modules)"

  -- Add `references` pseudo-module to hierarchy since references.html is always generated
  let start ← IO.monoMsNow
  let hierarchy := Hierarchy.fromArray (shared.moduleNames.push `references)
  IO.println s!"Hierarchy took {(← IO.monoMsNow) - start}ms"
  let start ← IO.monoMsNow
  let baseConfig ← getSimpleBaseContext buildDir hierarchy
  IO.println s!"Context took {(← IO.monoMsNow) - start}ms"

  -- Phase 2: Parallel HTML generation (one task per module)
  let start ← IO.monoMsNow
  IO.println s!"Generating HTML in parallel to: {buildDir}"
  let outputs ← htmlOutputResultsParallel baseConfig dbPath shared (sourceLinker? := some (dbSourceLinker shared.sourceUrls))
  IO.println s!"HTML took {(← IO.monoMsNow) - start}ms"
  let start ← IO.monoMsNow
  htmlOutputIndex baseConfig
  IO.println s!"HTML index took {(← IO.monoMsNow) - start}ms"
  IO.println "Done!"
  if let .some manifestOutput := manifestOutput? then
    IO.FS.writeFile manifestOutput (Lean.toJson outputs).compress
  return 0

def runBibPrepassCmd (p : Parsed) : IO UInt32 := do
  let buildDir := match p.flag? "build" with
    | some dir => dir.as! String
    | none => ".lake/build"
  if p.hasFlag "none" then
    IO.println "INFO: reference page disabled"
    disableBibFile buildDir
  else
    match p.variableArgsAs! String with
    | #[source] =>
      let contents ← IO.FS.readFile source
      if p.hasFlag "json" then
        IO.println "INFO: 'references.json' will be copied to the output path; there will be no 'references.bib'"
        preprocessBibJson buildDir contents
      else
        preprocessBibFile buildDir contents Bibtex.process
    | _ => throw <| IO.userError "there should be exactly one source file"
  return 0

def singleCmd := `[Cli|
  single VIA runSingleCmd;
  "Populate the database with documentation for the specified module."

  FLAGS:
    b, build : String; "Build directory."

  ARGS:
    module : String; "The module to document."
    db : String; "Path to the SQLite database (relative to build dir)"
    sourceUri : String; "The sourceUri as computed by the Lake facet"
]

def indexCmd := `[Cli|
  index VIA runIndexCmd;
  "Index the documentation that has been generated by single."

  FLAGS:
    b, build : String; "Build directory."
]

def genCoreCmd := `[Cli|
  genCore VIA runGenCoreCmd;
  "Populate the database with documentation for the specified Lean core module (Init, Std, Lake, Lean)."

  FLAGS:
    b, build : String; "Build directory."

  ARGS:
    module : String; "The core module prefix to document (e.g., Init, Lean)."
    db : String; "Path to the SQLite database (relative to build dir)"
]

def bibPrepassCmd := `[Cli|
  bibPrepass VIA runBibPrepassCmd;
  "Run the bibliography prepass: copy the bibliography file to output directory. By default it assumes the input is '.bib'."

  FLAGS:
    n, none; "Disable bibliography in this project."
    j, json; "The input file is '.json' which contains an array of objects with 4 fields: 'citekey', 'tag', 'html' and 'plaintext'."
    b, build : String; "Build directory."

  ARGS:
    ...source : String; "The bibliography file. We only support one file for input. Should be '.bib' or '.json' according to flags."
]

def headerDataCmd := `[Cli|
  headerData VIA runHeaderDataCmd;
  "Produce `header-data.bmp`, this allows embedding of doc-gen declarations into other pages and more."

  FLAGS:
    b, build : String; "Build directory."
]

def fromDbCmd := `[Cli|
  fromDb VIA runFromDbCmd;
  "Generate all HTML documentation from a SQLite database."

  FLAGS:
    b, build : String; "Build directory (default: .lake/build)"
    m, manifest : String; "Manifest output file, listing all generated HTML files."

  ARGS:
    db : String; "Path to the SQLite database"
]

def docGenCmd : Cmd := `[Cli|
  "doc-gen4" VIA runDocGenCmd; ["0.1.0"]
  "A documentation generator for Lean 4."

  SUBCOMMANDS:
    singleCmd;
    indexCmd;
    genCoreCmd;
    bibPrepassCmd;
    headerDataCmd;
    fromDbCmd
]

def main (args : List String) : IO UInt32 :=
  docGenCmd.validate args
