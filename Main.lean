import DocGen4
import Lean
import Cli

open DocGen4 DocGen4.DB DocGen4.Output Lean Cli

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
  let library? := (p.flag? "lib").map (·.as! String)
  let doc ← load <| .analyzeConcreteModules relevantModules
  updateModuleDb builtinDocstringValues doc buildDir dbFile (some sourceUri) library?
  return 0

def runGenCoreCmd (p : Parsed) : IO UInt32 := do
  let buildDir := match p.flag? "build" with
    | some dir => dir.as! String
    | none => ".lake/build"
  let dbFile := p.positionalArg! "db" |>.as! String
  let module := p.positionalArg! "module" |>.as! String |> String.toName
  let doc ← load <| .analyzePrefixModules module
  updateModuleDb builtinDocstringValues doc buildDir dbFile none none
  return 0

/--
Deletes the files that the documentation of `modules` left in the build directory: the marker that
tells Lake the analysis is done, the page, and the per-module data that the HTML phase reads.

Lake decides from the marker file, not from the database, whether to analyze a module again, so a
module that returns to a library is analyzed again. `fromDb` takes the navigation bar and the link
index from the pages on disk, and the search index from every `declaration-data-*.bmp` in
`doc-data/`, so a module leaves the site when the HTML phase runs again.
-/
def deleteModuleFiles (buildDir : System.FilePath) (modules : Array Name) : IO Unit := do
  for m in modules do
    let files := #[
      buildDir / "doc-data" / s!"{m}.doc",
      buildDir / "doc-data" / s!"{m}.doc.trace",
      buildDir / "doc-data" / s!"{m}.doc.hash",
      declarationsBasePath buildDir / s!"declaration-data-{m}.bmp",
      declarationsBasePath buildDir / s!"backrefs-{m}.json",
      basePath buildDir / moduleNameToFile m
    ]
    for file in files do
      if ← file.pathExists then
        IO.FS.removeFile file

def runPruneLibCmd (p : Parsed) : IO UInt32 := do
  let buildDir : System.FilePath := match p.flag? "build" with
    | some dir => dir.as! String
    | none => ".lake/build"
  let dbPath := buildDir / (p.positionalArg! "db" |>.as! String)
  let library := p.positionalArg! "lib" |>.as! String
  let moduleListFile := p.positionalArg! "modules" |>.as! String

  -- The first build of a library runs this before the analysis creates the database.
  if !(← dbPath.pathExists) then
    return 0

  let contents ← IO.FS.readFile moduleListFile
  let keep := contents.splitOn "\n" |>.filterMap fun line =>
    if line.isEmpty then none else some line.toName
  match ← (pruneLibrary builtinDocstringValues dbPath library keep.toArray
      (deleteModuleFiles buildDir)).toBaseIO with
  | .error e =>
    IO.eprintln s!"pruneLib: {e}"
    return 1
  | .ok stale =>
    if !stale.isEmpty then
      IO.println s!"Removed {stale.size} module(s) that {library} no longer has"
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

/-- Flush the WAL so the database file is self-contained. Connection is closed on return. -/
def walCheckpoint (dbPath : String) : IO Unit := do
  -- The checkpoint requires a read-write connection, which can be blocked by concurrent
  -- documentation info writes for other libraries that this library doesn't depend on. This uses a
  -- very long timeout (24h) because a full Mathlib build on a slow machine could in principle keep
  -- the DB locked for a long time.
  let db ← SQLite.open dbPath (busyTimeoutMs := 86400000)
  db.exec "PRAGMA wal_checkpoint(TRUNCATE)"
  db.exec "PRAGMA optimize"

def runFromDbCmd (p : Parsed) : IO UInt32 := do
  let buildDir := match p.flag? "build" with
    | some dir => dir.as! String
    | none => ".lake/build"
  let dbPath := p.positionalArg! "db" |>.as! String
  let manifestOutput? := (p.flag? "manifest").map (·.as! String)
  let moduleRoots := (p.variableArgsAs! String).map String.toName

  -- Flush WAL so the database file is self-contained for concurrent readers
  walCheckpoint dbPath

  -- Load linking context (module names, source URLs, declaration locations)
  let db ← openForReading dbPath builtinDocstringValues
  let linkCtx ← db.loadLinkingContext

  -- Determine which modules to generate HTML for
  let targetModules ←
    if moduleRoots.isEmpty then
      pure linkCtx.moduleNames
    else
      db.getTransitiveImports moduleRoots

  let baseConfig ← getSimpleBaseContext buildDir (Hierarchy.fromArray targetModules)
  -- Add `references` pseudo-module to hierarchy only when bibliography data exists
  let hierarchy := Hierarchy.fromArray
    (if baseConfig.refs.isEmpty then targetModules else targetModules.push `references)
  let baseConfig := { baseConfig with hierarchy }

  -- Parallel HTML generation
  let (outputs, jsonModules) ← htmlOutputResultsParallel baseConfig dbPath linkCtx targetModules (sourceLinker? := some (dbSourceLinker linkCtx.sourceUrls))

  -- Load all tactics from DB in sorted order and convert markdown docstrings to HTML
  let allTacticsRaw ← db.loadAllTactics
  let refsMap : Std.HashMap String BibItem :=
    Std.HashMap.emptyWithCapacity baseConfig.refs.size |>.insertMany
      (baseConfig.refs.iter.map fun x => (x.citekey, x))
  let minimalSiteCtx : SiteContext := {
    result := { name2ModIdx := linkCtx.name2ModIdx, moduleNames := linkCtx.moduleNames, moduleInfo := {} }
    sourceLinker := fun _ _ => "#"
    refsMap := refsMap
  }
  let (allTactics, _) := allTacticsRaw.mapM Process.TacticInfo.docStringToHtml |>.run {} minimalSiteCtx baseConfig

  -- Generate the search index (declaration-data.bmp)
  htmlOutputIndex baseConfig jsonModules allTactics

  -- Update navbar to include all modules on disk
  updateNavbarFromDisk buildDir
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
    lib : String; "Name of the Lake library that contains the module"

  ARGS:
    module : String; "The module to document."
    db : String; "Path to the SQLite database (relative to build dir)"
    sourceUri : String; "The sourceUri as computed by the Lake facet"
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

def pruneLibCmd := `[Cli|
  pruneLib VIA runPruneLibCmd;
  "Delete the documentation of the modules that a library no longer has. The Lake facets run this."

  FLAGS:
    b, build : String; "Build directory (default: .lake/build)"

  ARGS:
    db : String; "Name of the SQLite database in the build directory"
    lib : String; "Name of the Lake library"
    modules : String; "File that holds the module names of the library, one per line"
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

-- Prior versions of doc-gen4 generated HTML for one module at a time, directly from the olean, and
-- then ran an index command at the end to create the search index. Now, `fromDb` generates all HTML
-- and the search index in a single pass from the DB.
def fromDbCmd := `[Cli|
  fromDb VIA runFromDbCmd;
  "Generate HTML documentation from a SQLite database."

  FLAGS:
    b, build : String; "Build directory (default: .lake/build)"
    m, manifest : String; "Manifest output file, listing all generated HTML files."

  ARGS:
    db : String; "Path to the SQLite database"
    ...modules : String; "Optional: Module roots to generate docs for (computes transitive closure)"
]

def docGenCmd : Cmd := `[Cli|
  "doc-gen4" VIA runDocGenCmd; ["0.1.0"]
  "A documentation generator for Lean 4."

  SUBCOMMANDS:
    singleCmd;
    genCoreCmd;
    pruneLibCmd;
    bibPrepassCmd;
    headerDataCmd;
    fromDbCmd
]

def main (args : List String) : IO UInt32 :=
  docGenCmd.validate args
