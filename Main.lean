import DocGen4
import Lean
import Cli

open DocGen4 DocGen4.DB DocGen4.Output Lean Cli

/-- Reads the package directories from the file that the `--packages` flag names, if it is given. -/
def packageDirsFlag? (p : Parsed) : IO (Option PackageDirs) :=
  (p.flag? "packages").mapM fun file => do
    let json ← IO.ofExcept <| Json.parse (← IO.FS.readFile (file.as! String))
    IO.ofExcept (fromJson? json : Except String PackageDirs)

def runHeaderDataCmd (p : Parsed) : IO UInt32 := do
  let buildDir := match p.flag? "build" with
    | some dir => dir.as! String
    | none => ".lake/build"
  let dbPath := p.positionalArg! "db" |>.as! String
  let db ← openForReading dbPath builtinDocstringValues
  let linkedModules := Std.HashSet.ofArray (← db.getModuleNames (← packageDirsFlag? p))
  headerDataOutput buildDir linkedModules
  return 0

/-- Returns the location of a core module's source file. Fails for modules outside core. -/
def coreModuleSource (module : Name) : IO ModuleSource :=
  match Output.SourceLinker.coreSourcePath? module with
  | some path => pure { package? := none, path }
  | none => throw <| IO.userError s!"'{module}' is not a core module"

def runSingleCmd (p : Parsed) : IO UInt32 := do
  let buildDir := match p.flag? "build" with
    | some dir => dir.as! String
    | none => ".lake/build"
  let dbFile := p.positionalArg! "db" |>.as! String
  let module := p.positionalArg! "module" |>.as! String |> String.toName
  let sourceUri := p.positionalArg! "sourceUri" |>.as! String
  let package? := (p.flag? "package").map (·.as! String)
  let sourcePath? := (p.flag? "source-path").map (·.as! String)
  let source : ModuleSource ← match package?, sourcePath?, p.hasFlag "core" with
    | some package, some path, false => pure { package? := some package, path }
    | none, none, true => coreModuleSource module
    | _, _, _ =>
      throw <| IO.userError "either `--package` with `--source-path`, or `--core` alone, is required"
  let doc ← load <| .analyzeConcreteModules #[module]
  updateModuleDb builtinDocstringValues doc buildDir dbFile (some sourceUri) fun _ => pure source
  return 0

def runGenCoreCmd (p : Parsed) : IO UInt32 := do
  let buildDir := match p.flag? "build" with
    | some dir => dir.as! String
    | none => ".lake/build"
  let dbFile := p.positionalArg! "db" |>.as! String
  let module := p.positionalArg! "module" |>.as! String |> String.toName
  let doc ← load <| .analyzePrefixModules module
  updateModuleDb builtinDocstringValues doc buildDir dbFile none coreModuleSource
  return 0

def runDocGenCmd (_p : Parsed) : IO UInt32 := do
  IO.println "You most likely want to use me via Lake now, check my README on Github on how to:"
  IO.println "https://github.com/leanprover/doc-gen4"
  return 0

/-- A source linker that uses URLs from the database, falling back to core module URLs -/
def dbSourceLinker (sourceUrls : Std.HashMap Name String) (_gitUrl? : Option String) (module : Name) : Option DeclarationRange → String :=
  if let some path := Output.SourceLinker.coreSourcePath? module then
    Output.SourceLinker.mkGithubSourceLinker s!"https://github.com/leanprover/lean4/blob/{Lean.githash}/src/{path}"
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
  let packageDirs? ← packageDirsFlag? p
  let moduleRoots := (p.variableArgsAs! String).map String.toName

  -- Flush WAL so the database file is self-contained for concurrent readers
  walCheckpoint dbPath

  -- Load linking context (names of modules whose source files exist, source URLs, declaration
  -- locations)
  let db ← openForReading dbPath builtinDocstringValues
  let linkCtx ← db.loadLinkingContext packageDirs?

  -- Determine which modules to generate HTML for
  let targetModules ←
    if moduleRoots.isEmpty then
      pure linkCtx.moduleNames
    else
      db.getTransitiveImports moduleRoots
  let linkedModules := Std.HashSet.ofArray linkCtx.moduleNames

  -- If a target is outside the linking context, then the location was recorded incorrectly.
  if let some packageDirs := packageDirs? then
    let sources := Std.HashMap.ofList (← db.getModules).toList
    for mod in targetModules do
      unless linkedModules.contains mod do
        let reason := match sources[mod]? with
          | none => "it is not found in the database"
          | some source =>
            match source.package? with
            | none => "it has no package"
            | some package =>
              match packageDirs[package]? with
              | none => s!"its package '{package}' is not in the package map"
              | some dir => s!"its source file '{dir / source.path}' was not found"
        IO.eprintln s!"warning: HTML for module '{mod}' is generated, but nothing links to it: {reason}"

  let baseConfig ← getSimpleBaseContext buildDir (Hierarchy.fromArray targetModules)
  -- Add `references` pseudo-module to hierarchy only when bibliography data exists
  let hierarchy := Hierarchy.fromArray
    (if baseConfig.refs.isEmpty then targetModules else targetModules.push `references)
  let baseConfig := { baseConfig with hierarchy }

  -- Parallel HTML generation
  let (outputs, jsonModules) ← htmlOutputResultsParallel baseConfig dbPath linkCtx targetModules (sourceLinker? := some (dbSourceLinker linkCtx.sourceUrls))

  -- Load the tactics of the linking context's modules from DB in sorted order and convert markdown
  -- docstrings to HTML
  let allTacticsRaw := (← db.loadAllTactics).filter (linkedModules.contains ·.definingModule)
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
  htmlOutputIndex baseConfig jsonModules allTactics linkedModules

  -- Update navbar to include the linking context's modules that have a page on disk
  updateNavbarFromDisk buildDir linkedModules
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
    p, package : String; "The `baseName` in Lake of the package that contains the module. Requires `--source-path`."
    s, "source-path" : String; "Path of the module's source file relative to the package directory, with `/` separators."
    c, core; "The module is a core module (Init, Std, Lake, Lean). Replaces `--package` and `--source-path`."

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
    p, packages : String; "JSON file that maps the `baseName` in Lake of each package to its directory. When given, a package's module is included only when the package is listed and the module's source file exists."

  ARGS:
    db : String; "Path to the SQLite database"
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
    p, packages : String; "JSON file that maps the `baseName` in Lake of each package to its directory. When given, a package's module is included only when the package is listed and the module's source file exists."

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
    bibPrepassCmd;
    headerDataCmd;
    fromDbCmd
]

def main (args : List String) : IO UInt32 :=
  docGenCmd.validate args
