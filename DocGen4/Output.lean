/-
Copyright (c) 2021 Henrik Böving. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Henrik Böving
-/
import Lean
import DocGen4.Process
import DocGen4.DB
import DocGen4.Output.Base
import DocGen4.Output.Index
import DocGen4.Output.Module
import DocGen4.Output.NotFound
import DocGen4.Output.Find
import DocGen4.Output.References
import DocGen4.Output.Bibtex
import DocGen4.Output.SourceLinker
import DocGen4.Output.Search
import DocGen4.Output.Tactics
import DocGen4.Output.ToJson
import DocGen4.Output.FoundationalTypes
import DocGen4.Helpers

namespace DocGen4

open Lean IO System Output Process DB

def collectBackrefs (buildDir : System.FilePath) : IO (Array BackrefItem) := do
  let mut backrefs : Array BackrefItem := #[]
  for entry in ← System.FilePath.readDir (declarationsBasePath buildDir) do
    if entry.fileName.startsWith "backrefs-" && entry.fileName.endsWith ".json" then
      let fileContent ← FS.readFile entry.path
      match Json.parse fileContent with
      | .error err =>
        throw <| IO.userError s!"failed to parse file '{entry.path}' as json: {err}"
      | .ok jsonContent =>
        match fromJson? jsonContent with
        | .error err =>
          throw <| IO.userError s!"failed to parse file '{entry.path}': {err}"
        | .ok (arr : Array BackrefItem) => backrefs := backrefs ++ arr
  return backrefs

def htmlOutputSetup (config : SiteBaseContext) (tacticInfo : Array (Process.TacticInfo Html)) : IO Unit := do
  let findBasePath (buildDir : System.FilePath) := basePath buildDir / "find"

  -- Base structure
  FS.createDirAll <| basePath config.buildDir
  FS.createDirAll <| findBasePath config.buildDir
  FS.createDirAll <| srcBasePath config.buildDir
  FS.createDirAll <| declarationsBasePath config.buildDir

  -- All the doc-gen static stuff
  let indexHtml := ReaderT.run index config |>.toString
  let notFoundHtml := ReaderT.run notFound config |>.toString
  let foundationalTypesHtml := ReaderT.run foundationalTypes config |>.toString
  let navbarHtml := ReaderT.run navbar config |>.toString
  let searchHtml := ReaderT.run search config |>.toString
  let referencesHtml := ReaderT.run (references (← collectBackrefs config.buildDir)) config |>.toString
  let tacticsHtml := ReaderT.run (tactics tacticInfo) config |>.toString
  let docGenStatic := #[
    ("style.css", styleCss),
    ("favicon.svg", faviconSvg),
    ("declaration-data.js", declarationDataCenterJs),
    ("color-scheme.js", colorSchemeJs),
    ("nav.js", navJs),
    ("jump-src.js", jumpSrcJs),
    ("expand-nav.js", expandNavJs),
    ("how-about.js", howAboutJs),
    ("search.html", searchHtml),
    ("search.js", searchJs),
    ("mathjax-config.js", mathjaxConfigJs),
    ("instances.js", instancesJs),
    ("importedBy.js", importedByJs),
    ("index.html", indexHtml),
    ("foundational_types.html", foundationalTypesHtml),
    ("404.html", notFoundHtml),
    ("navbar.html", navbarHtml),
    ("references.html", referencesHtml),
    ("tactics.html", tacticsHtml),
  ]
  for (fileName, content) in docGenStatic do
    FS.writeFile (basePath config.buildDir / fileName) content

  let findHtml := ReaderT.run find { config with depthToRoot := 1 } |>.toString
  let findStatic := #[
    ("index.html", findHtml),
    ("find.js", findJs)
  ]
  for (fileName, content) in findStatic do
    FS.writeFile (findBasePath config.buildDir / fileName) content

/-- Custom source linker type: given an optional source URL and module name, returns a function from declaration range to URL -/
abbrev SourceLinkerFn := Option String → Name → Option DeclarationRange → String

/-- Generate HTML for all modules in parallel.
    Each task loads its module from DB, renders HTML, and writes output files.
    The linking context provides cross-module linking without loading all module data upfront.
    When `targetModules` is provided, only those modules are rendered (but linking uses all modules). -/
def htmlOutputResultsParallel (baseConfig : SiteBaseContext) (dbPath : System.FilePath)
    (linkCtx : LinkingContext)
    (targetModules : Array Name := linkCtx.moduleNames)
    (sourceLinker? : Option SourceLinkerFn := none)
    (declarationDecorator? : Option DeclarationDecoratorFn := none) : IO (Array System.FilePath × Array JsonModule) := do
  FS.createDirAll <| basePath baseConfig.buildDir
  FS.createDirAll <| declarationsBasePath baseConfig.buildDir

  let chunkSize := if targetModules.size > 20 then targetModules.size / 20 else 1
  -- Spawn about 20 tasks
  let tasks ← (chunksOf targetModules chunkSize).mapM fun mods => IO.asTask do
    -- Each task opens its own DB connection (SQLite handles concurrent readers well)
    let db ← DB.openForReading dbPath builtinDocstringValues
    mods.mapM fun modName => do
      let module ← db.loadModule modName
      let containedNames ← db.getContainedNames modName

      -- Build a minimal AnalyzerResult with just this module's info
      let result : AnalyzerResult := {
        name2ModIdx := linkCtx.name2ModIdx
        moduleNames := linkCtx.moduleNames
        moduleInfo := ({} : Std.HashMap Name Process.Module).insert modName module
        containedNames
      }

      let config : SiteContext := {
        result := result
        sourceLinker := (sourceLinker?.getD SourceLinker.sourceLinker) none
        refsMap := Std.HashMap.emptyWithCapacity baseConfig.refs.size |>.insertMany (baseConfig.refs.iter.map fun x => (x.citekey, x))
        declarationDecorator := declarationDecorator?.getD defaultDeclarationDecorator
      }

      -- path: 'basePath/module/components/till/last.html'
      -- The last component is the file name, so we drop it from the depth to root.
      let moduleConfig := { baseConfig with
        depthToRoot := modName.components.dropLast.length
        currentName := some modName
      }
      let (moduleHtml, cfg) := moduleToHtml module |>.run {} config moduleConfig
      if not cfg.errors.isEmpty then
        throw <| IO.userError s!"There are errors when generating HTML for '{modName}': {cfg.errors}"

      -- Write HTML file
      let relFilePath := basePathComponent / moduleNameToFile modName
      let filePath := baseConfig.buildDir / relFilePath
      if let .some d := filePath.parent then
        FS.createDirAll d
      FS.writeFile filePath moduleHtml.toString

      -- Write backrefs JSON
      FS.writeFile (declarationsBasePath baseConfig.buildDir / s!"backrefs-{module.name}.json")
        (toString (toJson cfg.backrefs))

      -- Generate declaration data JSON for search
      let (jsonModule, _) := moduleToJsonModule module |>.run {} config baseConfig
      FS.writeFile (declarationsBasePath baseConfig.buildDir / s!"declaration-data-{module.name}.bmp")
        (ToJson.toJson jsonModule).compress

      return (relFilePath, jsonModule)

  -- Wait for all tasks and collect output paths and modules
  let mut outputs := #[]
  let mut jsonModules := #[]
  for task in tasks do
    match (← IO.wait task) with
    | .ok results =>
      for (path, jsonMod) in results do
        outputs := outputs.push path
        jsonModules := jsonModules.push jsonMod
    | .error e => throw e
  return (outputs, jsonModules)

def getSimpleBaseContext (buildDir : System.FilePath) (hierarchy : Hierarchy) :
    IO SiteBaseContext := do
  let contents ← FS.readFile (declarationsBasePath buildDir / "references.json") <|> (pure "[]")
  match Json.parse contents with
  | .error err =>
    throw <| IO.userError s!"Failed to parse 'references.json': {err}"
  | .ok jsonContent =>
    match fromJson? jsonContent with
    | .error err =>
      throw <| IO.userError s!"Failed to parse 'references.json': {err}"
    | .ok (refs : Array BibItem) =>
      return {
        buildDir := buildDir
        depthToRoot := 0
        currentName := none
        hierarchy := hierarchy
        refs := refs
      }

def htmlOutputIndex (baseConfig : SiteBaseContext) (modules : Array JsonModule) (tacticInfo : Array (Process.TacticInfo Html)) : IO Unit := do
  htmlOutputSetup baseConfig tacticInfo

  -- Build a set of module names we just generated (already in memory)
  let freshModuleNames : Std.HashSet String := modules.foldl (init := {}) fun s m => s.insert m.name

  -- Load per-module data from disk for modules NOT in the current task set.
  -- This enables incremental builds: prior runs wrote declaration-data-{module}.bmp files,
  -- and we merge them so the unified search index covers all modules.
  let mut diskModules : Array JsonModule := #[]
  for entry in ← System.FilePath.readDir (declarationsBasePath baseConfig.buildDir) do
    if entry.fileName.startsWith "declaration-data-" && entry.fileName.endsWith ".bmp" then
      -- Extract module name from filename: "declaration-data-Foo.Bar.bmp" -> "Foo.Bar"
      let modName := entry.fileName.drop "declaration-data-".length |>.dropEnd ".bmp".length |>.toString
      if freshModuleNames.contains modName then continue
      let fileContent ← FS.readFile entry.path
      match Json.parse fileContent with
      | .error _ => continue
      | .ok jsonContent =>
        match fromJson? jsonContent with
        | .error _ => continue
        | .ok (module : JsonModule) =>
          diskModules := diskModules.push module

  let allModules := modules ++ diskModules
  let mut index : JsonIndex := {}
  for module in allModules do
    index := index.addModule module |>.run baseConfig

  let finalJson := toJson index
  -- The root JSON for find
  let declarationDir := basePath  baseConfig.buildDir / "declarations"
  FS.createDirAll declarationDir
  FS.writeFile (declarationDir / "declaration-data.bmp") finalJson.compress

def headerDataOutput (buildDir : System.FilePath) : IO Unit := do
  let mut headerIndex : JsonHeaderIndex := {}
  for entry in ← System.FilePath.readDir (declarationsBasePath buildDir) do
    if entry.fileName.startsWith "declaration-data-" && entry.fileName.endsWith ".bmp" then
      let fileContent ← FS.readFile entry.path
      let jsonContent ←
        match Json.parse fileContent with
        | .ok c => pure c
        | .error e => throw <| IO.userError s!"failed to parse JSON in {entry.path}: {e}"
      let (module : JsonModule) ←
        match fromJson? jsonContent with
        | .ok v => pure v
        | .error e =>
          throw <| IO.userError s!"failed to deserialize JsonModule from {entry.path}: {e}"
      headerIndex := headerIndex.addModule module

  let finalHeaderJson := toJson headerIndex
  let declarationDir := basePath buildDir / "declarations"
  FS.createDirAll declarationDir
  FS.writeFile (declarationDir / "header-data.bmp") finalHeaderJson.compress

/-- Convert HTML file path to module name: doc/A/B/C.html -> `A.B.C -/
def htmlPathToModuleName (docDir : System.FilePath) (htmlPath : System.FilePath) : Option Name :=
  -- Get relative path from doc directory
  let docDirStr := docDir.toString
  let htmlPathStr := htmlPath.toString
  -- Strip the doc directory prefix (handle both with and without trailing separator)
  let relPath? :=
    if htmlPathStr.startsWith (docDirStr ++ "/") then
      some (htmlPathStr.drop (docDirStr.length + 1))
    else if htmlPathStr.startsWith docDirStr then
      some (htmlPathStr.drop docDirStr.length)
    else
      none
  relPath?.bind fun relPath =>
    -- Remove .html extension
    if relPath.endsWith ".html" then
      let withoutExt := relPath.dropEnd 5
      -- Convert path separators to dots
      let name := withoutExt.replace "/" "." |>.replace "\\" "."
      some name.toName
    else
      none

/-- Scan for existing module HTML files under docDir -/
partial def scanModuleHtmlFiles (docDir : System.FilePath) : IO (Array Name) := do
  -- Files/directories to skip (not module HTML files)
  let skipFiles := ["index.html", "404.html", "navbar.html", "search.html",
                    "foundational_types.html", "references.html", "tactics.html"]
  let skipDirs := ["find", "declarations", "src"]

  let rec scanDir (dir : System.FilePath) : IO (Array Name) := do
    let mut result := #[]
    if !(← dir.pathExists) then return result
    for entry in ← System.FilePath.readDir dir do
      let entryPath := entry.root / entry.fileName
      if ← entryPath.isDir then
        -- Skip special directories
        if skipDirs.contains entry.fileName then continue
        result := result ++ (← scanDir entryPath)
      else if entry.fileName.endsWith ".html" then
        -- Skip special files
        if skipFiles.contains entry.fileName then continue
        -- Convert file path to module name
        if let some modName := htmlPathToModuleName docDir entryPath then
          result := result.push modName
    return result

  scanDir docDir

/-- Rebuild navbar.html by scanning existing HTML files on disk.
    This enables incremental builds where subsequent builds include modules from previous builds. -/
def updateNavbarFromDisk (buildDir : System.FilePath) : IO Unit := do
  let docDir := basePath buildDir
  -- Scan for all existing module HTML files
  let existingModules ← scanModuleHtmlFiles docDir
  -- Add `references` pseudo-module for navbar
  let allModules := existingModules.push `references
  -- Build hierarchy from all found modules
  let hierarchy := Hierarchy.fromArray allModules
  -- Load references for base context
  let contents ← FS.readFile (declarationsBasePath buildDir / "references.json") <|> (pure "[]")
  let refs : Array BibItem ← match Json.parse contents with
    | .error _ => pure #[]
    | .ok jsonContent =>
      match fromJson? jsonContent with
      | .error _ => pure #[]
      | .ok refs => pure refs
  let baseConfig : SiteBaseContext := {
    buildDir := buildDir
    depthToRoot := 0
    currentName := none
    hierarchy := hierarchy
    refs := refs
  }
  -- Regenerate navbar
  let navbarHtml := ReaderT.run navbar baseConfig |>.toString
  FS.writeFile (docDir / "navbar.html") navbarHtml

end DocGen4
