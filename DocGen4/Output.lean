/-
Copyright (c) 2021 Henrik Böving. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Henrik Böving
-/
import Lean
import DocGen4.Process
import DocGen4.Output.Base
import DocGen4.Output.Index
import DocGen4.Output.Module
import DocGen4.Output.NotFound
import DocGen4.Output.Find
import DocGen4.Output.References
import DocGen4.Output.Pybtex
import DocGen4.Output.Bibtex
import DocGen4.Output.SourceLinker
import DocGen4.Output.Search
import DocGen4.Output.ToJson
import DocGen4.Output.FoundationalTypes
import Lean.Data.HashMap

namespace DocGen4

open Lean IO System Output Process

def collectBackrefs : IO (Array BackrefItem) := do
  let mut backrefs : Array BackrefItem := #[]
  for entry in ← System.FilePath.readDir declarationsBasePath do
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

def htmlOutputSetup (config : SiteBaseContext) : IO Unit := do
  let findBasePath := basePath / "find"

  -- Base structure
  FS.createDirAll basePath
  FS.createDirAll findBasePath
  FS.createDirAll srcBasePath
  FS.createDirAll declarationsBasePath

  -- All the doc-gen static stuff
  let indexHtml := ReaderT.run index config |>.toString
  let notFoundHtml := ReaderT.run notFound config |>.toString
  let foundationalTypesHtml := ReaderT.run foundationalTypes config |>.toString
  let navbarHtml := ReaderT.run navbar config |>.toString
  let searchHtml := ReaderT.run search config |>.toString
  let referencesHtml := ReaderT.run (references (← collectBackrefs)) config |>.toString
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
    ("references.html", referencesHtml)
  ]
  for (fileName, content) in docGenStatic do
    FS.writeFile (basePath / fileName) content

  let findHtml := ReaderT.run find { config with depthToRoot := 1 } |>.toString
  let findStatic := #[
    ("index.html", findHtml),
    ("find.js", findJs)
  ]
  for (fileName, content) in findStatic do
    FS.writeFile (findBasePath / fileName) content

def htmlOutputDeclarationDatas (result : AnalyzerResult) : HtmlT IO Unit := do
  for (_, mod) in result.moduleInfo.toArray do
    let jsonDecls ← Module.toJson mod
    FS.writeFile (declarationsBasePath / s!"declaration-data-{mod.name}.bmp") (toJson jsonDecls).compress

def htmlOutputResults (baseConfig : SiteBaseContext) (result : AnalyzerResult) (sourceUrl? : Option String) : IO Unit := do
  let config : SiteContext := {
    result := result
    sourceLinker := SourceLinker.sourceLinker sourceUrl?
    refsMap := .ofList (baseConfig.refs.map fun x => (x.citekey, x)).toList
  }

  FS.createDirAll basePath
  FS.createDirAll declarationsBasePath

  discard <| htmlOutputDeclarationDatas result |>.run {} config baseConfig

  for (modName, module) in result.moduleInfo.toArray do
    let fileDir := moduleNameToDirectory basePath modName
    let filePath := moduleNameToFile basePath modName
    -- path: 'basePath/module/components/till/last.html'
    -- The last component is the file name, so we drop it from the depth to root.
    let baseConfig := { baseConfig with
      depthToRoot := modName.components.dropLast.length
      currentName := some modName
    }
    let (moduleHtml, cfg) := moduleToHtml module |>.run {} config baseConfig
    if not cfg.errors.isEmpty then
      throw <| IO.userError s!"There are errors when generating '{filePath}': {cfg.errors}"
    FS.createDirAll fileDir
    FS.writeFile filePath moduleHtml.toString
    FS.writeFile (declarationsBasePath / s!"backrefs-{module.name}.json") (toString (toJson cfg.backrefs))

def getSimpleBaseContext (hierarchy : Hierarchy) : IO SiteBaseContext := do
  let contents ← FS.readFile (declarationsBasePath / "references.json") <|> (pure "[]")
  match Json.parse contents with
  | .error err =>
    throw <| IO.userError s!"Failed to parse 'references.json': {err}"
  | .ok jsonContent =>
    match fromJson? jsonContent with
    | .error err =>
      throw <| IO.userError s!"Failed to parse 'references.json': {err}"
    | .ok (refs : Array BibItem) =>
      return {
        depthToRoot := 0
        currentName := none
        hierarchy := hierarchy
        refs := refs
      }

def htmlOutputIndex (baseConfig : SiteBaseContext) : IO Unit := do
  htmlOutputSetup baseConfig

  let mut index : JsonIndex := {}
  for entry in ← System.FilePath.readDir declarationsBasePath do
    if entry.fileName.startsWith "declaration-data-" && entry.fileName.endsWith ".bmp" then
      let fileContent ← FS.readFile entry.path
      match Json.parse fileContent with
      | .error err =>
        throw <| IO.userError s!"failed to parse file '{entry.path}' as json: {err}"
      | .ok jsonContent =>
        match fromJson? jsonContent with
        | .error err =>
          throw <| IO.userError s!"failed to parse file '{entry.path}': {err}"
        | .ok (module : JsonModule) =>
          index := index.addModule module |>.run baseConfig

  let finalJson := toJson index
  -- The root JSON for find
  let declarationDir := basePath / "declarations"
  FS.createDirAll declarationDir
  FS.writeFile (declarationDir / "declaration-data.bmp") finalJson.compress

def headerDataOutput : IO Unit := do
  let mut headerIndex : JsonHeaderIndex := {}
  for entry in ← System.FilePath.readDir declarationsBasePath do
    if entry.fileName.startsWith "declaration-data-" && entry.fileName.endsWith ".bmp" then
      let fileContent ← FS.readFile entry.path
      let .ok jsonContent := Json.parse fileContent | unreachable!
      let .ok (module : JsonModule) := fromJson? jsonContent | unreachable!
      headerIndex := headerIndex.addModule module

  let finalHeaderJson := toJson headerIndex
  let declarationDir := basePath / "declarations"
  FS.createDirAll declarationDir
  FS.writeFile (declarationDir / "header-data.bmp") finalHeaderJson.compress

/--
The main entrypoint for outputting the documentation HTML based on an
`AnalyzerResult`.
-/
def htmlOutput (result : AnalyzerResult) (hierarchy : Hierarchy) (sourceUrl? : Option String) : IO Unit := do
  let baseConfig ← getSimpleBaseContext hierarchy
  htmlOutputResults baseConfig result sourceUrl?
  htmlOutputIndex baseConfig

end DocGen4
