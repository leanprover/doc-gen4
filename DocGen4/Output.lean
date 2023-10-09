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
import DocGen4.Output.SourceLinker
import DocGen4.Output.Search
import DocGen4.Output.ToJson
import DocGen4.Output.FoundationalTypes
import DocGen4.LeanInk.Process
import Lean.Data.HashMap

namespace DocGen4

open Lean IO System Output Process

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
  let docGenStatic := #[
    ("style.css", styleCss),
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
    ("navbar.html", navbarHtml)
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

  let alectryonStatic := #[
    ("alectryon.css", alectryonCss),
    ("alectryon.js", alectryonJs),
    ("docutils_basic.css", docUtilsCss),
    ("pygments.css", pygmentsCss)
  ]

  for (fileName, content) in alectryonStatic do
    FS.writeFile (srcBasePath / fileName) content

def htmlOutputDeclarationDatas (result : AnalyzerResult) : HtmlT IO Unit := do
  for (_, mod) in result.moduleInfo.toArray do
    let jsonDecls ← Module.toJson mod
    FS.writeFile (declarationsBasePath / s!"declaration-data-{mod.name}.bmp") (toJson jsonDecls).compress

def htmlOutputResults (baseConfig : SiteBaseContext) (result : AnalyzerResult) (gitUrl? : Option String) (ink : Bool) : IO Unit := do
  let config : SiteContext := {
    result := result,
    sourceLinker := ← SourceLinker.sourceLinker gitUrl?
    leanInkEnabled := ink
  }

  FS.createDirAll basePath
  FS.createDirAll declarationsBasePath

  let some p := (← IO.getEnv "LEAN_SRC_PATH") | throw <| IO.userError "LEAN_SRC_PATH not found in env"
  let sourceSearchPath := System.SearchPath.parse p

  discard <| htmlOutputDeclarationDatas result |>.run config baseConfig

  for (modName, module) in result.moduleInfo.toArray do
    let fileDir := moduleNameToDirectory basePath modName
    let filePath := moduleNameToFile basePath modName
    -- path: 'basePath/module/components/till/last.html'
    -- The last component is the file name, so we drop it from the depth to root.
    let baseConfig := { baseConfig with
      depthToRoot := modName.components.dropLast.length
      currentName := some modName
    }
    let moduleHtml := moduleToHtml module |>.run config baseConfig
    FS.createDirAll fileDir
    FS.writeFile filePath moduleHtml.toString
    if ink then
      if let some inputPath ← Lean.SearchPath.findModuleWithExt sourceSearchPath "lean" module.name then
        -- path: 'basePath/src/module/components/till/last.html'
        -- The last component is the file name, however we are in src/ here so dont drop it this time
        let baseConfig := {baseConfig with depthToRoot := modName.components.length }
        Process.LeanInk.runInk inputPath |>.run config baseConfig

def getSimpleBaseContext (hierarchy : Hierarchy) : IO SiteBaseContext := do
  return {
    depthToRoot := 0,
    currentName := none,
    hierarchy
  }

def htmlOutputIndex (baseConfig : SiteBaseContext) : IO Unit := do
  htmlOutputSetup baseConfig

  let mut index : JsonIndex := {}
  let mut headerIndex : JsonHeaderIndex := {}
  for entry in ← System.FilePath.readDir declarationsBasePath do
    if entry.fileName.startsWith "declaration-data-" && entry.fileName.endsWith ".bmp" then
      let fileContent ← FS.readFile entry.path
      let .ok jsonContent := Json.parse fileContent | unreachable!
      let .ok (module : JsonModule) := fromJson? jsonContent | unreachable!
      index := index.addModule module |>.run baseConfig
      headerIndex := headerIndex.addModule module

  let finalJson := toJson index
  let finalHeaderJson := toJson headerIndex
  -- The root JSON for find
  FS.writeFile (declarationsBasePath / "declaration-data.bmp") finalJson.compress
  FS.writeFile (declarationsBasePath / "header-data.bmp") finalHeaderJson.compress

/--
The main entrypoint for outputting the documentation HTML based on an
`AnalyzerResult`.
-/
def htmlOutput (result : AnalyzerResult) (hierarchy : Hierarchy) (gitUrl? : Option String) (ink : Bool) : IO Unit := do
  let baseConfig ← getSimpleBaseContext hierarchy
  htmlOutputResults baseConfig result gitUrl? ink
  htmlOutputIndex baseConfig

end DocGen4

