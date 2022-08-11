/-
Copyright (c) 2021 Henrik Böving. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Henrik Böving
-/
import Lean
import Lake
import DocGen4.Process
import DocGen4.Output.Base
import DocGen4.Output.Index
import DocGen4.Output.Module
import DocGen4.Output.NotFound
import DocGen4.Output.Find
import DocGen4.Output.SourceLinker
import DocGen4.Output.ToJson
import DocGen4.LeanInk.Process
import Std.Data.HashMap

namespace DocGen4

open Lean IO System Output Process Std

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
  let navbarHtml := ReaderT.run navbar config |>.toString
  let docGenStatic := #[
    ("style.css", styleCss),
    ("declaration-data.js", declarationDataCenterJs),
    ("nav.js", navJs),
    ("how-about.js", howAboutJs),
    ("search.js", searchJs),
    ("mathjax-config.js", mathjaxConfigJs),
    ("instances.js", instancesJs),
    ("importedBy.js", importedByJs),
    ("index.html", indexHtml),
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

def htmlOutputResults (baseConfig : SiteBaseContext) (result : AnalyzerResult) (ws : Lake.Workspace) (ink : Bool) : IO Unit := do
  let config : SiteContext := {
    result := result,
    sourceLinker := ←sourceLinker ws
    leanInkEnabled := ink
  }

  FS.createDirAll basePath
  FS.createDirAll declarationsBasePath

  -- Rendering the entire lean compiler takes time....
  --let sourceSearchPath := ((←Lean.findSysroot) / "src" / "lean") :: ws.root.srcDir :: ws.leanSrcPath
  let sourceSearchPath := ws.root.srcDir :: ws.leanSrcPath

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
        IO.println s!"Inking: {modName.toString}"
        -- path: 'basePath/src/module/components/till/last.html'
        -- The last component is the file name, however we are in src/ here so dont drop it this time
        let baseConfig := {baseConfig with depthToRoot := modName.components.length }
        Process.LeanInk.runInk inputPath |>.run config baseConfig

def getSimpleBaseContext (hierarchy : Hierarchy) : SiteBaseContext :=
  {
    depthToRoot := 0,
    currentName := none,
    hierarchy
  }

def htmlOutputIndex (baseConfig : SiteBaseContext) : IO Unit := do
  htmlOutputSetup baseConfig

  let mut index : JsonIndex := { }
  for entry in ←System.FilePath.readDir declarationsBasePath do
    if entry.fileName.startsWith "declaration-data-" && entry.fileName.endsWith ".bmp" then
      let fileContent ← FS.readFile entry.path
      let .ok jsonContent := Json.parse fileContent | unreachable!
      let .ok (module : JsonModule) := fromJson? jsonContent | unreachable!
      index := index.addModule module |>.run baseConfig

  let finalJson := toJson index
  -- The root JSON for find
  FS.writeFile (declarationsBasePath / "declaration-data.bmp") finalJson.compress

/--
The main entrypoint for outputting the documentation HTML based on an
`AnalyzerResult`.
-/
def htmlOutput (result : AnalyzerResult) (hierarchy : Hierarchy) (ws : Lake.Workspace) (ink : Bool) : IO Unit := do
  let baseConfig := getSimpleBaseContext hierarchy
  htmlOutputResults baseConfig result ws ink
  htmlOutputIndex baseConfig

end DocGen4

