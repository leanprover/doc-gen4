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
import DocGen4.LeanInk.Output
import Std.Data.HashMap

namespace DocGen4

open Lean IO System Output Process Std

def basePath := FilePath.mk "." / "build" / "doc"
def srcBasePath := basePath / "src"

def htmlOutputSetup (config : SiteBaseContext) : IO Unit := do
  let findBasePath := basePath / "find"

  -- Base structure
  FS.createDirAll basePath
  FS.createDirAll (basePath / "find")
  FS.createDirAll srcBasePath

  -- All the doc-gen static stuff
  let indexHtml := ReaderT.run index config |>.toString
  let notFoundHtml := ReaderT.run notFound config |>.toString
  let docGenStatic := #[
    ("style.css", styleCss),
    ("declaration-data.js", declarationDataCenterJs),
    ("nav.js", navJs),
    ("how-about.js", howAboutJs),
    ("search.js", searchJs),
    ("mathjax-config.js", mathjaxConfigJs),
    ("index.html", indexHtml),
    ("404.html", notFoundHtml)
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

def DocInfo.toJson (module : Name) (info : DocInfo) : HtmlM Json := do
  let name := info.getName.toString
  let doc := info.getDocString.getD ""
  let docLink ← declNameToLink info.getName
  let sourceLink ← getSourceUrl module info.getDeclarationRange
  pure $ Json.mkObj [("name", name), ("doc", doc), ("docLink", docLink), ("sourceLink", sourceLink)]

def Process.Module.toJson (module : Module) : HtmlM (Array Json) := do
    let mut jsonDecls := #[]
    for decl in filterMapDocInfo module.members do
      let json ← DocInfo.toJson module.name decl
      jsonDecls := jsonDecls.push json
    pure jsonDecls

def htmlOutputResults (baseConfig : SiteBaseContext) (result : AnalyzerResult) (ws : Lake.Workspace) (inkPath : Option System.FilePath) : IO Unit := do
  let config : SiteContext := {
    result := result,
    sourceLinker := ←sourceLinker ws
    leanInkEnabled := inkPath.isSome
  }

  FS.createDirAll basePath

  -- Rendering the entire lean compiler takes time....
  --let sourceSearchPath := ((←Lean.findSysroot) / "src" / "lean") :: ws.root.srcDir :: ws.leanSrcPath
  let sourceSearchPath := ws.root.srcDir :: ws.leanSrcPath

  let mut declMap := HashMap.empty
  for (_, mod) in result.moduleInfo.toArray do
    let topLevelMod := mod.name.getRoot
    let jsonDecls := Module.toJson mod |>.run config baseConfig
    let currentModDecls := declMap.findD topLevelMod #[]
    declMap := declMap.insert topLevelMod (currentModDecls ++ jsonDecls)

  for (topLevelMod, decls) in declMap.toList do
    FS.writeFile (basePath / s!"declaration-data-{topLevelMod}.bmp") (Json.arr decls).compress

  for (modName, module) in result.moduleInfo.toArray do
    let fileDir := moduleNameToDirectory basePath modName
    let filePath := moduleNameToFile basePath modName
    -- path: 'basePath/module/components/till/last.html'
    -- The last component is the file name, so we drop it from the depth to root.
    let baseConfig := { baseConfig with depthToRoot := modName.components.dropLast.length }
    let moduleHtml := moduleToHtml module |>.run config baseConfig
    FS.createDirAll $ fileDir
    FS.writeFile filePath moduleHtml.toString
    if let some inkPath := inkPath then
      if let some inputPath ← Lean.SearchPath.findModuleWithExt sourceSearchPath "lean" module.name then
        IO.println s!"Inking: {modName.toString}"
        -- path: 'basePath/src/module/components/till/last.html'
        -- The last component is the file name, however we are in src/ here so dont drop it this time
        let baseConfig := { baseConfig with depthToRoot := modName.components.length }
        let srcHtml ← LeanInk.moduleToHtml module inkPath inputPath |>.run config baseConfig
        let srcDir := moduleNameToDirectory srcBasePath modName
        let srcPath := moduleNameToFile srcBasePath modName
        FS.createDirAll srcDir
        FS.writeFile srcPath srcHtml.toString

def getSimpleBaseContext (hierarchy : Hierarchy) : SiteBaseContext :=
  {
    depthToRoot := 0,
    currentName := none,
    hierarchy
  }

def htmlOutputFinalize (baseConfig : SiteBaseContext) : IO Unit := do
  htmlOutputSetup baseConfig

  let mut topLevelModules := #[]
  for entry in ←System.FilePath.readDir basePath do
    if entry.fileName.startsWith "declaration-data-" && entry.fileName.endsWith ".bmp" then
      let module := entry.fileName.drop "declaration-data-".length |>.dropRight ".bmp".length
      topLevelModules := topLevelModules.push (Json.str module)

  -- The root JSON for find
  FS.writeFile (basePath / "declaration-data.bmp") (Json.arr topLevelModules).compress

/--
The main entrypoint for outputting the documentation HTML based on an
`AnalyzerResult`.
-/
def htmlOutput (result : AnalyzerResult) (hierarchy : Hierarchy) (ws : Lake.Workspace) (inkPath : Option System.FilePath) : IO Unit := do
  let baseConfig := getSimpleBaseContext hierarchy
  htmlOutputResults baseConfig result ws inkPath
  htmlOutputFinalize baseConfig

end DocGen4

