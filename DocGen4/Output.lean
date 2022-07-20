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

def htmlOutputSetup (config : SiteBaseContext) : IO Unit := do
  let basePath := FilePath.mk "." / "build" / "doc"
  let srcBasePath := basePath / "src"
  let declarationDataPath := basePath / "declaration-data.bmp"

  -- Base structure
  FS.createDirAll basePath
  FS.createDirAll (basePath / "find")
  FS.createDirAll srcBasePath

  -- The three HTML files we always need
  let indexHtml := ReaderT.run index config
  FS.writeFile (basePath / "index.html") indexHtml.toString

  let notFoundHtml := ReaderT.run notFound config
  FS.writeFile (basePath / "404.html") notFoundHtml.toString

  let findHtml := ReaderT.run find { config with depthToRoot := 1 }
  FS.writeFile (basePath / "find" / "index.html") findHtml.toString

  -- The root JSON for find
  let topLevelModules := config.hierarchy.getChildren.toArray.map (Json.str ∘ toString ∘ Prod.fst)
  FS.writeFile declarationDataPath (Json.arr topLevelModules).compress

  -- All the static stuff
  FS.writeFile (basePath / "style.css") styleCss
  FS.writeFile (basePath / "declaration-data.js") declarationDataCenterJs
  FS.writeFile (basePath / "nav.js") navJs
  FS.writeFile (basePath / "find" / "find.js") findJs
  FS.writeFile (basePath / "how-about.js") howAboutJs
  FS.writeFile (basePath / "search.js") searchJs
  FS.writeFile (basePath / "mathjax-config.js") mathjaxConfigJs

  -- All the static stuff for LeanInk
  FS.writeFile (srcBasePath / "alectryon.css") alectryonCss
  FS.writeFile (srcBasePath / "alectryon.js") alectryonJs
  FS.writeFile (srcBasePath / "docutils_basic.css") docUtilsCss
  FS.writeFile (srcBasePath / "pygments.css") pygmentsCss


def htmlOutputResults (baseConfig : SiteBaseContext) (result : AnalyzerResult) (ws : Lake.Workspace) (leanHash: String) (inkPath : Option System.FilePath) : IO Unit := do
  let config : SiteContext := {
    result := result,
    sourceLinker := ←sourceLinker ws leanHash
    leanInkEnabled := inkPath.isSome
  }
  let basePath := FilePath.mk "." / "build" / "doc"
  let srcBasePath := basePath / "src"
  -- Rendering the entire lean compiler takes time....
  --let sourceSearchPath := ((←Lean.findSysroot) / "src" / "lean") :: ws.root.srcDir :: ws.leanSrcPath
  let sourceSearchPath := ws.root.srcDir :: ws.leanSrcPath

  let mut declMap := HashMap.empty
  for (_, mod) in result.moduleInfo.toArray do
    let topLevelMod := mod.name.getRoot
    let mut jsonDecls := #[]
    for decl in filterMapDocInfo mod.members do
      let name := decl.getName.toString
      let doc := decl.getDocString.getD ""
      let docLink := declNameToLink decl.getName |>.run config baseConfig
      IO.println s!"DOC: {docLink}"
      let sourceLink := getSourceUrl mod.name decl.getDeclarationRange |>.run config baseConfig
      let json := Json.mkObj [("name", name), ("doc", doc), ("docLink", docLink), ("sourceLink", sourceLink)]
      jsonDecls := jsonDecls.push json
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

/--
The main entrypoint for outputting the documentation HTML based on an
`AnalyzerResult`.
-/
def htmlOutput (result : AnalyzerResult) (hierarchy : Hierarchy) (ws : Lake.Workspace) (leanHash: String) (inkPath : Option System.FilePath) : IO Unit := do
  let baseConfig := getSimpleBaseContext hierarchy
  htmlOutputSetup baseConfig
  htmlOutputResults baseConfig result ws leanHash inkPath

end DocGen4

