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
import DocGen4.Output.Semantic

namespace DocGen4

open Lean Std IO System Output


/-
Three link types from git supported:
- https://github.com/org/repo
- https://github.com/org/repo.git
- git@github.com:org/repo.git
TODO: This function is quite brittle and very github specific, we can
probably do better.
-/
def getGithubBaseUrl (gitUrl : String) : String := Id.run do
  let mut url := gitUrl
  if url.startsWith "git@" then
    url := url.drop 15
    url := url.dropRight 4
    pure s!"https://github.com/{url}"
  else if url.endsWith ".git" then
    pure $ url.dropRight 4
  else
    pure url

def getProjectGithubUrl : IO String := do
  let out ← IO.Process.output {cmd := "git", args := #["remote", "get-url", "origin"]}
  if out.exitCode != 0 then
    throw <| IO.userError <| "git exited with code " ++ toString out.exitCode
  pure out.stdout.trimRight

def getProjectCommit : IO String := do
  let out ← IO.Process.output {cmd := "git", args := #["rev-parse", "HEAD"]}
  if out.exitCode != 0 then
    throw <| IO.userError <| "git exited with code " ++ toString out.exitCode
  pure out.stdout.trimRight

def sourceLinker (ws : Lake.Workspace) (leanHash : String): IO (Name → Option DeclarationRange → String) := do
  -- Compute a map from package names to source URL
  let mut gitMap := Std.mkHashMap
  let projectBaseUrl := getGithubBaseUrl (←getProjectGithubUrl)
  let projectCommit ← getProjectCommit
  gitMap := gitMap.insert ws.root.name (projectBaseUrl, projectCommit)
  for pkg in ws.packageArray do
    for dep in pkg.dependencies do
      let value := match dep.src with
        | Lake.Source.git url commit => (getGithubBaseUrl url, commit)
        -- TODO: What do we do here if linking a source is not possible?
        | _ => ("https://example.com", "master")
      gitMap := gitMap.insert dep.name value

  pure $ λ module range =>
    let parts := module.components.map Name.toString
    let path := (parts.intersperse "/").foldl (· ++ ·) ""
    let root := module.getRoot
    let basic := if root == `Lean ∨ root == `Init ∨ root == `Std then
      s!"https://github.com/leanprover/lean4/blob/{leanHash}/src/{path}.lean"
    else
      match ws.packageForModule? module with
      | some pkg =>
        let (baseUrl, commit) := gitMap.find! pkg.name
        s!"{baseUrl}/blob/{commit}/{path}.lean"
      | none => "https://example.com"

    match range with
    | some range => s!"{basic}#L{range.pos.line}-L{range.endPos.line}"
    | none => basic

def htmlOutput (result : AnalyzerResult) (ws : Lake.Workspace) (leanHash: String) : IO Unit := do
  let config : SiteContext := { depthToRoot := 0, result := result, currentName := none, sourceLinker := ←sourceLinker ws leanHash}
  let basePath := FilePath.mk "." / "build" / "doc"
  let indexHtml := ReaderT.run index config 
  let findHtml := ReaderT.run find { config with depthToRoot := 1 }
  let notFoundHtml := ReaderT.run notFound config
  FS.createDirAll basePath
  FS.createDirAll (basePath / "find")
  FS.createDirAll (basePath / "semantic")

  let mut declList := #[]
  for (_, mod) in result.moduleInfo.toArray do
    for decl in filterMapDocInfo mod.members do
      let name := decl.getName.toString
      let config := { config with depthToRoot := 0 }
      let doc := decl.getDocString.getD ""
      let root := Id.run <| ReaderT.run (getRoot) config
      let link :=  root ++ s!"../semantic/{decl.getName.hash}.xml#"
      let docLink := Id.run <| ReaderT.run (declNameToLink decl.getName) config
      let sourceLink := Id.run <| ReaderT.run (getSourceUrl mod.name decl.getDeclarationRange) config
      let obj := Json.mkObj [("name", name), ("doc", doc), ("link", link), ("docLink", docLink), ("sourceLink", sourceLink)]
      declList := declList.push obj
      let xml := toString <| Id.run <| ReaderT.run (semanticXml decl) config 
      FS.writeFile (basePath / "semantic" / s!"{decl.getName.hash}.xml") xml
  let json := Json.arr declList

  FS.writeFile (basePath / "semantic" / "docgen4.xml") <| toString <| Id.run <| ReaderT.run schemaXml config 

  FS.writeFile (basePath / "index.html") indexHtml.toString
  FS.writeFile (basePath / "404.html") notFoundHtml.toString
  FS.writeFile (basePath / "find" / "index.html") findHtml.toString

  FS.writeFile (basePath / "style.css") styleCss

  let declarationDataPath := basePath / "declaration-data.bmp"
  FS.writeFile declarationDataPath json.compress
  FS.writeFile (basePath / "declaration-data.timestamp") <| toString (←declarationDataPath.metadata).modified.sec

  FS.writeFile (basePath / "declaration-data.js") declarationDataCenterJs
  FS.writeFile (basePath / "nav.js") navJs
  FS.writeFile (basePath / "find" / "find.js") findJs
  FS.writeFile (basePath / "how-about.js") howAboutJs
  FS.writeFile (basePath / "search.js") searchJs
  FS.writeFile (basePath / "mathjax-config.js") mathjaxConfigJs

  for (module, content) in result.moduleInfo.toArray do
    let fileDir := moduleNameToDirectory basePath module
    let filePath := moduleNameToFile basePath module
    -- path: 'basePath/module/components/till/last.html'
    -- The last component is the file name, so we drop it from the depth to root.
    let config := { config with depthToRoot := module.components.dropLast.length }
    let moduleHtml := ReaderT.run (moduleToHtml content) config
    FS.createDirAll $ fileDir
    FS.writeFile filePath moduleHtml.toString

end DocGen4

