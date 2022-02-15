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
def getGithubBaseUrl : IO String := do
  let out ← IO.Process.output {cmd := "git", args := #["remote", "get-url", "origin"]}
  if out.exitCode != 0 then
    throw <| IO.userError <| "git exited with code " ++ toString out.exitCode
  let mut url := out.stdout.trimRight

  if url.startsWith "git@" then
    url := url.drop 15
    url := url.dropRight 4
    pure s!"https://github.com/{url}"
  else if url.endsWith ".git" then
    pure $ url.dropRight 4
  else
    pure url

def getCommit : IO String := do
  let out ← IO.Process.output {cmd := "git", args := #["rev-parse", "HEAD"]}
  if out.exitCode != 0 then
    throw <| IO.userError <| "git exited with code " ++ toString out.exitCode
  pure out.stdout.trimRight

def sourceLinker : IO (Name → Option DeclarationRange → String) := do
  let baseUrl ← getGithubBaseUrl
  let commit ← getCommit
  pure λ name range =>
    let parts := name.components.map Name.toString
    let path := (parts.intersperse "/").foldl (· ++ ·) ""
    let r := name.getRoot
    let basic := if r == `Lean ∨ r == `Init ∨ r == `Std then
      s!"https://github.com/leanprover/lean4/blob/{githash}/src/{path}.lean"
    else
      s!"{baseUrl}/blob/{commit}/{path}.lean"

    match range with
    | some range => s!"{basic}#L{range.pos.line}-L{range.endPos.line}"
    | none => basic

def htmlOutput (result : AnalyzerResult) : IO Unit := do
  let basePath := FilePath.mk "./build/doc/"
  let config := { depthToRoot := 0, result := result, currentName := none, sourceLinker := ←sourceLinker}
  FS.createDirAll basePath
  FS.createDirAll (basePath / "find")
  let indexHtml := ReaderT.run index config 
  let notFoundHtml := ReaderT.run notFound config

  let mut declList := #[]
  for (module, mod) in result.moduleInfo.toArray do
    for decl in filterMapDocInfo mod.members do
      let findDir := basePath / "find" / decl.getName.toString
      let findFile := (findDir / "index.html")
      -- path: 'basePath/find/decl.getName.toString'
      let config := { config with depthToRoot := 2 }
      let findHtml := ReaderT.run (findRedirectHtml decl.getName) config
      FS.createDirAll findDir
      FS.writeFile findFile findHtml.toString
      let obj := Json.mkObj [("name", decl.getName.toString), ("description", decl.getDocString.getD "")]
      declList := declList.push obj
  let json := Json.arr declList

  FS.writeFile (basePath / "searchable_data.bmp") json.compress
  FS.writeFile (basePath / "index.html") indexHtml.toString
  FS.writeFile (basePath / "style.css") styleCss
  FS.writeFile (basePath / "404.html") notFoundHtml.toString
  FS.writeFile (basePath / "nav.js") navJs
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

