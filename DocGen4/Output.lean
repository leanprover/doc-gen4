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

def htmlOutput (result : AnalyzerResult) (root : String) : IO Unit := do
  let config := { root := root, result := result, currentName := none, sourceLinker := ←sourceLinker}
  let basePath := FilePath.mk "./build/doc/"
  let indexHtml := ReaderT.run index config 
  let notFoundHtml := ReaderT.run notFound config
  FS.createDirAll basePath
  FS.writeFile (basePath / "index.html") indexHtml.toString
  FS.writeFile (basePath / "style.css") styleCss
  FS.writeFile (basePath / "404.html") notFoundHtml.toString
  FS.writeFile (basePath / "nav.js") navJs
  for (module, content) in result.moduleInfo.toArray do
    let moduleHtml := ReaderT.run (moduleToHtml content) config
    let path := moduleNameToFile basePath module
    FS.createDirAll $ moduleNameToDirectory basePath module
    FS.writeFile path moduleHtml.toString

end DocGen4

