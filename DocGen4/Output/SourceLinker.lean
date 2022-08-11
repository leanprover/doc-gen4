/-
Copyright (c) 2022 Henrik Böving. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Henrik Böving
-/
import Lean
import Lake

namespace DocGen4.Output

open Lean

/--
Turns a Github git remote URL into an HTTPS Github URL.
Three link types from git supported:
- https://github.com/org/repo
- https://github.com/org/repo.git
- git@github.com:org/repo.git

TODO: This function is quite brittle and very Github specific, we can
probably do better.
-/
def getGithubBaseUrl (gitUrl : String) : String := Id.run do
  let mut url := gitUrl
  if url.startsWith "git@" then
    url := url.drop 15
    url := url.dropRight 4
    pure s!"https://github.com/{url}"
  else if url.endsWith ".git" then
    pure <| url.dropRight 4
  else
    pure url

/--
Obtain the Github URL of a project by parsing the origin remote.
-/
def getProjectGithubUrl : IO String := do
  let out ← IO.Process.output {cmd := "git", args := #["remote", "get-url", "origin"]}
  if out.exitCode != 0 then
    throw <| IO.userError <| "git exited with code " ++ toString out.exitCode
  pure out.stdout.trimRight

/--
Obtain the git commit hash of the project that is currently getting analyzed.
-/
def getProjectCommit : IO String := do
  let out ← IO.Process.output {cmd := "git", args := #["rev-parse", "HEAD"]}
  if out.exitCode != 0 then
    throw <| IO.userError <| "git exited with code " ++ toString out.exitCode
  pure out.stdout.trimRight

/--
Given a lake workspace with all the dependencies as well as the hash of the
compiler release to work with this provides a function to turn names of
declarations into (optionally positional) Github URLs.
-/
def sourceLinker (ws : Lake.Workspace) : IO (Name → Option DeclarationRange → String) := do
  let leanHash := ws.lakeEnv.lean.githash
  -- Compute a map from package names to source URL
  let mut gitMap := Std.mkHashMap
  let projectBaseUrl := getGithubBaseUrl (←getProjectGithubUrl)
  let projectCommit ← getProjectCommit
  gitMap := gitMap.insert ws.root.name (projectBaseUrl, projectCommit)
  let manifest ← Lake.Manifest.loadOrEmpty ws.root.manifestFile
      |>.run (Lake.MonadLog.eio .normal)
      |>.toIO (λ _ => IO.userError "Failed to load lake manifest")
  for pkg in manifest.toArray do
    let value := (getGithubBaseUrl pkg.url, pkg.rev)
    gitMap := gitMap.insert pkg.name value

  pure λ module range =>
    let parts := module.components.map Name.toString
    let path := (parts.intersperse "/").foldl (· ++ ·) ""
    let root := module.getRoot
    let basic := if root == `Lean ∨ root == `Init ∨ root == `Std then
      s!"https://github.com/leanprover/lean4/blob/{leanHash}/src/{path}.lean"
    else
      match ws.packageArray.find? (·.isLocalModule module) with
      | some pkg =>
        match gitMap.find? pkg.name with
        | some (baseUrl, commit) => s!"{baseUrl}/blob/{commit}/{path}.lean"
        | none => "https://example.com"
      | none => "https://example.com"

    match range with
    | some range => s!"{basic}#L{range.pos.line}-L{range.endPos.line}"
    | none => basic

end DocGen4.Output
