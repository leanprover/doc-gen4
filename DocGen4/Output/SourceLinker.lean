/-
Copyright (c) 2022 Henrik Böving. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Henrik Böving
-/
import Lean
import Lake.Load

namespace DocGen4.Output.SourceLinker

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
    return s!"https://github.com/{url}"
  else if url.endsWith ".git" then
    return url.dropRight 4
  else
    return url

/--
Obtain the Github URL of a project by parsing the origin remote.
-/
def getProjectGithubUrl (directory : System.FilePath := "." ) : IO String := do
  let out ← IO.Process.output {
    cmd := "git",
    args := #["remote", "get-url", "origin"],
    cwd := directory
  }
  if out.exitCode != 0 then
    throw <| IO.userError <| s!"git exited with code {out.exitCode} while looking for the git remote in {directory}"
  return out.stdout.trimRight

/--
Obtain the git commit hash of the project that is currently getting analyzed.
-/
def getProjectCommit (directory : System.FilePath := "." ) : IO String := do
  let out ← IO.Process.output {
    cmd := "git",
    args := #["rev-parse", "HEAD"]
    cwd := directory
  }
  if out.exitCode != 0 then
    throw <| IO.userError <| s!"git exited with code {out.exitCode} while looking for the current commit in {directory}"
  return out.stdout.trimRight

def modulePath (ws : Lake.Workspace) (module : Name) : Option (Lake.Package × Lake.LeanLibConfig) := do
  let pkg ← ws.packages.find? (·.isLocalModule module)
  let libConfig ← pkg.leanLibConfigs.toArray.find? (·.isLocalModule module)
  return (pkg, libConfig)

/--
Given a lake workspace with all the dependencies as well as the hash of the
compiler release to work with this provides a function to turn names of
declarations into (optionally positional) Github URLs.
-/
def sourceLinker (ws : Lake.Workspace) : IO (Name → Option DeclarationRange → String) := do
  let leanHash := ws.lakeEnv.lean.githash
  -- Compute a map from package names to source URL
  let mut gitMap := Lean.mkHashMap
  let projectBaseUrl := getGithubBaseUrl (← getProjectGithubUrl)
  let projectCommit ← getProjectCommit
  gitMap := gitMap.insert ws.root.name (projectBaseUrl, projectCommit)
  let manifest ← Lake.Manifest.loadOrEmpty ws.root.manifestFile
      |>.run (Lake.MonadLog.eio .normal)
      |>.toIO (fun _ => IO.userError "Failed to load lake manifest")
  for pkg in manifest.packages do
    match pkg with
    | .git _ _ _ url rev .. => gitMap := gitMap.insert pkg.name (getGithubBaseUrl url, rev)
    | .path _ _ _ path =>
      let pkgBaseUrl := getGithubBaseUrl (← getProjectGithubUrl path)
      let pkgCommit ← getProjectCommit path
      gitMap := gitMap.insert pkg.name (pkgBaseUrl, pkgCommit)

  return fun module range =>
    let parts := module.components.map Name.toString
    let path := String.intercalate "/" parts
    let root := module.getRoot
    let basic := if root == `Lean ∨ root == `Init then
      s!"https://github.com/leanprover/lean4/blob/{leanHash}/src/{path}.lean"
    else if root == `Lake then
      s!"https://github.com/leanprover/lean4/blob/{leanHash}/src/lake/{path}.lean"
    else
      match modulePath ws module with
      | some (pkg, lib) =>
        match gitMap.find? pkg.name with
        | some (baseUrl, commit) =>
          let libPath := pkg.config.srcDir / lib.srcDir
          let basePath := String.intercalate "/" (libPath.components.filter (· != "."))
          s!"{baseUrl}/blob/{commit}/{basePath}/{path}.lean"
        | none => "https://example.com"
      | none => "https://example.com"

    match range with
    | some range => s!"{basic}#L{range.pos.line}-L{range.endPos.line}"
    | none => basic

end DocGen4.Output.SourceLinker
