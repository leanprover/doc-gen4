import Lake
open System Lake DSL

package «doc-gen4»

lean_lib DocGen4

@[default_target]
lean_exe «doc-gen4» {
  root := `Main
  supportInterpreter := true
}

require CMark from git
  "https://github.com/xubaiw/CMark.lean" @ "main"

require «UnicodeBasic» from git
  "https://github.com/fgdorais/lean4-unicode-basic" @ "main"

require Cli from git
  "https://github.com/mhuisi/lean4-cli" @ "nightly"

require leanInk from git
  "https://github.com/hargonix/LeanInk" @ "doc-gen"


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

def getGitUrl (pkg : Package) (lib : LeanLibConfig) (mod : Module) : IO String := do
  let baseUrl := getGithubBaseUrl (← getProjectGithubUrl pkg.dir)
  let commit ← getProjectCommit pkg.dir

  let parts := mod.name.components.map toString
  let path := String.intercalate "/" parts
  let libPath := pkg.config.srcDir / lib.srcDir
  let basePath := String.intercalate "/" (libPath.components.filter (· != "."))
  let url := s!"{baseUrl}/blob/{commit}/{basePath}/{path}.lean"
  return url

module_facet docs (mod) : FilePath := do
  let some docGen4 ← findLeanExe? `«doc-gen4»
    | error "no doc-gen4 executable configuration found in workspace"
  let exeJob ← docGen4.exe.fetch
  let modJob ← mod.leanArts.fetch
  let ws ← getWorkspace
  let pkg ← ws.packages.find? (·.isLocalModule mod.name)
  let libConfig ← pkg.leanLibConfigs.toArray.find? (·.isLocalModule mod.name)
  -- Build all documentation imported modules
  let imports ← mod.imports.fetch
  let depDocJobs ← BuildJob.mixArray <| ← imports.mapM fun mod => fetch <| mod.facet `docs
  let gitUrl ← getGitUrl pkg libConfig mod
  let buildDir := ws.root.buildDir
  let docFile := mod.filePath (buildDir / "doc") "html"
  depDocJobs.bindAsync fun _ depDocTrace => do
  exeJob.bindAsync fun exeFile exeTrace => do
  modJob.bindSync fun _ modTrace => do
    let depTrace := mixTraceArray #[exeTrace, modTrace, depDocTrace]
    let trace ← buildFileUnlessUpToDate docFile depTrace do
      logStep s!"Documenting module: {mod.name}"
      proc {
        cmd := exeFile.toString
        args := #["single", mod.name.toString, gitUrl]
        env := ← getAugmentedEnv
      }
    return (docFile, trace)

-- TODO: technically speaking this facet does not show all file dependencies
target coreDocs : FilePath := do
  let some docGen4 ← findLeanExe? `«doc-gen4»
    | error "no doc-gen4 executable configuration found in workspace"
  let exeJob ← docGen4.exe.fetch
  let basePath := (←getWorkspace).root.buildDir / "doc"
  let dataFile := basePath / "declarations" / "declaration-data-Lean.bmp"
  exeJob.bindSync fun exeFile exeTrace => do
    let trace ← buildFileUnlessUpToDate dataFile exeTrace do
      logStep "Documenting Lean core: Init and Lean"
      proc {
        cmd := exeFile.toString
        args := #["genCore"]
        env := ← getAugmentedEnv
      }
    return (dataFile, trace)

library_facet docs (lib) : FilePath := do
  let mods ← lib.modules.fetch
  let moduleJobs ← BuildJob.mixArray <| ← mods.mapM (fetch <| ·.facet `docs)
  let coreJob : BuildJob FilePath ← coreDocs.fetch
  let exeJob ← «doc-gen4».fetch
  -- Shared with DocGen4.Output
  let basePath := (←getWorkspace).root.buildDir / "doc"
  let dataFile := basePath / "declarations" / "declaration-data.bmp"
  let staticFiles := #[
    basePath / "style.css",
    basePath / "declaration-data.js",
    basePath / "color-scheme.js",
    basePath / "nav.js",
    basePath / "jump-src.js",
    basePath / "expand-nav.js",
    basePath / "how-about.js",
    basePath / "search.js",
    basePath / "mathjax-config.js",
    basePath / "instances.js",
    basePath / "importedBy.js",
    basePath / "index.html",
    basePath / "404.html",
    basePath / "navbar.html",
    basePath / "search.html",
    basePath / "find" / "index.html",
    basePath / "find" / "find.js",
    basePath / "src"  / "alectryon.css",
    basePath / "src"  / "alectryon.js",
    basePath / "src"  / "docutils_basic.css",
    basePath / "src"  / "pygments.css"
  ]
  coreJob.bindAsync fun _ coreInputTrace => do
    exeJob.bindAsync fun exeFile exeTrace => do
      moduleJobs.bindSync fun _ inputTrace => do
        let depTrace := mixTraceArray #[inputTrace, exeTrace, coreInputTrace]
        let trace ← buildFileUnlessUpToDate dataFile depTrace do
          logInfo "Documentation indexing"
          proc {
            cmd := exeFile.toString
            args := #["index"]
          }
        let traces ← staticFiles.mapM computeTrace
        let indexTrace := mixTraceArray traces

        return (dataFile, trace.mix indexTrace)
