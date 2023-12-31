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
Obtain the Github URL of a project by parsing the origin remote.
-/
def getGitRemoteUrl (directory : System.FilePath := "." ) (remote : String := "origin") : IO String := do
  let out ← IO.Process.output {
    cmd := "git",
    args := #["remote", "get-url", remote],
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

def filteredPath (path : FilePath) : List String := path.components.filter (· != ".")


/--
Append the module path to a string with the separator used for name components.
-/
def appendModPath (libUri : String) (pathSep : Char) (mod : Module)  : String :=
  mod.name.components.foldl (init := libUri) (·.push pathSep ++ ·.toString) ++ ".lean"

/--
Append the library and mod path to the given Uri referring to the package source.
-/
def appendLibModPath (pkgUri : String) (pathSep : Char) (lib : LeanLibConfig) (mod : Module) : String :=
  let libPath := filteredPath lib.srcDir
  appendModPath (pathSep.toString.intercalate (pkgUri :: libPath)) pathSep mod

/--
Turns a Github git remote URL into an HTTPS Github URL.
Three link types from git supported:
- https://github.com/org/repo
- https://github.com/org/repo.git
- git@github.com:org/repo.git
-/
def getGithubBaseUrl (url : String) : Option String :=
  if url.startsWith "git@github.com:" && url.endsWith ".git" then
    let url := url.drop "git@github.com:".length
    let url := url.dropRight ".git".length
    .some s!"https://github.com/{url}"
  else if url.startsWith "https://github.com/" then
    if url.endsWith ".git" then
      .some <| url.dropRight ".git".length
    else
      .some url
  else
    .none

def getGithubUrl (pkg : Package) (lib : LeanLibConfig) (mod : Module) : IO String := do
  let url ←  getGitRemoteUrl pkg.dir "origin"
  let .some baseUrl := getGithubBaseUrl url
      | throw <| IO.userError <|
        s!"Could not interpret Git remote uri {url} as a Github source repo.\n"
          ++ "See README on source URIs for more details."
  let commit ← getProjectCommit pkg.dir
  let srcDir := filteredPath pkg.config.srcDir
  let pkgUri := "/".intercalate <| baseUrl :: "blob" :: commit :: srcDir

  pure <| appendLibModPath pkgUri '/' lib mod

/--
Return a File uri for the module.
-/
def getFileUri (pkg : Package) (lib : LeanLibConfig) (mod : Module) := do
  let dir ← IO.FS.realPath (pkg.dir / pkg.config.srcDir)
  pure <| appendLibModPath s!"file://{dir}" FilePath.pathSeparator lib mod

/--
Return a VSCode uri for the module.
-/
def getVSCodeUri (pkg : Package) (lib : LeanLibConfig) (mod : Module) : IO String := do
  let dir ← IO.FS.realPath (pkg.dir / pkg.config.srcDir)
  pure <| appendLibModPath s!"vscode://file/{dir}" FilePath.pathSeparator lib mod

/--
Attempt to determine URI to use for the module source file.
-/
def getSrcUri (pkg : Package) (lib : LeanLibConfig) (mod : Module) : IO String := do
  match ←IO.getEnv "DOCGEN_SRC" with
  | .none | .some "github" | .some "" => getGithubUrl pkg lib mod
  | .some "vscode" => getVSCodeUri pkg lib mod
  | .some "file" => getFileUri pkg lib mod
  | .some _ => throw <| IO.userError "$DOCGEN_SRC should be github, file, or vscode."

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
  let srcUri ← getSrcUri pkg libConfig mod
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
        args := #["single", mod.name.toString, srcUri]
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
