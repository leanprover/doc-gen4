import Lake
open System Lake DSL

package «doc-gen4»

lean_lib DocGen4

@[default_target]
lean_exe «doc-gen4» {
  root := `Main
  supportInterpreter := true
}

require MD4Lean from git
  "https://github.com/acmepjz/md4lean" @ "main"

require BibtexQuery from git
  "https://github.com/dupuisf/BibtexQuery" @ "master"

require «UnicodeBasic» from git
  "https://github.com/fgdorais/lean4-unicode-basic" @ "main"

require Cli from git
  "https://github.com/mhuisi/lean4-cli" @ "main"

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
    let explanation := "Failed to find a git remote in your project, consider reading: https://github.com/leanprover/doc-gen4#source-locations"
    let err := s!"git exited with code {out.exitCode} while looking for the git remote in {directory}"
    throw <| IO.userError <| explanation ++ "\n" ++ err
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
def appendLibModPath (pkgUri : String) (pathSep : Char) (mod : Module) : String :=
  let libPath := filteredPath mod.lib.config.srcDir
  let newBase := (pathSep.toString.intercalate (pkgUri :: libPath))
  appendModPath newBase pathSep mod

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

def getGithubUrl (mod : Module) : IO String := do
  let url ← getGitRemoteUrl mod.pkg.dir "origin"
  let .some baseUrl := getGithubBaseUrl url
      | throw <| IO.userError <|
        s!"Could not interpret Git remote uri {url} as a Github source repo.\n"
          ++ "See README on source URIs for more details."
  let commit ← getProjectCommit mod.pkg.dir
  let srcDir := filteredPath mod.pkg.config.srcDir
  let pkgUri := "/".intercalate <| baseUrl :: "blob" :: commit :: srcDir
  return appendLibModPath pkgUri '/' mod

/--
Return a File uri for the module.
-/
def getFileUri (mod : Module) : IO String := do
  let dir ← IO.FS.realPath (mod.pkg.dir / mod.pkg.config.srcDir)
  return appendLibModPath s!"file://{dir}" FilePath.pathSeparator mod

/--
Return a VSCode uri for the module.
-/
def getVSCodeUri (mod : Module) : IO String := do
  let dir ← IO.FS.realPath (mod.pkg.dir / mod.pkg.config.srcDir)
  return appendLibModPath s!"vscode://file/{dir}" FilePath.pathSeparator mod

/--
Attempt to determine URI to use for the module source file.
-/
def getSrcUri (mod : Module) : IO String := do
  match ←IO.getEnv "DOCGEN_SRC" with
  | .none | .some "github" | .some "" => getGithubUrl mod
  | .some "vscode" => getVSCodeUri mod
  | .some "file" => getFileUri mod
  | .some _ => throw <| IO.userError "$DOCGEN_SRC should be github, file, or vscode."

target bibPrepass : FilePath := do
  let exeJob ← «doc-gen4».fetch
  let buildDir := (← getRootPackage).buildDir
  let dataPath := buildDir / "doc-data"
  let inputJsonFile := (← getRootPackage).srcDir / "docs" / "references.json"
  let inputBibFile := (← getRootPackage).srcDir / "docs" / "references.bib"
  let outputFile := dataPath / "references.json"
  let tryJson : JobM (Array String) := do
    addTrace <| ← computeTrace inputJsonFile
    addTrace <| BuildTrace.ofHash (.ofString "json")
    return #["--build", buildDir.toString, "--json", inputJsonFile.toString]
  let tryBib : JobM (Array String) := do
    addTrace <| ← computeTrace inputBibFile
    addTrace <| BuildTrace.ofHash (.ofString "bib")
    return #["--build", buildDir.toString, inputBibFile.toString]
  let tryBibFailed : JobM (Array String) := do
    addTrace .nil
    return #["--build", buildDir.toString, "--none"]
  exeJob.mapM fun exeFile => do
    let args ← tryJson <|> tryBib <|> tryBibFailed
    buildFileUnlessUpToDate' outputFile do
      proc {
        cmd := exeFile.toString
        args := #["bibPrepass"] ++ args
        env := ← getAugmentedEnv
      }
    return outputFile

module_facet docs (mod) : FilePath := do
  let exeJob ← «doc-gen4».fetch
  let bibPrepassJob ← bibPrepass.fetch
  let modJob ← mod.leanArts.fetch
  -- Build all documentation imported modules
  let imports ← mod.imports.fetch
  let depDocJobs := Job.mixArray <| ← imports.mapM fun mod => fetch <| mod.facet `docs
  let srcUri ← getSrcUri mod
  let buildDir := (← getRootPackage).buildDir
  let docFile := mod.filePath (buildDir / "doc") "html"
  depDocJobs.bindM fun _ => do
    bibPrepassJob.bindM fun _ => do
      exeJob.bindM fun exeFile => do
        modJob.mapM fun _ => do
          buildFileUnlessUpToDate' docFile do
            proc {
              cmd := exeFile.toString
              args := #["single", "--build", buildDir.toString, mod.name.toString, srcUri]
              env := ← getAugmentedEnv
            }
          return docFile

-- TODO: technically speaking this target does not show all file dependencies
def coreTarget (component : Lean.Name) : FetchM (Job FilePath) := do
  let exeJob ← «doc-gen4».fetch
  let bibPrepassJob ← bibPrepass.fetch
  let dataPath := (← getRootPackage).buildDir / "doc-data"
  let dataFile := dataPath / s!"declaration-data-{component}.bmp"
  let buildDir := (← getRootPackage).buildDir
  bibPrepassJob.bindM fun _ => do
    exeJob.mapM fun exeFile  => do
      buildFileUnlessUpToDate' dataFile do
        proc {
          cmd := exeFile.toString
          args := #["genCore", component.toString, "--build", buildDir.toString]
          env := ← getAugmentedEnv
        }
      return dataFile

target coreDocs : Unit := do
  let coreComponents := #[`Init, `Std, `Lake, `Lean]
  return Job.mixArray <| ← coreComponents.mapM coreTarget

library_facet docs (lib) : FilePath := do
  let mods ← lib.modules.fetch
  let moduleJobs := Job.mixArray <| ← mods.mapM (fetch <| ·.facet `docs)
  let coreJobs ← coreDocs.fetch
  let exeJob ← «doc-gen4».fetch
  -- Shared with DocGen4.Output
  let buildDir := (← getRootPackage).buildDir
  let basePath := buildDir / "doc"
  let dataFile := basePath / "declarations" / "declaration-data.bmp"
  let staticFiles := #[
    basePath / "style.css",
    basePath / "favicon.svg",
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
    basePath / "find" / "find.js"
  ]
  exeJob.bindM fun exeFile => do
    coreJobs.bindM fun _ => do
      moduleJobs.mapM fun _ => do
        buildFileUnlessUpToDate' dataFile do
          logInfo "Documentation indexing"
          proc {
            cmd := exeFile.toString
            args := #["index", "--build", buildDir.toString]
          }
        let traces ← staticFiles.mapM computeTrace
        addTrace <| mixTraceArray traces
        return dataFile

library_facet docsHeader (lib) : FilePath := do
  let mods ← lib.modules.fetch
  let moduleJobs := Job.mixArray <| ← mods.mapM (fetch <| ·.facet `docs)
  let exeJob ← «doc-gen4».fetch
  let coreJobs ← coreDocs.fetch
  -- Shared with DocGen4.Output
  let buildDir := (← getRootPackage).buildDir
  let basePath := buildDir / "doc"
  let dataFile := basePath / "declarations" / "header-data.bmp"
  exeJob.bindM fun exeFile => do
   coreJobs.bindM fun _ => do
      moduleJobs.mapM fun _ => do
        buildFileUnlessUpToDate' dataFile do
          logInfo "Documentation header indexing"
          proc {
            cmd := exeFile.toString
            args := #["headerData", "--build", buildDir.toString]
          }
        return dataFile
