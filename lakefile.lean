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
  "https://github.com/leanprover/lean4-cli" @ "main"

require leansqlite from git
  "https://github.com/david-christiansen/leansqlite" @ "main"

require plausible from git
  "https://github.com/leanprover-community/plausible" @ "v4.28.0-rc1"

/--
Obtains the subdirectory of the Lean package relative to the root of the enclosing git repository.
-/
def getGitSubDirectory (directory : System.FilePath := "." ) : IO System.FilePath := do
  let out ← IO.Process.output {
    cmd := "git",
    args := #["rev-parse", "--show-prefix"],
    cwd := directory
  }
  if out.exitCode != 0 then
    let explanation := "Failed to execute git rev-parse --show-prefix"
    let err := s!"git exited with code {out.exitCode} while looking for the git subdirectory in {directory}"
    throw <| IO.userError <| explanation ++ "\n" ++ err
  let subdir := out.stdout.trimAsciiEnd
  -- e.g. if the Lean package is under a directory "myleanpackage",
  -- `git rev-parse --show-prefix` would return "myleanpackage/".
  -- We drop the trailing path separator.
  return if subdir == "".toSlice then "." else subdir.dropEnd 1 |>.copy

/--
Obtains the GitHub URL of a project by parsing the origin remote.
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
  return out.stdout.trimAsciiEnd.copy

/--
Obtains the Git commit hash of the project that is currently getting analyzed.
-/
def getProjectCommit (directory : System.FilePath := "." ) : IO String := do
  let out ← IO.Process.output {
    cmd := "git",
    args := #["rev-parse", "HEAD"]
    cwd := directory
  }
  if out.exitCode != 0 then
    throw <| IO.userError <| s!"git exited with code {out.exitCode} while looking for the current commit in {directory}"
  return out.stdout.trimAsciiEnd.copy

def filteredPath (path : FilePath) : List String := path.components.filter (· != ".")

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
    let url := url.dropEnd ".git".length
    .some s!"https://github.com/{url}"
  else if url.startsWith "https://github.com/" then
    if url.endsWith ".git" then
      .some <| url.dropEnd ".git".length |>.copy
    else
      .some url
  else
    .none

inductive UriSource
  | github
  | vscode
  | file

def UriSource.parse : IO UriSource := do
  match ← IO.getEnv "DOCGEN_SRC" with
  | .none | .some "github" | .some "" => pure .github
  | "vscode" => pure .vscode
  | "file" => pure .file
  | _ => error "$DOCGEN_SRC should be github, file, or vscode."

/-! Note that all URIs can use `/` even when the system path separator is `\`. -/


/-- The GitHub URI of the source code of the package. -/
package_facet srcUri.github (pkg) : String := Job.async do
  let url ← getGitRemoteUrl pkg.dir "origin"
  let .some baseUrl := getGithubBaseUrl url
      | error <|
        s!"Could not interpret Git remote uri {url} as a Github source repo.\n"
          ++ "See README on source URIs for more details."
  let commit ← getProjectCommit pkg.dir
  logInfo s!"Found git remote for {pkg.baseName} at {baseUrl} @ {commit}"
  let subdir ← getGitSubDirectory pkg.dir
  return "/".intercalate <| baseUrl :: "blob" :: commit :: filteredPath (subdir / pkg.config.srcDir)

/-- The `vscode://` URI of the source code of the package. -/
package_facet srcUri.vscode (pkg) : String := .pure <$> do
  let dir ← IO.FS.realPath (pkg.dir / pkg.config.srcDir)
  return s!"vscode://file/{dir}"

/-- The `file://` URI of the source code of the package. -/
package_facet srcUri.file (pkg) : String := .pure <$> do
  let dir ← IO.FS.realPath (pkg.dir / pkg.config.srcDir)
  return s!"file://{dir}"

/-- The URI of the source code of the package, respecting `DOCGEN_SRC`. -/
package_facet srcUri (pkg) : String := do
  match ← UriSource.parse with
  | .github => fetch <| pkg.facet `srcUri.github
  | .vscode => fetch <| pkg.facet `srcUri.vscode
  | .file => fetch <| pkg.facet `srcUri.file


private def makeLibSrcUriFacet (lib : LeanLib) (which : Lean.Name)
    [FamilyDef FacetOut (Package.facetKind ++ which) String] :
    FetchM (Job String) := do
  let pkgUri ← fetch <| lib.pkg.facet which
  pkgUri.mapM (sync := true) fun pkgUri => do
    return "/".intercalate (pkgUri :: filteredPath lib.config.srcDir)

/-- The github URI of the source code of the library. -/
library_facet srcUri.github (lib) : String := makeLibSrcUriFacet lib `srcUri.github
/-- The `vscode://` URI of the source code of the library. -/
library_facet srcUri.vscode (lib) : String := makeLibSrcUriFacet lib `srcUri.vscode
/-- The `file://` URI of the source code of the library. -/
library_facet srcUri.file (lib) : String := makeLibSrcUriFacet lib `srcUri.file
/-- The URI of the source code of the library, respecting `DOCGEN_SRC`. -/
library_facet srcUri (lib) : String := makeLibSrcUriFacet lib `srcUri

private def makeModuleSrcUriFacet (mod : Module) (which : Lean.Name)
    [FamilyDef FacetOut (LeanLib.facetKind ++ which) String] :
    FetchM (Job String) := do
  let libUri ← fetch <| mod.lib.facet which
  libUri.mapM (sync := true) fun libUri => do
    return mod.name.components.foldl (init := libUri) (·.push '/' ++ ·.toString (escape := False)) ++ ".lean"

/-- The GitHub URI of the source code of the module. -/
module_facet srcUri.github (mod) : String := makeModuleSrcUriFacet mod `srcUri.github
/-- The `vscode://` URI of the source code of the module. -/
module_facet srcUri.vscode (mod) : String := makeModuleSrcUriFacet mod `srcUri.vscode
/-- The `file://` URI of the source code of the module. -/
module_facet srcUri.file (mod) : String := makeModuleSrcUriFacet mod `srcUri.file
/-- The URI of the source code of the module, respecting `DOCGEN_SRC`. -/
module_facet srcUri (mod) : String := makeModuleSrcUriFacet mod `srcUri

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

def coreTarget (component : Lean.Name) : FetchM (Job FilePath) := do
  let exeJob ← «doc-gen4».fetch
  let bibPrepassJob ← bibPrepass.fetch
  let buildDir := (← getRootPackage).buildDir
  let markerFile := buildDir / "doc-data" / s!"core-{component}.doc"
  bibPrepassJob.bindM fun _ => do
    exeJob.mapM fun exeFile => do
      buildFileUnlessUpToDate' markerFile do
        proc {
          cmd := exeFile.toString
          args := #["genCore", "--build", buildDir.toString, component.toString, "api-docs.db"]
          env := ← getAugmentedEnv
        }
        IO.FS.createDirAll markerFile.parent.get!
        IO.FS.writeFile markerFile ""
      return markerFile

/--
Populates the database with documentation data for core Lean. Returns a set of marker files that
indicate that the database has been updated for the corresponding modules, allowing Lake to track
changes and dependencies.
-/
target coreDocs : Array FilePath := do
  let coreComponents := #[`Init, `Std, `Lake, `Lean]
  return ← (Job.collectArray <| ← coreComponents.mapM coreTarget).mapM fun deps =>
    return deps

/--
Places the module's documentation content into the package's documentation database.

Returns a marker file that indicates the database has been populated for this module.
The marker file participates in Lake's dependency tracking, allowing for incremental updates.
-/
module_facet docInfo (mod) : FilePath := do
  let exeJob ← «doc-gen4».fetch
  let bibPrepassJob ← bibPrepass.fetch
  let coreJob ← coreDocs.fetch
  let modJob ← mod.leanArts.fetch
  -- Build all documentation for imported modules
  let imports ← (← mod.imports.fetch).await
  let depDocJobs := Job.mixArray <| ← imports.mapM fun mod => fetch <| mod.facet `docInfo
  let buildDir := (← getRootPackage).buildDir
  let markerFile := buildDir / "doc-data" / s!"{mod.name}.doc"
  coreJob.bindM fun _ => do
    depDocJobs.bindM fun _ => do
      bibPrepassJob.bindM fun _ => do
        exeJob.bindM fun exeFile => do
          modJob.mapM fun _ => do
            buildFileUnlessUpToDate' markerFile do
              let uriJob ← fetch <| mod.facet `srcUri
              let srcUri ← uriJob.await
              proc {
                cmd := exeFile.toString
                args := #["single", "--build", buildDir.toString, mod.name.toString, "api-docs.db", srcUri]
                env := ← getAugmentedEnv
              }
              IO.FS.createDirAll markerFile.parent.get!
              IO.FS.writeFile markerFile ""
            return markerFile

/--
Populates the database with information for all modules in a library.
-/
library_facet docInfo (lib) : Array FilePath := do
  let mods ← (← lib.modules.fetch).await
  let moduleJobs := Job.collectArray <| ← mods.mapM (fetch <| ·.facet `docInfo)
  moduleJobs.mapM fun modDeps =>
    return modDeps

/--
A facet to collect docInfo dependencies for a package (no HTML generation).
This populates the database with all module data and core docs for all packages
in the workspace (including dependencies).
Returns the database file path.
-/
package_facet docInfo (pkg) : FilePath := do
  let ws ← getWorkspace
  let allLibs := ws.packages.flatMap (·.leanLibs)
  let libDocJobs := Job.collectArray <| ← allLibs.mapM (fetch <| ·.facet `docInfo)
  let coreJobs ← coreDocs.fetch
  let dbPath := pkg.buildDir / "api-docs.db"
  coreJobs.bindM fun _ => do
    libDocJobs.mapM fun _ =>
      return dbPath

library_facet docsHeader (lib) : FilePath := do
  -- Depend on the package docs facet to ensure HTML is generated first
  let pkgDocsJob ← fetch <| lib.pkg.facet `docs
  let exeJob ← «doc-gen4».fetch
  -- Shared with DocGen4.Output
  let buildDir := (← getRootPackage).buildDir
  let basePath := buildDir / "doc"
  let dataFile := basePath / "declarations" / "header-data.bmp"
  exeJob.bindM fun exeFile => do
    pkgDocsJob.mapM fun _ => do
      buildFileUnlessUpToDate' dataFile do
        logInfo "Documentation header indexing"
        proc {
          cmd := exeFile.toString
          args := #["headerData", "--build", buildDir.toString]
        }
      return dataFile


/-- Generate HTML for this module and its transitive imports. -/
module_facet docs (mod) : Unit := do
  let exeJob ← «doc-gen4».fetch
  let bibPrepassJob ← bibPrepass.fetch
  let docInfoJob ← fetch <| mod.facet `docInfo

  let buildDir := (← getRootPackage).buildDir
  let dbPath := buildDir / "api-docs.db"

  bibPrepassJob.bindM fun _ => do
    exeJob.bindM fun exeFile => do
      docInfoJob.mapM fun _ => do
        logInfo s!"Generating documentation for {mod.name} and dependencies"
        proc {
          cmd := exeFile.toString
          args := #["fromDb", "--build", buildDir.toString, dbPath.toString, mod.name.toString]
          env := ← getAugmentedEnv
        }

/-- Generate HTML for all modules in this library. -/
library_facet docs (lib) : Unit := do
  let coreJob ← coreDocs.fetch
  let mods ← (← lib.modules.fetch).await
  let jobs ← mods.mapM fun mod => fetch <| mod.facet `docs
  coreJob.bindM fun _ => do
    Job.collectArray jobs |>.mapM fun _ => pure ()

/--
Generates documentation for the package's default library targets. Builds the `docs` facet of each
library, which in turn generates HTML for each module.
-/
package_facet docs (pkg) : Unit := do
  let defaultTargets := pkg.defaultTargets
  let libs := pkg.leanLibs.filter fun lib => defaultTargets.contains lib.name
  let jobs ← libs.mapM fun lib => fetch <| lib.facet `docs
  Job.collectArray jobs |>.mapM fun _ => pure ()
