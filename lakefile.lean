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

/--
Obtain the subdirectory of the Lean package relative to the root of the enclosing git repository.
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
  return out.stdout.trimAsciiEnd.copy

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


/-- The github URI of the source code of the package. -/
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

/-- The github URI of the source code of the module. -/
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

/--
Direct and transitive dependencies.

Loosely inspired by bazel's [depset](https://bazel.build/rules/lib/builtins/depset). -/
abbrev DepSet (α) [Hashable α] [BEq α] := Array α × OrdHashSet α

namespace DepSet
variable {α} [Hashable α] [BEq α]

def mk (direct : Array α) (trans : Array (DepSet α)) : DepSet α := Id.run do
  let mut deps := OrdHashSet.mkEmpty 0
  for (direct, trans) in trans do
    deps := deps.appendArray direct
    deps := deps.append trans
  return (direct, deps)

/-- Flatten a set of dependencies into a single list. -/
def toArray (d : DepSet α) : Array α := d.1 ++ d.2.toArray

instance [Lean.ToJson α] : Lean.ToJson (OrdHashSet α) where toJson x := Lean.toJson x.toArray
instance [QueryText α] : Lake.QueryText (DepSet α) where queryText d := Lake.QueryText.queryText d.toArray

end DepSet

module_facet docs (mod) : DepSet FilePath := do
  let exeJob ← «doc-gen4».fetch
  let bibPrepassJob ← bibPrepass.fetch
  let modJob ← mod.leanArts.fetch
  -- Build all documentation imported modules
  let imports ← (← mod.imports.fetch).await
  let depDocJobs := Job.collectArray <| ← imports.mapM fun mod => fetch <| mod.facet `docs
  let buildDir := (← getRootPackage).buildDir
  let docFile := mod.filePath (buildDir / "doc") "html"
  depDocJobs.bindM fun docDeps => do
    bibPrepassJob.bindM fun _ => do
      exeJob.bindM fun exeFile => do
        modJob.mapM fun _ => do
          buildFileUnlessUpToDate' docFile do
            -- hack: do this here to avoid having to save the git output anywhere else
            let uriJob ← fetch <| mod.facet `srcUri
            let srcUri ← uriJob.await
            proc {
              cmd := exeFile.toString
              args := #["single", "--build", buildDir.toString, mod.name.toString, srcUri]
              env := ← getAugmentedEnv
            }
          return DepSet.mk #[docFile] docDeps

def coreTarget (component : Lean.Name) : FetchM (Job <| Array FilePath) := do
  let exeJob ← «doc-gen4».fetch
  let bibPrepassJob ← bibPrepass.fetch
  let dataPath := (← getRootPackage).buildDir / "doc-data"
  let manifestFile := (← getRootPackage).buildDir / s!"{component}-manifest.json"
  let dataFile := dataPath / s!"declaration-data-{component}.bmp"
  let buildDir := (← getRootPackage).buildDir
  bibPrepassJob.bindM fun _ => do
    exeJob.mapM fun exeFile => do
      buildFileUnlessUpToDate' manifestFile do
        proc {
          cmd := exeFile.toString
          args := #["genCore", component.toString,
            "--build", buildDir.toString,
            "--manifest", manifestFile.toString]
          env := ← getAugmentedEnv
        }
      addTrace (← computeTrace dataFile)
      match Lean.Json.parse <| ← IO.FS.readFile manifestFile with
      | .error e => ELog.error s!"Could not parse json from {manifestFile}: {e}"
      | .ok manifestData =>
      match Lean.fromJson? manifestData with
      | .error e => ELog.error s!"Could not parse an array from {manifestFile}: {e}"
      | .ok (deps : Array System.FilePath) =>
      return deps.map (buildDir / ·)

target coreDocs : Array FilePath := do
  let coreComponents := #[`Init, `Std, `Lake, `Lean]
  return ← (Job.collectArray <| ← coreComponents.mapM coreTarget).mapM fun deps =>
    return deps.flatten

/-- A facet to generate the docs for a library. Returns all the filepaths that are required to
deploy a doc archive as a starting website. -/
library_facet docs (lib) : Array FilePath := do
  let mods ← (← lib.modules.fetch).await
  let moduleJobs := Job.collectArray <| ← mods.mapM (fetch <| ·.facet `docs)
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
    basePath / "foundational_types.html",
    basePath / "references.html",
    basePath / "references.bib",
    basePath / "Tactics.html",
    basePath / "find" / "index.html",
    basePath / "find" / "find.js"
  ]
  exeJob.bindM fun exeFile => do
    coreJobs.bindM fun coreDeps => do
      moduleJobs.mapM fun modDeps => do
        buildFileUnlessUpToDate' dataFile do
          logInfo "Documentation indexing"
          proc {
            cmd := exeFile.toString
            args := #["index", "--build", buildDir.toString]
          }
        let traces ← staticFiles.mapM computeTrace
        addTrace <| mixTraceArray traces
        return (DepSet.mk (#[dataFile] ++ staticFiles) (modDeps.push (.mk coreDeps #[]))).toArray

library_facet docsHeader (lib) : FilePath := do
  let mods ← (← lib.modules.fetch).await
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
