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

require «lean4-unicode-basic» from git
  "https://github.com/fgdorais/lean4-unicode-basic" @ "main"

require Cli from git
  "https://github.com/mhuisi/lean4-cli" @ "nightly"

require leanInk from git
  "https://github.com/hargonix/LeanInk" @ "doc-gen"

module_facet docs (mod) : FilePath := do
  let some docGen4 ← findLeanExe? `«doc-gen4»
    | error "no doc-gen4 executable configuration found in workspace"
  let exeJob ← docGen4.exe.fetch
  let modJob ← mod.leanBin.fetch
  let buildDir := (← getWorkspace).root.buildDir
  let docFile := mod.filePath (buildDir / "doc") "html"
  exeJob.bindAsync fun exeFile exeTrace => do
  modJob.bindSync fun _ modTrace => do
    let depTrace := exeTrace.mix modTrace
    let trace ← buildFileUnlessUpToDate docFile depTrace do
      logStep s!"Documenting module: {mod.name}"
      proc {
        cmd := exeFile.toString
        args := #["single", mod.name.toString]
        env := #[("LEAN_PATH", (← getAugmentedLeanPath).toString)]
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
        env := #[("LEAN_PATH", (← getAugmentedLeanPath).toString)]
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
