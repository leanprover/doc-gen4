/-
Copyright (c) 2021 Henrik Böving. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Henrik Böving
-/

import Lean
import Lake
import Lake.CLI.Main
import DocGen4.Process
import Std.Data.HashMap

namespace DocGen4

open Lean System Std IO

/--
Sets up a lake workspace for the current project. Furthermore initialize
the Lean search path with the path to the proper compiler from lean-toolchain
as well as all the dependencies.
-/
def lakeSetup (imports : List String) : IO (Except UInt32 (Lake.Workspace × String)) := do
  let (leanInstall?, lakeInstall?) ← Lake.findInstall?
  match Lake.Cli.mkLakeConfig {leanInstall?, lakeInstall?} with
  | Except.ok config =>
    let ws ← Lake.LogT.run Lake.MonadLog.eio (Lake.loadWorkspace config)
    let lean := config.leanInstall
    if lean.githash ≠ Lean.githash then
      IO.println s!"WARNING: This doc-gen was built with Lean: {Lean.githash} but the project is running on: {lean.githash}"
    let lake := config.lakeInstall
    let ctx ← Lake.mkBuildContext ws lean lake
    ws.root.buildImportsAndDeps imports |>.run Lake.MonadLog.eio ctx
    initSearchPath (←findSysroot) ws.leanPaths.oleanPath
    pure $ Except.ok (ws, lean.githash)
  | Except.error err =>
    throw $ IO.userError err.toString

/--
Load a list of modules from the current Lean search path into an `Environment`
to process for documentation.
-/
def load (imports : List Name) : IO Process.AnalyzerResult := do
  let env ← importModules (List.map (Import.mk · false) imports) Options.empty
  IO.println "Processing modules"
  let config := {
    -- TODO: parameterize maxHeartbeats
    maxHeartbeats := 100000000,
    options := ⟨[(`pp.tagAppFns, true)]⟩,
    -- TODO: Figure out whether this could cause some bugs
    fileName := default,
    fileMap := default,
  }
  Prod.fst <$> Meta.MetaM.toIO Process.process config { env := env} {} {}

end DocGen4
