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
def lakeSetup : IO (Except UInt32 Lake.Workspace) := do
  let (leanInstall?, lakeInstall?) ← Lake.findInstall?
  match ←(EIO.toIO' <| Lake.mkLoadConfig {leanInstall?, lakeInstall?}) with
  | .ok config =>
    let ws : Lake.Workspace ← Lake.loadWorkspace config
      |>.run Lake.MonadLog.eio
      |>.toIO (λ _ => IO.userError "Failed to load Lake workspace")
    pure <| Except.ok ws
  | .error err =>
    throw <| IO.userError err.toString

def envOfImports (imports : List Name) : IO Environment := do
 importModules (imports.map (Import.mk · false)) Options.empty

def loadInit (imports : List Name) : IO Hierarchy := do
 let env ← envOfImports imports
 pure <| Hierarchy.fromArray env.header.moduleNames

/--
Load a list of modules from the current Lean search path into an `Environment`
to process for documentation.
-/
def load (task : Process.AnalyzeTask) : IO (Process.AnalyzerResult × Hierarchy) := do
  let env ← envOfImports task.getLoad
  IO.println "Processing modules"
  let config := {
    -- TODO: parameterize maxHeartbeats
    maxHeartbeats := 100000000,
    options := ⟨[(`pp.tagAppFns, true)]⟩,
    -- TODO: Figure out whether this could cause some bugs
    fileName := default,
    fileMap := default,
  }

  Prod.fst <$> Meta.MetaM.toIO (Process.process task) config { env := env } {} {}

def loadCore : IO (Process.AnalyzerResult × Hierarchy) := do
  load <| .loadAll [`Init, `Std, `Lean]

end DocGen4
