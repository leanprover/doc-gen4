/-
Copyright (c) 2021 Henrik Böving. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Henrik Böving
-/

import Lean
import DocGen4.Process
import Lean.Data.HashMap

namespace DocGen4

open Lean System IO

def envOfImports (imports : Array Name) : IO Environment := do
 importModules (imports.map (Import.mk · false)) Options.empty

def loadInit (imports : Array Name) : IO Hierarchy := do
 let env ← envOfImports imports
 pure <| Hierarchy.fromArray env.header.moduleNames

/--
Load a list of modules from the current Lean search path into an `Environment`
to process for documentation.
-/
def load (task : Process.AnalyzeTask) : IO (Process.AnalyzerResult × Hierarchy) := do
  initSearchPath (← findSysroot)
  let env ← envOfImports task.getLoad
  let config := {
    -- TODO: parameterize maxHeartbeats
    maxHeartbeats := 100000000,
    options := ⟨[
      (`pp.tagAppFns, true),
      (`pp.funBinderTypes, true)
    ]⟩,
    -- TODO: Figure out whether this could cause some bugs
    fileName := default,
    fileMap := default,
    catchRuntimeEx := true,
  }

  Prod.fst <$> Meta.MetaM.toIO (Process.process task) config { env := env } {} {}

def loadCore : IO (Process.AnalyzerResult × Hierarchy) := do
  load <| .loadAll #[`Init, `Lean, `Lake]

end DocGen4
