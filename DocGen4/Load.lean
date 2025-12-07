/-
Copyright (c) 2021 Henrik Böving. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Henrik Böving
-/
import DocGen4.Process

namespace DocGen4

open Lean System IO

def envOfImports (imports : Array Name) : IO Environment := do
  -- needed for modules which use syntax registered with `initialize add_parser_alias ..`
  unsafe Lean.enableInitializersExecution
  importModules (imports.map (Import.mk · false true false)) Options.empty (leakEnv := true) (loadExts := true)

def loadInit (imports : Array Name) : IO Hierarchy := do
 let env ← envOfImports imports
 pure <| Hierarchy.fromArray env.header.moduleNames

/--
Load a list of modules from the current Lean search path into an `Environment`
to process for documentation.
-/
def load (task : Process.AnalyzeTask) (maxHeartbeats : Nat := 100000000) : IO (Process.AnalyzerResult × Hierarchy) := do
  initSearchPath (← findSysroot)
  let env ← envOfImports task.getLoad
  let config := {
    maxHeartbeats := maxHeartbeats,
    options := ⟨[
      (`pp.tagAppFns, true),
      (`pp.funBinderTypes, true),
      (`debug.skipKernelTC, true),
      (`Elab.async, false)
    ]⟩,
    -- TODO: Figure out whether this could cause some bugs
    fileName := default,
    fileMap := default,
  }

  Prod.fst <$> Meta.MetaM.toIO (Process.process task) config { env := env } {} {}

end DocGen4
