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

/--
Load a list of modules from the current Lean search path into an `Environment`
to process for documentation.
-/
def load (task : Process.AnalyzeTask) : IO Process.AnalyzerResult := do
  initSearchPath (← findSysroot)
  let env ← envOfImports task.getLoad
  let config := {
    -- TODO: parameterize maxHeartbeats
    maxHeartbeats := 100000000,
    options :=
      Options.empty
        |>.setBool `pp.tagAppFns true
        |>.setBool `pp.funBinderTypes true
        |>.setBool `debug.skipKernelTC true
        |>.setBool `Elab.async false,
    -- TODO: Figure out whether this could cause some bugs
    fileName := default,
    fileMap := default,
  }

  Prod.fst <$> Meta.MetaM.toIO (Process.process task) config { env := env } {} {}

end DocGen4
