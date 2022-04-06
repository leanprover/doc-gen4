/-
Copyright (c) 2021 Henrik Böving. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Henrik Böving
-/

import Lean
import Lake
import DocGen4.Process
import Std.Data.HashMap

namespace DocGen4

open Lean System Std IO

def lakeSetup (imports : List String) : IO (Except UInt32 (Lake.Workspace × String)) := do
  let (leanInstall?, lakeInstall?) ← Lake.findInstall?
  let res ← StateT.run Lake.Cli.loadWorkspace {leanInstall?, lakeInstall?} |>.toIO'
  match res with
  | Except.ok (ws, options) =>
    let lean := leanInstall?.get!
    if lean.githash ≠ Lean.githash then
      IO.println s!"WARNING: This doc-gen was built with Lean: {Lean.githash} but the project is running on: {lean.githash}"
    let lake := lakeInstall?.get!
    let ctx ← Lake.mkBuildContext ws lean lake
    ws.root.buildImportsAndDeps imports |>.run Lake.LogMethods.eio ctx
    initSearchPath (←findSysroot) ws.leanPaths.oleanPath
    pure $ Except.ok (ws, lean.githash)
  | Except.error rc => pure $ Except.error rc

def load (imports : List Name) : IO AnalyzerResult := do
  let env ← importModules (List.map (Import.mk · false) imports) Options.empty
  -- TODO parameterize maxHeartbeats
  IO.println "Processing modules"
  Prod.fst <$> Meta.MetaM.toIO process { maxHeartbeats := 100000000, options := ⟨[(`pp.tagAppFns, true)]⟩ } { env := env} {} {}

end DocGen4
