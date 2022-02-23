/-
Copyright (c) 2021 Henrik Böving. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Henrik Böving
-/

import Lean
import DocGen4.Process
import Std.Data.HashMap

namespace DocGen4

open Lean System Std IO

def getLakePath : IO FilePath := do
  match (← IO.getEnv "LAKE") with
  | some path => pure $ System.FilePath.mk path
  | none =>
    let lakePath := (←findSysroot?) / "bin" / "lake"
    pure $ lakePath.withExtension System.FilePath.exeExtension

-- Modified from the LSP Server
def lakeSetupSearchPath (lakePath : System.FilePath) (imports : List String) : IO Lean.SearchPath := do
  let args := "print-paths" :: imports
  let cmdStr := " ".intercalate (toString lakePath :: args)
  let lakeProc ← Process.spawn {
    stdin  := Process.Stdio.null
    stdout := Process.Stdio.piped
    stderr := Process.Stdio.piped
    cmd    := lakePath.toString
    args := args.toArray
  }
  let stdout := String.trim (← lakeProc.stdout.readToEnd)
  let stderr := String.trim (← lakeProc.stderr.readToEnd)
  match (← lakeProc.wait) with
  | 0 =>
    let stdout := stdout.split (· == '\n') |>.getLast!
    let Except.ok (paths : LeanPaths) ← pure (Json.parse stdout >>= fromJson?)
      | throw $ userError s!"invalid output from `{cmdStr}`:\n{stdout}\nstderr:\n{stderr}"
    initSearchPath (← findSysroot?) paths.oleanPath
    paths.oleanPath.mapM realPathNormalized
  | 2 => pure []  -- no lakefile.lean
  | _ => throw $ userError s!"`{cmdStr}` failed:\n{stdout}\nstderr:\n{stderr}"

def load (imports : List Name) : IO AnalyzerResult := do
  let env ← importModules (List.map (Import.mk · false) imports) Options.empty
  -- TODO parameterize maxHeartbeats
  IO.println "Processing modules"
  Prod.fst <$> (Meta.MetaM.toIO process { maxHeartbeats := 100000000, options := ⟨[(`pp.tagAppFns, true)]⟩ } { env := env} {} {})

end DocGen4
