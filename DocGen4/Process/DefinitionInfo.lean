/-
Copyright (c) 2022 Henrik Böving. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Henrik Böving
-/
import Lean

import DocGen4.Process.Base
import DocGen4.Process.NameInfo

namespace DocGen4.Process

open Lean Meta Widget

def valueToEq (v : DefinitionVal) : MetaM Expr := withLCtx {} {} do
  withOptions (tactic.hygienic.set . false) do
    lambdaTelescope v.value fun xs body => do
      let us := v.levelParams.map mkLevelParam
      let type ← mkEq (mkAppN (Lean.mkConst v.name us) xs) body
      let type ← mkForallFVars xs type
      return type

def prettyPrintEquation (expr : Expr) : MetaM RenderedCode :=
  Meta.forallTelescope expr.consumeMData (fun _ e => prettyPrintTerm e)

def processEq (eq : Name) : MetaM RenderedCode := do
  let type ← (mkConstWithFreshMVarLevels eq >>= inferType)
  prettyPrintEquation type

def computeEquations? (v : DefinitionVal) : AnalyzeM (Array RenderedCode) := do
  unless (← read).genEquations do return #[]
  let eqs? ← getEqnsFor? v.name
  match eqs? with
  | some eqs =>
    let eqs ← liftM (eqs.mapM processEq)
    return eqs
  | none =>
    let equations := #[← prettyPrintEquation (← valueToEq v)]
    return equations

def DefinitionInfo.ofDefinitionVal (v : DefinitionVal) : AnalyzeM DefinitionInfo := do
  let info ← Info.ofConstantVal v.toConstantVal
  let isUnsafe := v.safety == DefinitionSafety.unsafe
  let isNonComputable := isNoncomputable (← getEnv) v.name
  let sorried := v.value.hasSorry

  let equations ←
    tryCatchRuntimeEx
      (.some <$> computeEquations? v)
      (fun err => do
        IO.println s!"WARNING: Failed to calculate equational lemmata for {v.name}: {← err.toMessageData.toString}"
        return none)

  return {
    toInfo := { info with sorried := sorried },
    isUnsafe,
    hints := v.hints,
    equations,
    isNonComputable
  }


end DocGen4.Process
