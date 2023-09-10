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

partial def stripArgs (e : Expr) : Expr :=
  match e.consumeMData with
  | Expr.forallE name _ body _ =>
    let name := name.eraseMacroScopes
    stripArgs (Expr.instantiate1 body (mkFVar ⟨name⟩))
  | _ => e

def processEq (eq : Name) : MetaM CodeWithInfos := do
  let type ← (mkConstWithFreshMVarLevels eq >>= inferType)
  let final := stripArgs type
  prettyPrintTerm final

def DefinitionInfo.ofDefinitionVal (v : DefinitionVal) : MetaM DefinitionInfo := do
  let info ← Info.ofConstantVal v.toConstantVal
  let isUnsafe := v.safety == DefinitionSafety.unsafe
  let isNonComputable := isNoncomputable (← getEnv) v.name
  try
    let eqs? ← getEqnsFor? v.name
    let equations ← eqs?.mapM (·.mapM processEq)
    return {
      toInfo := info,
      isUnsafe,
      hints := v.hints,
      equations,
      isNonComputable
    }
  catch err =>
    IO.println s!"WARNING: Failed to calculate equational lemmata for {v.name}: {← err.toMessageData.toString}"
    return {
      toInfo := info,
      isUnsafe,
      hints := v.hints,
      equations := none,
      isNonComputable
    }



end DocGen4.Process
