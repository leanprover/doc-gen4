/-
Copyright (c) 2022 Henrik Böving. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Henrik Böving
-/
import Lean

import DocGen4.Process.Base
import DocGen4.Process.Attributes

namespace DocGen4.Process
open Lean Meta

def NameInfo.ofTypedName (n : Name) (t : Expr) : MetaM NameInfo := do
  let env ← getEnv
  return { name := n, type := ← prettyPrintTerm t, doc := ← findDocString? env n}

/--
Pretty prints a `Lean.Parser.Term.bracketedBinder`.
-/
private def prettyPrintBinder (stx : Syntax) (infos : SubExpr.PosMap Elab.Info) : MetaM Widget.CodeWithInfos := do
  let fmt ← PrettyPrinter.format Parser.Term.bracketedBinder.formatter stx
  let tt := Widget.TaggedText.prettyTagged fmt
  let ctx := {
    env := ← getEnv
    mctx := ← getMCtx
    options := ← getOptions
    currNamespace := ← getCurrNamespace
    openDecls := ← getOpenDecls
    fileMap := default,
    ngen := ← getNGen
  }
  return Widget.tagCodeInfos ctx infos tt

private def prettyPrintTermStx (stx : Term) (infos : SubExpr.PosMap Elab.Info) : MetaM Widget.CodeWithInfos := do
  let fmt ← PrettyPrinter.formatTerm stx
  let tt := Widget.TaggedText.prettyTagged fmt
  let ctx := {
    env := ← getEnv
    mctx := ← getMCtx
    options := ← getOptions
    currNamespace := ← getCurrNamespace
    openDecls := ← getOpenDecls
    fileMap := default,
    ngen := ← getNGen
  }
  return Widget.tagCodeInfos ctx infos tt

def Info.ofConstantVal (v : ConstantVal) : MetaM Info := do
  let e := Expr.const v.name (v.levelParams.map mkLevelParam)
  -- Use the main signature delaborator. We need to run sanitization, parenthesization, and formatting ourselves
  -- to be able to extract the pieces of the signature right before they are formatted
  -- and then format them individually.
  let (sigStx, infos) ← PrettyPrinter.delabCore e (delab := PrettyPrinter.Delaborator.delabConstWithSignature)
  let sigStx := (sanitizeSyntax sigStx).run' { options := (← getOptions) }
  let sigStx ← PrettyPrinter.parenthesize PrettyPrinter.Delaborator.declSigWithId.parenthesizer sigStx
  let `(PrettyPrinter.Delaborator.declSigWithId| $_:term $binders* : $type) := sigStx
    | throwError "signature pretty printer failure for {v.name}"
  let args ← binders.mapM fun binder => do
    let fmt ← prettyPrintBinder binder infos
    return Arg.mk fmt (!binder.isOfKind ``Parser.Term.explicitBinder)
  let type ← prettyPrintTermStx type infos
  match ← findDeclarationRanges? v.name with
  -- TODO: Maybe selection range is more relevant? Figure this out in the future
  | some range =>
    return {
      toNameInfo := { name := v.name, type, doc := ← findDocString? (← getEnv) v.name},
      args,
      declarationRange := range.range,
      attrs := ← getAllAttributes v.name
    }
  | none => panic! s!"{v.name} is a declaration without position"

end DocGen4.Process
