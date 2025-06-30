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
  Widget.tagCodeInfos ctx infos tt

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
  Widget.tagCodeInfos ctx infos tt

/-!
Everything in the section is a hacky backport of code that was modified in https://github.com/leanprover/lean4/pull/8511
It should be removed once we know how to replace this functionality with the new code in that PR.
-/
section adaptations_8511

/-- Don't do any renaming for forall binders, but do add fresh macro scopes when there is shadowing. -/
private def ppPiPreserveNames := `pp.piPreserveNames
/-- Causes non-dependent foralls to print with binder names. -/
private def ppPiBinderNames := `pp.piBinderNames

private unsafe def evalSyntaxConstantUnsafe (env : Environment) (opts : Options) (constName : Name) : ExceptT String Id Syntax :=
  env.evalConstCheck Syntax opts ``Syntax constName

@[implemented_by evalSyntaxConstantUnsafe]
private opaque evalSyntaxConstant (env : Environment) (opts : Options) (constName : Name) : ExceptT String Id Syntax := throw ""

open Lean.Parser.Term Lean.PrettyPrinter.Delaborator SubExpr in
/--
Pretty-prints a constant `c` as `c.{<levels>} <params> : <type>`.

If `universes` is `false`, then the universe level parameters are omitted.
-/
partial def delabConstWithSignature (universes : Bool := true) : Delab := do
  let e ← getExpr
  -- use virtual expression node of arity 2 to separate name and type info
  let idStx ← descend e 0 <|
    withOptions (pp.universes.set · universes |> (pp.fullNames.set · true)) <|
      delabConst
  descend (← inferType e) 1 <|
    delabParams {} ⟨idStx.raw⟩ #[]
where
  /--
  For types in the signature, we want to be sure pi binder types are pretty printed.
  -/
  delabTy : DelabM Term := withOptions (pp.piBinderTypes.set · true) delab
  /-
  Similar to `delabBinders`, but does not uniquify binder names (since for named arguments we want to know the name),
  and it always merges binder groups when possible.
  Once it reaches a binder with an inaccessible name, or a name that has already been used,
  the remaining binders appear in pi types after the `:` of the declaration.
  -/
  delabParams (bindingNames : NameSet) (idStx : Ident) (groups : TSyntaxArray ``bracketedBinder) := do
    let e ← getExpr
    if e.isForall && e.binderInfo.isInstImplicit && e.bindingName!.hasMacroScopes then
      -- Assumption: this instance can be found by instance search, so it does not need to be named.
      -- The oversight here is that this inaccessible name can appear in the pretty printed expression.
      -- We could check to see whether the instance appears in the type and avoid omitting the instance name,
      -- but this would be the usual case.
      let group ← withBindingDomain do `(bracketedBinderF|[$(← delabTy)])
      withBindingBody e.bindingName! <| delabParams bindingNames idStx (groups.push group)
    else if e.isForall && (!e.isArrow || !(e.bindingName!.hasMacroScopes || bindingNames.contains e.bindingName!)) then
      delabParamsAux bindingNames idStx groups #[]
    else
      let (opts', e') ← processSpine {} (← readThe SubExpr)
      withReader (fun ctx => {ctx with optionsPerPos := opts', subExpr := { ctx.subExpr with expr := e' }}) do
        let type ← delabTy
        let r ← `(declSigWithId| $idStx:ident $groups* : $type)
        pure ⟨r.raw⟩
  /--
  Inner loop for `delabParams`, collecting binders.
  Invariants:
  - The current expression is a forall.
  - It has a name that's not inaccessible.
  - It has a name that hasn't been used yet.
  -/
  delabParamsAux (bindingNames : NameSet) (idStx : Ident) (groups : TSyntaxArray ``bracketedBinder) (curIds : Array Ident) := do
    let e@(.forallE n d e' i) ← getExpr | unreachable!
    let n ← if bindingNames.contains n then withFreshMacroScope <| MonadQuotation.addMacroScope n else pure n
    let bindingNames := bindingNames.insert n
    if shouldGroupWithNext bindingNames e e' then
      withBindingBody' n (mkAnnotatedIdent n) fun stxN =>
        delabParamsAux bindingNames idStx groups (curIds.push stxN)
    else
      /-
      `mkGroup` constructs binder syntax for the binder names `curIds : Array Ident`, which all have the same type and binder info.
      This being a function is solving the following issue:
      - To get the last binder name, we need to be under `withBindingBody'`, which lets us annotate the binder with its fvar.
      - However, we should delaborate the binder type from outside `withBindingBody'`.
      - Thus, we need to partially construct the binder syntax, waiting on the final value of `curIds`.
      -/
      let mkGroup : Array Ident → DelabM Syntax ← withBindingDomain do
        match i with
        | .implicit       => let ty ← delabTy; pure fun curIds => `(bracketedBinderF|{$curIds* : $ty})
        | .strictImplicit => let ty ← delabTy; pure fun curIds => `(bracketedBinderF|⦃$curIds* : $ty⦄)
        | .instImplicit   => let ty ← delabTy; pure fun curIds => `(bracketedBinderF|[$(curIds[0]!) : $ty])
        | _ =>
          if d.isOptParam then
            let ty ← withAppFn <| withAppArg delabTy
            let val ← withAppArg delabTy
            pure fun curIds => `(bracketedBinderF|($curIds* : $ty := $val))
          else if let some (.const tacticDecl _) := d.getAutoParamTactic? then
            let ty ← withAppFn <| withAppArg delabTy
            let tacticSyntax := ⟨← ofExcept <| evalSyntaxConstant (← getEnv) (← getOptions) tacticDecl⟩
            pure fun curIds => `(bracketedBinderF|($curIds* : $ty := by $tacticSyntax))
          else
            let ty ← delabTy
            pure fun curIds => `(bracketedBinderF|($curIds* : $ty))
      withBindingBody' n (mkAnnotatedIdent n) fun stxN => do
        let curIds := curIds.push stxN
        let group ← mkGroup curIds
        delabParams bindingNames idStx (groups.push ⟨group⟩)
  /-
  Given the forall `e` with body `e'`, determines if the binder from `e'` (if it is a forall) should be grouped with `e`'s binder.
  -/
  shouldGroupWithNext (bindingNames : NameSet) (e e' : Expr) : Bool :=
    e'.isForall &&
    (!e'.isArrow ||
      -- At the first sign of an inaccessible name, stop merging binders:
    !(e'.bindingName!.hasMacroScopes ||
      -- If it's a name that has already been used, stop merging binders:
      bindingNames.contains e'.bindingName!)) &&
    e.binderInfo == e'.binderInfo &&
    e.bindingDomain! == e'.bindingDomain! &&
    -- Inst implicits can't be grouped:
    e'.binderInfo != BinderInfo.instImplicit
  /--
  Go through rest of type, alpha renaming and setting options along the spine.
  -/
  processSpine (opts : OptionsPerPos) (subExpr : SubExpr) : MetaM (OptionsPerPos × Expr) := do
    if let .forallE n t b bi := subExpr.expr then
      let used := (← getLCtx).usesUserName n
      withLocalDecl n bi t fun fvar => do
        let (opts, b') ← processSpine opts { expr := b.instantiate1 fvar, pos := subExpr.pos.pushBindingBody }
        let b' := b'.abstract #[fvar]
        let opts := opts.insertAt subExpr.pos ppPiPreserveNames true
        if n.hasMacroScopes then
          return (opts, .forallE n t b' bi)
        else if !used then
          let opts := opts.insertAt subExpr.pos ppPiBinderNames true
          return (opts, .forallE n t b' bi)
        else
          let n' ← withFreshMacroScope <| MonadQuotation.addMacroScope n
          return (opts, .forallE n' t b' bi)
    else
      return (opts, subExpr.expr)

end adaptations_8511

def Info.ofTypedName (n : Name) (t : Expr) : MetaM Info := do
  -- Use the main signature delaborator. We need to run sanitization, parenthesization, and formatting ourselves
  -- to be able to extract the pieces of the signature right before they are formatted
  -- and then format them individually.
  let (sigStx, infos) ← withTheReader Core.Context ({ · with currNamespace := n.getPrefix }) <|
    PrettyPrinter.delabCore t (delab := delabConstWithSignature.delabParams {} default #[])
  let sigStx := (sanitizeSyntax sigStx).run' { options := (← getOptions) }
  let sigStx ← PrettyPrinter.parenthesize PrettyPrinter.Delaborator.declSigWithId.parenthesizer sigStx
  let `(PrettyPrinter.Delaborator.declSigWithId| $_:term $binders* : $type) := sigStx
    | throwError "signature pretty printer failure for {n}"
  let args ← binders.mapM fun binder => do
    let fmt ← prettyPrintBinder binder infos
    return Arg.mk fmt (!binder.isOfKind ``Parser.Term.explicitBinder)
  let type ← prettyPrintTermStx type infos
  match ← findDeclarationRanges? n with
  -- TODO: Maybe selection range is more relevant? Figure this out in the future
  | some range =>
    return {
      toNameInfo := { name := n, type, doc := ← findDocString? (← getEnv) n},
      args,
      declarationRange := range.range,
      attrs := ← getAllAttributes n
    }
  | none => panic! s!"{n} is a declaration without position"

def Info.ofConstantVal (v : ConstantVal) : MetaM Info := do
  let e := Expr.const v.name (v.levelParams.map mkLevelParam)
  ofTypedName v.name (← inferType e)

end DocGen4.Process
