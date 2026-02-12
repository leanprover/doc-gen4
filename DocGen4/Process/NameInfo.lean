/-
Copyright (c) 2022 Henrik Böving. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Henrik Böving
-/
import Lean

import DocGen4.Process.Base
import DocGen4.Process.Attributes
import DocGen4.RenderedCode

namespace DocGen4.Process
open Lean Meta

open Lean.Parser.Tactic.Doc in
/-- Gets the rendered extensions for the given canonical tactic name as Verso content -/
def getTacticExtensionText (env : Environment) (tactic : Name) : Option (Doc.Block ElabInline ElabBlock) :=
  let exts := getTacticExtensions env tactic
  if exts.size == 0 then none
  else
    some <| .concat #[
      .para #[.text "Extensions:"],
      .ul <| exts.map (⟨#[.para #[.text ·]]⟩)
    ]

open Lean.Parser.Term.Doc in
/--
Renders the recommended spellings for the given declaration into Verso content for appending to
the docstring.
-/
def getRecommendedSpellingText (env : Environment) (declName : Name) : Option (Doc.Block ElabInline ElabBlock) := Id.run do
  let spellings := getRecommendedSpellingsForName env declName
  if spellings.size == 0 then none
  else some <| .concat #[
    .para #[.text "Conventions for notations in identifiers:"],
    .ul (spellings.map bullet)
  ]
where
  bullet (spelling : RecommendedSpelling) : Doc.ListItem (Doc.Block ElabInline ElabBlock) :=
    let firstLine : Array (Doc.Inline ElabInline) := #[
      .text "The recommended spelling of ",
      .code spelling.«notation»,
      .text " in identifiers is ",
      .code spelling.recommendedSpelling
    ]
    let additionalInfoLines := spelling.additionalInformation?.map (·.split '\n' |>.toStringList)
    .mk <| (#[.para ·]) <| match additionalInfoLines with
    | none | some [] => firstLine ++ #[.text ".", .linebreak "\n", .linebreak "\n"]
    | some [l] => firstLine ++ #[.text s!" ({l.trimAsciiEnd}).", .linebreak "\n", .linebreak "\n"]
    | some ls => firstLine ++ #[.text ".", .linebreak "\n", .linebreak "\n", .text (String.join ls), .linebreak "\n", .linebreak "\n"]


open Lean.Parser.Tactic.Doc in
open Lean.Parser.Term.Doc in
def getDocString? (env : Environment) (name : Name) : IO (Option (String ⊕ VersoDocString)) := do
  let name := alternativeOfTactic env name |>.getD name
  match (← findInternalDocString? env name) with
  | none => return none
  | some (.inr verso) =>
    let exts := getTacticExtensionText env name |>.map (#[·]) |>.getD #[]
    let spellings := getRecommendedSpellingText env name |>.map (#[·]) |>.getD #[]
    return some <| .inr <| { verso with text := verso.text ++ exts ++ spellings }
  | some (.inl _) =>
    return (·.map .inl) (← Lean.findDocString? env name)


def NameInfo.ofTypedName (n : Name) (t : Expr) : MetaM NameInfo := do
  let env ← getEnv
  return { name := n, type := ← prettyPrintTerm t, doc := ← getDocString? env n}

/--
Pretty prints a `Lean.Parser.Term.bracketedBinder`.
-/
private def prettyPrintBinder (stx : Syntax) (infos : SubExpr.PosMap Elab.Info) : MetaM RenderedCode := do
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
  return renderTagged (← Widget.tagCodeInfos ctx infos tt)

private def prettyPrintTermStx (stx : Term) (infos : SubExpr.PosMap Elab.Info) : MetaM RenderedCode := do
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
  return renderTagged (← Widget.tagCodeInfos ctx infos tt)

def Info.ofTypedName (n : Name) (t : Expr) : MetaM Info := do
  -- Use the main signature delaborator. We need to run sanitization, parenthesization, and formatting ourselves
  -- to be able to extract the pieces of the signature right before they are formatted
  -- and then format them individually.
  let (sigStx, infos) ← withTheReader Core.Context ({ · with currNamespace := n.getPrefix }) <|
    PrettyPrinter.delabCore t (delab := PrettyPrinter.Delaborator.delabForallParamsWithSignature fun binders type =>
      -- Use `declSig` as a data structure so that the binders and type can be put through the sanitizer all together.
      `(declSig| $binders* : $type))
  let sigStx := (sanitizeSyntax sigStx).run' { options := (← getOptions) }
  let sigStx ← PrettyPrinter.parenthesize Parser.Command.declSig.parenthesizer sigStx
  let `(declSig| $binders* : $type) := sigStx
    | throwError "signature pretty printer failure for {n}"
  let args ← binders.mapM fun binder => do
    let fmt ← prettyPrintBinder binder infos
    return Arg.mk fmt (!binder.isOfKind ``Parser.Term.explicitBinder)
  let type ← prettyPrintTermStx type infos
  match ← findDeclarationRanges? n with
  -- TODO: Maybe selection range is more relevant? Figure this out in the future
  | some range =>
    return {
      toNameInfo := { name := n, type, doc := ← getDocString? (← getEnv) n},
      args,
      declarationRange := range.range,
      attrs := ← getAllAttributes n
    }
  | none => panic! s!"{n} is a declaration without position"

def Info.ofConstantVal (v : ConstantVal) : MetaM Info := do
  let e := Expr.const v.name (v.levelParams.map mkLevelParam)
  ofTypedName v.name (← inferType e)
end DocGen4.Process
