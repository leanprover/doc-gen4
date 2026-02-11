/-
Copyright (c) 2021 Wojciech Nawrocki. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.

Authors: Wojciech Nawrocki, Sebastian Ullrich, Henrik Böving
-/
import Lean.Data.Json
import Lean.Data.Xml
import Lean.Parser
import Lean.Elab.Term

/-! This module defines:
- streaming HTML write helpers
- a JSX-like DSL that expands to monadic writes -/

namespace DocGen4

open Lean

/-- A raw HTML string that should not be escaped. -/
structure Raw where
  html : String

def escapePairs : Array (String × String) :=
  #[
    ("&", "&amp;"),
    ("<", "&lt;"),
    (">", "&gt;"),
    ("\"", "&quot;")
  ]

def escape (s : String) : String :=
  escapePairs.foldl (fun acc (o, r) => acc.replace o r) s

namespace Html
def escape := @DocGen4.escape
end Html

-- TODO: remove the following 3 functions
-- once <https://github.com/leanprover/lean4/issues/4411> is fixed

def _root_.Lean.Xml.Attributes.toStringEscaped (as : Xml.Attributes) : String :=
  as.foldl (fun s n v => s ++ s!" {n}=\"{DocGen4.escape v}\"") ""

mutual

partial def _root_.Lean.Xml.eToStringEscaped : Xml.Element → String
| .Element n a c => s!"<{n}{a.toStringEscaped}>{c.map cToStringEscaped |>.foldl (· ++ ·) ""}</{n}>"

partial def _root_.Lean.Xml.cToStringEscaped : Xml.Content → String
| .Element e => eToStringEscaped e
| .Comment c => s!"<!--{c}-->"
| .Character c => DocGen4.escape c

end

mutual

partial def _root_.Lean.Xml.eToPlaintext : Xml.Element → String
| .Element _ _ c => s!"{c.map cToPlaintext |>.foldl (· ++ ·) ""}"

partial def _root_.Lean.Xml.cToPlaintext : Xml.Content → String
| .Element e => eToPlaintext e
| .Comment _ => ""
| .Character c => c

end

namespace Jsx
open Parser PrettyPrinter

declare_syntax_cat jsxElement
declare_syntax_cat jsxChild

-- JSXTextCharacter : SourceCharacter but not one of {, <, > or }
def jsxText : Parser :=
  withAntiquot (mkAntiquot "jsxText" `jsxText) {
    fn := fun c s =>
      let startPos := s.pos
      let s := takeWhile1Fn (!"[{<>}]$".contains ·) "expected JSX text" c s
      mkNodeToken `jsxText startPos true c s }

@[combinator_formatter DocGen4.Jsx.jsxText] def jsxText.formatter : Formatter := pure ()
@[combinator_parenthesizer DocGen4.Jsx.jsxText] def jsxText.parenthesizer : Parenthesizer := pure ()

syntax jsxAttrName := rawIdent <|> str
syntax jsxAttrVal := str <|> group("{" term "}")
syntax jsxSimpleAttr := jsxAttrName "=" jsxAttrVal
syntax jsxAttrSpread := "[" term "]"
syntax jsxAttr := jsxSimpleAttr <|> jsxAttrSpread

syntax "<" rawIdent jsxAttr* "/>" : jsxElement
syntax "<" rawIdent jsxAttr* ">" jsxChild* "</" rawIdent ">" : jsxElement

syntax jsxText      : jsxChild
syntax "{" term "}" : jsxChild
syntax "[" term "]" : jsxChild
syntax jsxElement   : jsxChild

scoped syntax:max jsxElement : term

def translateAttrs (attrs : Array (TSyntax `DocGen4.Jsx.jsxAttr)) : MacroM (TSyntax `term) := do
  let mut as ← `(#[])
  for attr in attrs.map TSyntax.raw do
    as ← match attr with
    | `(jsxAttr| $n:jsxAttrName=$v:jsxAttrVal) =>
      let n ← match n with
        | `(jsxAttrName| $n:str) => pure n
        | `(jsxAttrName| $n:ident) => pure <| quote (toString n.getId)
        | _ => Macro.throwUnsupported
      let v ← match v with
        | `(jsxAttrVal| {$v}) => pure v
        | `(jsxAttrVal| $v:str) => pure v
        | _ => Macro.throwUnsupported
      `(($as).push ($n, ($v : String)))
    | `(jsxAttr| [$t]) => `($as ++ ($t : Array (String × String)))
    | _ => Macro.throwUnsupported
  return as

private def mkN (s : String) : Lean.Ident :=
  mkIdent (`DocGen4 ++ `Output ++ Name.mkSimple s)

private def mkApp (fn : Ident) (args : Array (TSyntax `term)) : MacroM (TSyntax `term) := do
  args.foldlM (fun acc arg => `($acc $arg)) (fn : TSyntax `term)

macro_rules
  | `(<$n $attrs* />) => do
    let tag : TSyntax `term := quote (toString n.getId)
    let atSyn ← translateAttrs attrs
    let pot := mkN "putOpenTag"
    let pct := mkN "putCloseTag"
    let openCall ← mkApp pot #[tag, atSyn]
    let closeCall ← mkApp pct #[tag]
    `(do ($openCall); ($closeCall))
  | `(<$n $attrs* >$children*</$m>) => do
    unless n.getId == m.getId do
      withRef m <| Macro.throwError s!"Leading and trailing part of tags don't match: '{n}', '{m}'"
    let atSyn ← translateAttrs attrs
    let tag : TSyntax `term := quote (toString n.getId)
    let pot := mkN "putOpenTag"
    let pct := mkN "putCloseTag"
    let pe := mkN "putEscaped"
    let openCall ← mkApp pot #[tag, atSyn]
    let closeCall ← mkApp pct #[tag]
    let mut stmts : Array (TSyntax `Lean.Parser.Term.doSeqItem) := #[]
    stmts := stmts.push (← `(Lean.Parser.Term.doSeqItem| ($openCall)))
    for child in children do
      let stmt ← match child with
      | `(jsxChild|$t:jsxText)    =>
        let s : TSyntax `term := quote t.raw[0]!.getAtomVal
        let call ← mkApp pe #[s]
        `(Lean.Parser.Term.doSeqItem| ($call))
      | `(jsxChild|{$t})          =>
        `(Lean.Parser.Term.doSeqItem| ($t))
      | `(jsxChild|[$t])          =>
        `(Lean.Parser.Term.doSeqItem| for _x in ($t : Array _) do _x)
      | `(jsxChild|$e:jsxElement) =>
        `(Lean.Parser.Term.doSeqItem| $e:jsxElement)
      | _                         => Macro.throwUnsupported
      stmts := stmts.push stmt
    stmts := stmts.push (← `(Lean.Parser.Term.doSeqItem| ($closeCall)))
    `(do $stmts*)


end Jsx

end DocGen4
