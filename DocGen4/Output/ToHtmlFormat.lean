/-
Copyright (c) 2021 Wojciech Nawrocki. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.

Authors: Wojciech Nawrocki, Sebastian Ullrich, Henrik Böving
-/
import Lean.Data.Json
import Lean.Data.Xml
import Lean.Parser

/-! This module defines:
- a representation of HTML trees
- together with a JSX-like DSL for writing them
- and widget support for visualizing any type as HTML. -/

namespace DocGen4

open Lean

inductive Html where
  -- TODO(WN): it's nameless for shorter JSON; re-add names when we have deriving strategies for From/ToJson
  -- element (tag : String) (flatten : Bool) (attrs : Array HtmlAttribute) (children : Array Html)
  | element : String → Bool → Array (String × String) → Array Html → Html
  /-- A text node, which will be escaped in the output -/
  | text : String → Html
  /-- An arbitrary string containing HTML -/
  | raw : String → Html
  deriving Repr, BEq, Inhabited, FromJson, ToJson

instance : Coe String Html :=
  ⟨Html.text⟩

namespace Html

def escapePairs : Array (String × String) :=
  #[
    ("&", "&amp;"),
    ("<", "&lt;"),
    (">", "&gt;"),
    ("\"", "&quot;")
  ]

def escape (s : String) : String :=
  escapePairs.foldl (fun acc (o, r) => acc.replace o r) s

-- TODO: remove the following 3 functions
-- once <https://github.com/leanprover/lean4/issues/4411> is fixed

def _root_.Lean.Xml.Attributes.toStringEscaped (as : Xml.Attributes) : String :=
  as.fold (fun s n v => s ++ s!" {n}=\"{Html.escape v}\"") ""

mutual

partial def _root_.Lean.Xml.eToStringEscaped : Xml.Element → String
| .Element n a c => s!"<{n}{a.toStringEscaped}>{c.map cToStringEscaped |>.foldl (· ++ ·) ""}</{n}>"

partial def _root_.Lean.Xml.cToStringEscaped : Xml.Content → String
| .Element e => eToStringEscaped e
| .Comment c => s!"<!--{c}-->"
| .Character c => Html.escape c

end

mutual

partial def _root_.Lean.Xml.eToPlaintext : Xml.Element → String
| .Element _ _ c => s!"{c.map cToPlaintext |>.foldl (· ++ ·) ""}"

partial def _root_.Lean.Xml.cToPlaintext : Xml.Content → String
| .Element e => eToPlaintext e
| .Comment _ => ""
| .Character c => c

end

def attributesToString (attrs : Array (String × String)) :String :=
  attrs.foldl (fun acc (k, v) => acc ++ " " ++ k ++ "=\"" ++ escape v ++ "\"") ""

-- TODO: Termination proof
partial def toStringAux : Html → String
| element tag false attrs #[text s] => s!"<{tag}{attributesToString attrs}>{escape s}</{tag}>\n"
| element tag false attrs #[raw s] => s!"<{tag}{attributesToString attrs}>{s}</{tag}>\n"
| element tag false attrs #[child] => s!"<{tag}{attributesToString attrs}>\n{child.toStringAux}</{tag}>\n"
| element tag false attrs children => s!"<{tag}{attributesToString attrs}>\n{children.foldl (· ++ toStringAux ·) ""}</{tag}>\n"
| element tag true attrs children => s!"<{tag}{attributesToString attrs}>{children.foldl (· ++ toStringAux ·) ""}</{tag}>"
| text s => escape s
| raw s => s

def toString (html : Html) : String :=
  html.toStringAux.trimRight

partial def textLength : Html → Nat
| raw s => s.length  -- measures lengths of escape sequences too!
| text s => s.length
| element _ _ _ children =>
  let lengths := children.map textLength
  lengths.foldl Nat.add 0

end Html

namespace Jsx
open Parser PrettyPrinter

declare_syntax_cat jsxElement
declare_syntax_cat jsxChild

-- JSXTextCharacter : SourceCharacter but not one of {, <, > or }
def jsxText : Parser :=
  withAntiquot (mkAntiquot "jsxText" `jsxText) {
    fn := fun c s =>
      let startPos := s.pos
      let s := takeWhile1Fn (not ∘ "[{<>}]$".contains) "expected JSX text" c s
      mkNodeToken `jsxText startPos c s }

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

private def htmlHelper (n : Syntax) (children : Array Syntax) (m : Syntax) : MacroM (String × (TSyntax `term)):= do
  unless n.getId == m.getId do
    withRef m <| Macro.throwError s!"Leading and trailing part of tags don't match: '{n}', '{m}'"
  let mut cs ← `(#[])
  for child in children do
    cs ← match child with
    | `(jsxChild|$t:jsxText)    => `(($cs).push (Html.text $(quote t.raw[0]!.getAtomVal)))
    -- TODO(WN): elab as list of children if type is `t Html` where `Foldable t`
    | `(jsxChild|{$t})          => `(($cs).push ($t : Html))
    | `(jsxChild|[$t])          => `($cs ++ ($t : Array Html))
    | `(jsxChild|$e:jsxElement) => `(($cs).push ($e:jsxElement : Html))
    | _                         => Macro.throwUnsupported
  let tag := toString n.getId
  pure <| (tag, cs)

macro_rules
  | `(<$n $attrs* />) => do
    let kind := quote (toString n.getId)
    let attrs ← translateAttrs attrs
    `(Html.element $kind true $attrs #[])
  | `(<$n $attrs* >$children*</$m>) => do
    let (tag, children) ← htmlHelper n children m
    `(Html.element $(quote tag) true $(← translateAttrs attrs) $children)

end Jsx

/-- A type which implements `ToHtmlFormat` will be visualized
as the resulting HTML in editors which support it. -/
class ToHtmlFormat (α : Type u) where
  formatHtml : α → Html

end DocGen4
