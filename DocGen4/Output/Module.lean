/-
Copyright (c) 2021 Henrik Böving. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Henrik Böving
-/
import Lean
import Lean.PrettyPrinter
import Lean.Widget.TaggedText

import DocGen4.ToHtmlFormat
import DocGen4.Output.Template

namespace DocGen4
namespace Output

open scoped DocGen4.Jsx
open Lean PrettyPrinter Widget Elab

def declNameToLink (name : Name) : HtmlM String := do
  let res ← getResult
  let module := res.moduleNames[res.name2ModIdx.find! name]
  (←moduleNameToLink module) ++ "#" ++ name.toString

def splitWhitespaces (s : String) : (String × String × String) := Id.run do
  let front := "".pushn ' ' (s.find (!Char.isWhitespace ·))
  let mut s := s.trimLeft
  let back := "".pushn ' ' (s.length - s.offsetOfPos (s.find Char.isWhitespace))
  s:= s.trimRight
  (front, s, back)

partial def infoFormatToHtml (i : CodeWithInfos) : HtmlM (Array Html) := do
  match i with
  | TaggedText.text t => return #[t]
  | TaggedText.append tt => tt.foldlM (λ acc t => do acc ++ (←infoFormatToHtml t)) #[]
  | TaggedText.tag a t =>
    match a.info.val.info with
    | Info.ofTermInfo i =>
      match i.expr.consumeMData with
      | Expr.const name _ _ =>
         match t with
         | TaggedText.text t =>
           let (front, t, back) := splitWhitespaces t
           let elem := Html.element "a" true #[("href", ←declNameToLink name)] #[t]
           #[Html.text front, elem, Html.text back]
         | _ =>
           -- TODO: Is this ever reachable?
           #[Html.element "a" true #[("href", ←declNameToLink name)] (←infoFormatToHtml t)]
      | _ =>
         #[Html.element "span" true #[("class", "fn")] (←infoFormatToHtml t)]
    | _ => #[Html.element "span" true #[("class", "fn")] (←infoFormatToHtml t)]

def argToHtml (arg : Arg) : HtmlM Html := do
  let (l, r, implicit) := match arg.binderInfo with
  | BinderInfo.default => ("(", ")", false)
  | BinderInfo.implicit => ("{", "}", true)
  | BinderInfo.strictImplicit => ("⦃", "⦄", true)
  | BinderInfo.instImplicit => ("[", "]", true)
  -- TODO: Can this ever be reached here? What does it mean?
  | BinderInfo.auxDecl => unreachable!
  let mut nodes := #[Html.text s!"{l}{arg.name.toString} : "]
  nodes := nodes.append (←infoFormatToHtml arg.type)
  nodes := nodes.push r
  let inner := Html.element "span" true #[("class", "fn")] nodes
  let html := Html.element "span" false #[("class", "decl_args")] #[inner]
  if implicit then
    <span «class»="impl_arg">{html}</span>
  else
    html

def docInfoHeader (doc : DocInfo) : HtmlM Html := do
  let mut nodes := #[]
  -- TODO: noncomputable, partial
  -- TODO: Support all the kinds in CSS
  nodes := nodes.push <span «class»="decl_kind">{doc.getKind}</span>
  nodes := nodes.push
    <span «class»="decl_name">
      <a «class»="break_within" href={←declNameToLink doc.getName}>
        -- TODO: HTMLify the name
        {doc.getName.toString}
      </a>
    </span>
  for arg in doc.getArgs do
    nodes := nodes.push (←argToHtml arg)
  nodes := nodes.push <span «class»="decl_args">:</span>
  nodes := nodes.push $ Html.element "div" true #[("class", "decl_type")] (←infoFormatToHtml doc.getType)
  -- TODO: The final type of the declaration
  return <div «class»="decl_header"> [nodes] </div>

def docInfoToHtml (doc : DocInfo) : HtmlM Html := do
  <div «class»="decl" id={doc.getName.toString}>
    <div «class»={doc.getKind}>
      <div «class»="gh_link">
        -- TODO: Put the proper source link
        <a href="https://github.com">source</a>
      </div>
      -- TODO: Attributes
      {←docInfoHeader doc}
      -- TODO: The actual type information we are here for
    </div>
  </div>

def moduleToHtml (module : Module) : HtmlM Html := withReader (setCurrentName module.name) do
  -- TODO: Probably some sort of ordering by line number would be cool?
  -- maybe they should already be ordered in members.
  let docInfos ← module.members.mapM docInfoToHtml
  -- TODO: This is missing imports, imported by, source link, list of decls
  templateExtends (baseHtml module.name.toString) $
    Html.element "main" false #[] docInfos

end Output
end DocGen4
