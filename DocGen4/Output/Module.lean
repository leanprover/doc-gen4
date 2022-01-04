/-
Copyright (c) 2021 Henrik Böving. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Henrik Böving
-/
import DocGen4.Output.Template
import DocGen4.Output.Inductive
import DocGen4.Output.Structure

namespace DocGen4
namespace Output

open scoped DocGen4.Jsx
open Lean

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

def structureInfoHeader (s : StructureInfo) : HtmlM (Array Html) := do
  let mut nodes := #[]
  if s.parents.size > 0 then
    nodes := nodes.push <span «class»="decl_extends">extends</span>
    let mut parents := #[]
    for parent in s.parents do
      let link := <a «class»="break_within" href={←declNameToLink parent}>{parent.toString}</a>
      let inner := Html.element "span" true #[("class", "fn")] #[link]
      let html:= Html.element "span" false #[("class", "decl_parent")] #[inner]
      parents := parents.push html
    nodes := nodes.append (parents.toList.intersperse (Html.text ", ")).toArray
  nodes

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

  if let DocInfo.structureInfo i := doc then
    nodes := nodes.append (←structureInfoHeader i)

  nodes := nodes.push <span «class»="decl_args">:</span>
  nodes := nodes.push $ Html.element "div" true #[("class", "decl_type")] (←infoFormatToHtml doc.getType)
  -- TODO: The final type of the declaration
  return <div «class»="decl_header"> [nodes] </div>

def docInfoToHtml (doc : DocInfo) : HtmlM Html := do
  let doc_html ← match doc with
  | DocInfo.inductiveInfo i => inductiveToHtml i
  | DocInfo.structureInfo i => structureToHtml i
  | _ => #[]

  return <div «class»="decl" id={doc.getName.toString}>
    <div «class»={doc.getKind}>
      <div «class»="gh_link">
        -- TODO: Put the proper source link
        <a href="https://github.com">source</a>
      </div>
      -- TODO: Attributes
      {←docInfoHeader doc}
      [←doc_html]
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
