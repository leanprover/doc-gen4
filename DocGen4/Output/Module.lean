/-
Copyright (c) 2021 Henrik Böving. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Henrik Böving
-/
import DocGen4.Output.Template
import DocGen4.Output.Inductive
import DocGen4.Output.Structure
import DocGen4.Output.Class
import DocGen4.Output.Definition
import DocGen4.Output.Instance
import DocGen4.Output.ClassInductive
import DocGen4.Output.DocString
import Lean.Data.Xml.Parser

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
    pure <span «class»="impl_arg">{html}</span>
  else
    pure html

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
  pure nodes

def docInfoHeader (doc : DocInfo) : HtmlM Html := do
  let mut nodes := #[]
  nodes := nodes.push <span «class»="decl_kind">{doc.getKindDescription}</span>
  nodes := nodes.push
    <span «class»="decl_name">
      <a «class»="break_within" href={←declNameToLink doc.getName}>
        -- TODO: HTMLify the name
        {doc.getName.toString}
      </a>
    </span>
  for arg in doc.getArgs do
    nodes := nodes.push (←argToHtml arg)

  -- TODO: dedup this
  match doc with
  | DocInfo.structureInfo i => nodes := nodes.append (←structureInfoHeader i)
  | DocInfo.classInfo i => nodes := nodes.append (←structureInfoHeader i.toStructureInfo)
  | _ => nodes := nodes

  nodes := nodes.push <span «class»="decl_args">:</span>
  nodes := nodes.push $ Html.element "div" true #[("class", "decl_type")] (←infoFormatToHtml doc.getType)
  pure <div «class»="decl_header"> [nodes] </div>

def docInfoToHtml (module : Name) (doc : DocInfo) : HtmlM Html := do
  -- basic info like headers, types, structure fields, etc.
  let docInfoHtml ← match doc with
  | DocInfo.inductiveInfo i => inductiveToHtml i
  | DocInfo.structureInfo i => structureToHtml i
  | DocInfo.classInfo i => classToHtml i
  | DocInfo.classInductiveInfo i => classInductiveToHtml i
  | i => pure #[]
  -- rendered doc stirng
  let docStringHtml ← match doc.getDocString with
  | some s => docStringToHtml s
  | none => pure #[]
  -- extra information like equations and instances
  let extraInfoHtml ← match doc with
  | DocInfo.classInfo i => pure #[←classInstancesToHtml i.instances]
  | DocInfo.definitionInfo i => equationsToHtml i
  | DocInfo.instanceInfo i => equationsToHtml i
  | DocInfo.classInductiveInfo i => pure #[←classInstancesToHtml i.instances]
  | i => pure #[]
  let attrs := doc.getAttrs
  let attrsHtml :=
    if attrs.size > 0 then
      let attrStr := "@[" ++ String.intercalate ", " doc.getAttrs.toList ++ "]"
      #[Html.element "div" false #[("class", "attributes")] #[attrStr]]
    else
      #[]

  pure
    <div «class»="decl" id={doc.getName.toString}>
      <div «class»={doc.getKind}>
        <div «class»="gh_link">
          <a href={←getSourceUrl module doc.getDeclarationRange}>source</a>
        </div>
        [attrsHtml]
        {←docInfoHeader doc}
        [docInfoHtml]
        [docStringHtml]
        [extraInfoHtml]
      </div>
    </div>

def modDocToHtml (module : Name) (mdoc : ModuleDoc) : HtmlM Html := do
  pure 
    <div «class»="mod_doc">
      [←docStringToHtml mdoc.doc]
    </div>

def moduleMemberToHtml (module : Name) (member : ModuleMember) : HtmlM Html := do
  match member with
  | ModuleMember.docInfo d => docInfoToHtml module d
  | ModuleMember.modDoc d => modDocToHtml module d

def declarationToNavLink (module : Name) : Html :=
  <div «class»="nav_link">
    <a «class»="break_within" href={s!"#{module.toString}"}>{module.toString}</a>
  </div>

-- TODO: Similar functions are used all over the place, we should dedup them
def moduleToNavLink (module : Name) : HtmlM Html := do
  pure <a href={←moduleNameToLink module}>{module.toString}</a>

def getImports (module : Name) : HtmlM (Array Name) := do
  let res ← getResult
  let some idx := res.moduleNames.findIdx? (. == module) | unreachable!
  let adj := res.importAdj.get! idx
  let mut imports := #[]
  for i in [:adj.size] do
    if adj.get! i then
      imports := imports.push (res.moduleNames.get! i)
  pure imports

def getImportedBy (module : Name) : HtmlM (Array Name) := do
  let res ← getResult
  let some idx := res.moduleNames.findIdx? (. == module) | unreachable!
  let adj := res.importAdj
  let mut impBy := #[]
  for i in [:adj.size] do
    if adj.get! i |>.get! idx then
      impBy := impBy.push (res.moduleNames.get! i)
  pure impBy

def importedByHtml (moduleName : Name) : HtmlM (Array Html) := do
  let imports := (←getImportedBy moduleName) |>.qsort Name.lt
  imports.mapM (λ i => do pure <li>{←moduleToNavLink i}</li>)


def importsHtml (moduleName : Name) : HtmlM (Array Html) := do
  let imports := (←getImports moduleName) |>.qsort Name.lt
  imports.mapM (λ i => do pure <li>{←moduleToNavLink i}</li>)

def internalNav (members : Array Name) (moduleName : Name) : HtmlM Html := do
  pure
    <nav «class»="internal_nav">
      <h3><a «class»="break_within" href="#top">{moduleName.toString}</a></h3>
      <p «class»="gh_nav_link"><a href={←getSourceUrl moduleName none}>source</a></p>
      <div «class»="imports">
        <details>
          <summary>Imports</summary>
          <ul>
            [←importsHtml moduleName]
          </ul>
        </details>
        <details>
          <summary>Imported by</summary>
          <ul>
            [←importedByHtml moduleName]
          </ul>
        </details>
      </div>
      [members.map declarationToNavLink]
    </nav>

def moduleToHtml (module : Module) : HtmlM Html := withReader (setCurrentName module.name) do
  let memberDocs ← module.members.mapM (λ i => moduleMemberToHtml module.name i)
  let memberNames := filterMapDocInfo module.members |>.map DocInfo.getName
  templateExtends (baseHtmlArray module.name.toString) $ pure #[
    ←internalNav memberNames module.name,
    Html.element "main" false #[] memberDocs
  ]

end Output
end DocGen4
