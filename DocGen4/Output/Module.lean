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
import DocGen4.Process
import Lean.Data.Xml.Parser

namespace DocGen4
namespace Output

open scoped DocGen4.Jsx
open Lean Process

/--
Render an `Arg` as HTML, adding opacity effects etc. depending on what
type of binder it has.
-/
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
  let inner := <span class="fn">[nodes]</span>
  let html := Html.element "span" false #[("class", "decl_args")] #[inner]
  if implicit then
    pure <span class="impl_arg">{html}</span>
  else
    pure html

/--
Render the structures this structure extends from as HTML so it can be
added to the top level.
-/
def structureInfoHeader (s : Process.StructureInfo) : HtmlM (Array Html) := do
  let mut nodes := #[]
  if s.parents.size > 0 then
    nodes := nodes.push <span class="decl_extends">extends</span>
    let mut parents := #[]
    for parent in s.parents do
      let link ← declNameToHtmlBreakWithinLink parent
      let inner := <span class="fn">{link}</span>
      let html:= Html.element "span" false #[("class", "decl_parent")] #[inner]
      parents := parents.push html
    nodes := nodes.append (parents.toList.intersperse (Html.text ", ")).toArray
  pure nodes

/--
Render the general header of a declaration containing its declaration type
and name.
-/
def docInfoHeader (doc : DocInfo) : HtmlM Html := do
  let mut nodes := #[]
  nodes := nodes.push <| Html.element "span" false #[("class", "decl_kind")] #[doc.getKindDescription]
  nodes := nodes.push
    <span class="decl_name">
      <a class="break_within" href={←declNameToLink doc.getName}>
        -- TODO: HTMLify the name
        {doc.getName.toString}
      </a>
    </span>
  for arg in doc.getArgs do
    nodes := nodes.push (←argToHtml arg)

  match doc with
  | DocInfo.structureInfo i => nodes := nodes.append (←structureInfoHeader i)
  | DocInfo.classInfo i => nodes := nodes.append (←structureInfoHeader i)
  | _ => nodes := nodes

  nodes := nodes.push <| Html.element "span" true #[("class", "decl_args")] #[" :"]
  nodes := nodes.push <div class="decl_type">[←infoFormatToHtml doc.getType]</div>
  pure <div class="decl_header"> [nodes] </div>

/--
The main entry point for rendering a single declaration inside a given module.
-/
def docInfoToHtml (module : Name) (doc : DocInfo) : HtmlM Html := do
  -- basic info like headers, types, structure fields, etc.
  let docInfoHtml ← match doc with
  | DocInfo.inductiveInfo i => inductiveToHtml i
  | DocInfo.structureInfo i => structureToHtml i
  | DocInfo.classInfo i => classToHtml i
  | DocInfo.classInductiveInfo i => classInductiveToHtml i
  | _ => pure #[]
  -- rendered doc stirng
  let docStringHtml ← match doc.getDocString with
  | some s => docStringToHtml s
  | none => pure #[]
  -- extra information like equations and instances
  let extraInfoHtml ← match doc with
  | DocInfo.classInfo i => pure #[←classInstancesToHtml i.name]
  | DocInfo.definitionInfo i => equationsToHtml i
  | DocInfo.instanceInfo i => equationsToHtml i.toDefinitionInfo
  | DocInfo.classInductiveInfo i => pure #[←classInstancesToHtml i.name]
  | DocInfo.inductiveInfo i => pure #[←instancesForToHtml i.name]
  | DocInfo.structureInfo i => pure #[←instancesForToHtml i.name]
  | _ => pure #[]
  let attrs := doc.getAttrs
  let attrsHtml :=
    if attrs.size > 0 then
      let attrStr := "@[" ++ String.intercalate ", " doc.getAttrs.toList ++ "]"
      #[Html.element "div" false #[("class", "attributes")] #[attrStr]]
    else
      #[]
  let leanInkHtml :=
    if ←leanInkEnabled? then
      #[
        <div class="ink_link">
          <a href={←declNameToInkLink doc.getName}>ink</a>
        </div>
      ]
    else
      #[]

  pure
    <div class="decl" id={doc.getName.toString}>
      <div class={doc.getKind}>
        <div class="gh_link">
          <a href={←getSourceUrl module doc.getDeclarationRange}>source</a>
        </div>
        [leanInkHtml]
        [attrsHtml]
        {←docInfoHeader doc}
        [docInfoHtml]
        [docStringHtml]
        [extraInfoHtml]
      </div>
    </div>

/--
Rendering a module doc string, that is the ones with an ! after the opener
as HTML.
-/
def modDocToHtml (mdoc : ModuleDoc) : HtmlM Html := do
  pure 
    <div class="mod_doc">
      [←docStringToHtml mdoc.doc]
    </div>

/--
Render a module member, that is either a module doc string or a declaration
as HTML.
-/
def moduleMemberToHtml (module : Name) (member : ModuleMember) : HtmlM Html := do
  match member with
  | ModuleMember.docInfo d => docInfoToHtml module d
  | ModuleMember.modDoc d => modDocToHtml d

def declarationToNavLink (module : Name) : Html :=
  <div class="nav_link">
    <a class="break_within" href={s!"#{module.toString}"}>{module.toString}</a>
  </div>

/--
Returns the list of all imports this module does.
-/
def getImports (module : Name) : HtmlM (Array Name) := do
  let res ← getResult
  pure <| res.moduleInfo.find! module |>.imports

/--
Sort the list of all modules this one is importing, linkify it
and return the HTML.
-/
def importsHtml (moduleName : Name) : HtmlM (Array Html) := do
  let imports := (←getImports moduleName) |>.qsort Name.lt
  imports.mapM (λ i => do pure <li>{←moduleToHtmlLink i}</li>)

/--
Render the internal nav bar (the thing on the right on all module pages).
-/
def internalNav (members : Array Name) (moduleName : Name) : HtmlM Html := do
  pure
    <nav class="internal_nav">
      <h3><a class="break_within" href="#top">{moduleName.toString}</a></h3>
      <p class="gh_nav_link"><a href={←getSourceUrl moduleName none}>source</a></p>
      <div class="imports">
        <details>
          <summary>Imports</summary>
          <ul>
            [←importsHtml moduleName]
          </ul>
        </details>
        <details>
          <summary>Imported by</summary>
          <ul id={s!"imported-by-{moduleName}"} class="imported-by-list"> </ul>
        </details>
      </div>
      [members.map declarationToNavLink]
    </nav>

/--
The main entry point to rendering the HTML for an entire module.
-/
def moduleToHtml (module : Process.Module) : HtmlM Html := withTheReader SiteBaseContext (setCurrentName module.name) do
  let memberDocs ← module.members.mapM (λ i => moduleMemberToHtml module.name i)
  let memberNames := filterMapDocInfo module.members |>.map DocInfo.getName
  templateLiftExtends (baseHtmlGenerator module.name.toString) <| pure #[
    ←internalNav memberNames module.name,
    Html.element "main" false #[] memberDocs
  ]

end Output
end DocGen4
