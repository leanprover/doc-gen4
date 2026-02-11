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
open DocGen4 (Raw)

/--
Render the structures this structure extends from as HTML so it can be
added to the top level.
-/
def structureInfoHeader (s : Process.StructureInfo) : HtmlM Unit := do
  if s.parents.size > 0 then
    (<span class="decl_extends">extends</span>)
    Html.text " "
    for parent in s.parents, i in [0:s.parents.size] do
      if i > 0 then
        Html.text ", "
      renderedCodeToHtml parent.type

/--
Render the general header of a declaration containing its declaration type
and name.
-/
def docInfoHeader (doc : DocInfo) : HtmlM Unit := do
  <div class="decl_header">
    <span class="decl_kind">{doc.getKindDescription}</span>
    {if doc.getSorried then
      (<span class="decl_name" title="declaration uses 'sorry'">
        {" "}{declNameToHtmlBreakWithinLink doc.getName}{" "}
      </span>)
    else
      (<span class="decl_name">
        {" "}{declNameToHtmlBreakWithinLink doc.getName}{" "}
      </span>)}
    {doc.getArgs.forM argToHtml}
    {match doc with
    | DocInfo.structureInfo i => structureInfoHeader i
    | DocInfo.classInfo i => structureInfoHeader i
    | _ => pure ()}
    <span class="decl_args"> :</span>
    <div class="decl_type">{renderedCodeToHtml doc.getType}</div>
  </div>

/--
The main entry point for rendering a single declaration inside a given module.
-/
def docInfoToHtml (module : Name) (doc : DocInfo) : HtmlM Unit := do
  -- basic info like headers, types, structure fields, etc.
  let docInfoHtml : HtmlM Unit := match doc with
  | DocInfo.inductiveInfo i => inductiveToHtml i
  | DocInfo.structureInfo i => structureToHtml i
  | DocInfo.classInfo i => classToHtml i
  | DocInfo.classInductiveInfo i => classInductiveToHtml i
  | _ => pure ()
  -- rendered doc string
  let docStringHtml : HtmlM Unit := match doc.getDocString with
  | some s => docStringToHtml s doc.getName.toString
  | none => pure ()
  -- extra information like equations and instances
  let extraInfoHtml : HtmlM Unit := match doc with
  | DocInfo.classInfo i => do classInstancesToHtml i.name
  | DocInfo.definitionInfo i => do equationsToHtml i; instancesForToHtml i.name
  | DocInfo.instanceInfo i => equationsToHtml i.toDefinitionInfo
  | DocInfo.classInductiveInfo i => classInstancesToHtml i.name
  | DocInfo.inductiveInfo i => instancesForToHtml i.name
  | DocInfo.structureInfo i => instancesForToHtml i.name
  | _ => pure ()
  let attrs := doc.getAttrs
  let attrsHtml : HtmlM Unit :=
    if attrs.size > 0 then do
      let attrStr := "@[" ++ String.intercalate ", " doc.getAttrs.toList ++ "]"
      (<div class="attributes">{attrStr}</div>)
    else
      pure ()
  -- custom decoration (e.g., verification badges from external tools)
  let decorator ← getDeclarationDecorator
  let cssClass := "decl" ++ if doc.getSorried then " sorried" else ""
  (<div class={cssClass} id={doc.getName.toString}>
    <div class={doc.getKind}>
      <div class="gh_link">
        <a href={← getSourceUrl module doc.getDeclarationRange}>source</a>
      </div>
      {decorator module doc.getName doc.getKind}
      {attrsHtml}
      {docInfoHeader doc}
      {docStringHtml}
      {docInfoHtml}
      {extraInfoHtml}
    </div>
  </div>)

/--
Rendering a module doc string, that is the ones with an ! after the opener
as HTML.
-/
def modDocToHtml (mdoc : ModuleDoc) : HtmlM Unit := do
  <div class="mod_doc">
    {docStringToHtml (.inl mdoc.doc) ""}
  </div>

/--
Render a module member, that is either a module doc string or a declaration
as HTML.
-/
def moduleMemberToHtml (module : Name) (member : ModuleMember) : HtmlM Unit := do
  match member with
  | ModuleMember.docInfo d => docInfoToHtml module d
  | ModuleMember.modDoc d => modDocToHtml d

def declarationToNavLink [Monad m] [MonadReaderOf SiteBaseContext m] [MonadLiftT IO m] (module : Name) : m Unit := do
  <div class="nav_link">
    <a class="break_within" href={s!"#{module.toString}"}>
      {breakWithin module.toString}
    </a>
  </div>

/--
Returns the list of all imports this module does.
-/
def getImports (module : Name) : HtmlM (Array Name) := do
  let res ← getResult
  return res.moduleInfo[module]!.imports

/--
Sort the list of all modules this one is importing, linkify it
and return the HTML.
-/
def importsHtml (moduleName : Name) : HtmlM Unit := do
  let imports := (← getImports moduleName).qsort Name.lt
  for i in imports do
    <li>{moduleToHtmlLink i}</li>

/--
Render the internal nav bar (the thing on the right on all module pages).
-/
def internalNav (members : Array Name) (moduleName : Name) : HtmlM Unit := do
  <nav class="internal_nav">
    <p><a href="#top">return to top</a></p>
    <p class="gh_nav_link"><a href={← getSourceUrl moduleName none}>source</a></p>
    <div class="imports">
      <details>
        <summary>Imports</summary>
        <ul>
          {importsHtml moduleName}
        </ul>
      </details>
      <details>
        <summary>Imported by</summary>
        <ul id={s!"imported-by-{moduleName}"} class="imported-by-list"> </ul>
      </details>
    </div>
    {members.forM declarationToNavLink}
  </nav>

/--
The main entry point to rendering the HTML for an entire module.
-/
def moduleToHtml (module : Process.Module) : HtmlM Unit := withTheReader SiteBaseContext (setCurrentName module.name) do
  let relevantMembers := module.members.filter Process.ModuleMember.shouldRender
  let memberNames := filterDocInfo relevantMembers |>.map DocInfo.getName
  let siteCtx ← readThe SiteContext
  let stateRef ← IO.mkRef (← get)
  liftM (baseHtmlGenerator module.name.toString do
    runHtmlInBase (internalNav memberNames module.name) siteCtx stateRef
    (<main>
      {relevantMembers.forM fun member =>
        runHtmlInBase (moduleMemberToHtml module.name member) siteCtx stateRef}
    </main>)
    : BaseHtmlM Unit)
  set (← stateRef.get)

end Output
end DocGen4
