import DocGen4.Output.Arg
import DocGen4.Output.Template
import DocGen4.Output.DocString
import DocGen4.Process

namespace DocGen4
namespace Output

open scoped DocGen4.Jsx
open Lean

/--
Render a single field consisting of its documentation, its name and its type as HTML.
-/
def fieldToHtml (f : Process.FieldInfo) : HtmlM Html := do
  let shortName := f.name.componentsRev.head!.toString
  let name := f.name.toString
  let args ← f.args.mapM argToHtml
  if f.isDirect then
    let doc : Array HTML ←
      if let some doc := f.doc then
        let renderedDoc ← docStringToHtml doc name
        pure #[<div class="structure_field_doc">[renderedDoc]</div>]
      else
        pure #[]
    pure
      <li id={name} class="structure_field">
        <div class="structure_field_info">{shortName} [args] {" : "} [← renderedCodeToHtml f.type]</div>
        [doc]
      </li>
  else
    pure
      <li id={name} class="structure_field inherited_field">
        <div class="structure_field_info"><a href={← declNameToLink f.name}>{shortName}</a> [args] {" : "} [← renderedCodeToHtml f.type]</div>
      </li>

/--
Render all information about a structure as HTML.
-/
def structureToHtml (i : Process.StructureInfo) : HtmlM (Array Html) := do
  let structureHtml ← do
    if Name.isSuffixOf `mk i.ctor.name then
      pure
        <ul class="structure_fields" id={i.ctor.name.toString}>
          [← i.fieldInfo.mapM fieldToHtml]
        </ul>
    else
      let ctorShortName := i.ctor.name.componentsRev.head!.toString
      pure
        <ul class="structure_ext">
          <li id={i.ctor.name.toString} class="structure_ext_ctor">{s!"{ctorShortName} "} :: (</li>
          <ul class="structure_ext_fields">
            [← i.fieldInfo.mapM fieldToHtml]
          </ul>
          <li class="structure_ext_ctor">)</li>
        </ul>
  return #[structureHtml]

end Output
end DocGen4
