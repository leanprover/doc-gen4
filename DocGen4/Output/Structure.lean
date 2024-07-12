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
def fieldToHtml (f : Process.NameInfo) : HtmlM Html := do
  let shortName := f.name.componentsRev.head!.toString
  let name := f.name.toString
  if let some doc := f.doc then
    let renderedDoc ← docStringToHtml doc name
    pure
      <li id={name} class="structure_field">
        <div class="structure_field_info">{s!"{shortName} "} : [← infoFormatToHtml f.type]</div>
        <div class="structure_field_doc">[renderedDoc]</div>
      </li>
  else
    pure
      <li id={name} class="structure_field">
        <div class="structure_field_info">{s!"{shortName} "} : [← infoFormatToHtml f.type]</div>
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
