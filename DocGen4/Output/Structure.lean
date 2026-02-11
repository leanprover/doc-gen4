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
def fieldToHtml (f : Process.FieldInfo) : HtmlM Unit := do
  let shortName := f.name.componentsRev.head!.toString
  let name := f.name.toString
  if f.isDirect then
    if let some doc := f.doc then
      (<li id={name} class="structure_field">
        <div class="structure_field_info">
          {shortName} {f.args.forM argToHtml} {" : "} {renderedCodeToHtml f.type}
        </div>
        <div class="structure_field_doc">{docStringToHtml doc name}</div>
      </li>)
    else
      (<li id={name} class="structure_field">
        <div class="structure_field_info">
          {shortName} {f.args.forM argToHtml} {" : "} {renderedCodeToHtml f.type}
        </div>
      </li>)
  else
    (<li class="structure_field inherited_field">
      <div class="structure_field_info">
        <a href={← declNameToLink f.name}>{shortName}</a>
        {f.args.forM argToHtml} {" : "} {renderedCodeToHtml f.type}
      </div>
    </li>)

/--
Render all information about a structure as HTML.
-/
def structureToHtml (i : Process.StructureInfo) : HtmlM Unit := do
  if Name.isSuffixOf `mk i.ctor.name then
    (<ul class="structure_fields" id={i.ctor.name.toString}>
      {i.fieldInfo.forM fieldToHtml}
    </ul>)
  else
    let ctorShortName := i.ctor.name.componentsRev.head!.toString
    (<ul class="structure_ext">
      <li id={i.ctor.name.toString} class="structure_ext_ctor">{s!"{ctorShortName} "} :: (</li>
      <ul class="structure_ext_fields">
        {i.fieldInfo.forM fieldToHtml}
      </ul>
      <li class="structure_ext_ctor">)</li>
    </ul>)

end Output
end DocGen4
