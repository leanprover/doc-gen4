import DocGen4.Output.Template
import DocGen4.Output.DocString

namespace DocGen4
namespace Output

open scoped DocGen4.Jsx
open Lean

def fieldToHtml (f : NameInfo) : HtmlM Html := do
  let shortName := f.name.components'.head!.toString
  let name := f.name.toString
  pure <li «class»="structure_field" id={name}>{s!"{shortName} "} : [←infoFormatToHtml f.type]</li>

def structureToHtml (i : StructureInfo) : HtmlM (Array Html) := do
  let structureHtml :=
    if Name.isSuffixOf `mk i.ctor.name then
      (<ul «class»="structure_fields" id={i.ctor.name.toString}>
        [←i.fieldInfo.mapM fieldToHtml]
      </ul>)
    else
      let ctorShortName := i.ctor.name.components'.head!.toString
      (<ul «class»="structure_ext">
        <li id={i.ctor.name.toString} «class»="structure_ext_ctor">{s!"{ctorShortName} "} :: (</li>
        <ul «class»="structure_ext_fields">
          [←i.fieldInfo.mapM fieldToHtml]
        </ul>
        <li «class»="structure_ext_ctor">)</li>
      </ul>)
  pure #[structureHtml]

end Output
end DocGen4
