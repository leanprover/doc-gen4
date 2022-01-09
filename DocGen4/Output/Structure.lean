import DocGen4.Output.Template

namespace DocGen4
namespace Output

open scoped DocGen4.Jsx

def fieldToHtml (f : NameInfo) : HtmlM Html := do
  let shortName := f.name.components'.head!.toString
  let name := f.name.toString
  return <li «class»="structure_field" id={name}>{shortName} : [←infoFormatToHtml f.type]</li>

def structureToHtml (i : StructureInfo) : HtmlM (Array Html) := do
  #[Html.element "ul" false #[("class", "structure_fields"), ("id", s!"{i.name.toString}.mk")] (←i.fieldInfo.mapM fieldToHtml)]

end Output
end DocGen4
