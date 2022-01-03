import DocGen4.Output.Template

namespace DocGen4
namespace Output

open scoped DocGen4.Jsx


def ctorToHtml (i : NameInfo) : HtmlM Html := do
  let name := i.name.components'.head!.toString
  return <li «class»="constructor" id={name}>{name} : [←infoFormatToHtml i.type]</li>

def inductiveToHtml (i : InductiveInfo) : HtmlM (Array Html) := do
  #[Html.element "ul" false #[("class", "constructors")] (←i.ctors.toArray.mapM ctorToHtml)]

end Output
end DocGen4
