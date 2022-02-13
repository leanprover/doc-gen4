import DocGen4.Output.Template

namespace DocGen4
namespace Output

open scoped DocGen4.Jsx

def ctorToHtml (i : NameInfo) : HtmlM Html := do
  let shortName := i.name.components'.head!.toString
  let name := i.name.toString
  pure <li «class»="constructor" id={name}>{shortName} : [←infoFormatToHtml i.type]</li>

def inductiveToHtml (i : InductiveInfo) : HtmlM (Array Html) := do
  pure #[<ul "class"="constructors">[← i.ctors.toArray.mapM ctorToHtml]</ul>]

end Output
end DocGen4
