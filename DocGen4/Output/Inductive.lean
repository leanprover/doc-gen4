import DocGen4.Output.Template
import DocGen4.Output.DocString
import DocGen4.Process

namespace DocGen4
namespace Output

open scoped DocGen4.Jsx
open Lean

def instancesForToHtml (typeName : Name) : HtmlM Html := do
  pure
    <details «class»="instances">
        <summary>Instances For</summary>
        <ul id={s!"instances-for-list-{typeName}"} class="instances-for-list"></ul>
    </details>

def ctorToHtml (c : Process.NameInfo) : HtmlM Html := do
  let shortName := c.name.components'.head!.toString
  let name := c.name.toString
  if let some doc := c.doc then
    let renderedDoc ← docStringToHtml doc
    pure
      <li class="constructor" id={name}>
        <div class="inductive_ctor_doc">[renderedDoc]</div>
        {shortName} : [←infoFormatToHtml c.type]
      </li>
  else
    pure
      <li class="constructor" id={name}>
        {shortName} : [←infoFormatToHtml c.type]
      </li>

def inductiveToHtml (i : Process.InductiveInfo) : HtmlM (Array Html) := do
  let constructorsHtml := <ul class="constructors">[← i.ctors.toArray.mapM ctorToHtml]</ul>
  pure #[constructorsHtml]

end Output
end DocGen4
