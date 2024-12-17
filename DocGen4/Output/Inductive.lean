import DocGen4.Output.Arg
import DocGen4.Output.Template
import DocGen4.Output.DocString
import DocGen4.Process

namespace DocGen4
namespace Output

open scoped DocGen4.Jsx
open Lean

def instancesForToHtml (typeName : Name) : BaseHtmlM Html := do
  pure
    <details id={s!"instances-for-list-{typeName}"} «class»="instances-for-list">
        <summary>Instances For</summary>
        <ul class="instances-for-enum"></ul>
    </details>

def ctorToHtml (c : Process.ConstructorInfo) : HtmlM Html := do
  let shortName := c.name.componentsRev.head!.toString
  let name := c.name.toString
  let args ← c.args.mapM argToHtml
  if let some doc := c.doc then
    let renderedDoc ← docStringToHtml doc name
    pure
      <li class="constructor" id={name}>
        {shortName} [args] {" : "} [← infoFormatToHtml c.type]
        <div class="inductive_ctor_doc">[renderedDoc]</div>
      </li>
  else
    pure
      <li class="constructor" id={name}>
        {shortName} [args] {" : "} [← infoFormatToHtml c.type]
      </li>

def inductiveToHtml (i : Process.InductiveInfo) : HtmlM (Array Html) := do
  let constructorsHtml := <ul class="constructors">[← i.ctors.toArray.mapM ctorToHtml]</ul>
  return #[constructorsHtml]

end Output
end DocGen4
