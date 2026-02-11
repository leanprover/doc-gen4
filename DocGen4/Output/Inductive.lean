import DocGen4.Output.Arg
import DocGen4.Output.Template
import DocGen4.Output.DocString
import DocGen4.Process

namespace DocGen4
namespace Output

open scoped DocGen4.Jsx
open Lean

def instancesForToHtml (typeName : Name) : BaseHtmlM Unit := do
  <details id={s!"instances-for-list-{typeName}"} class="instances-for-list">
      <summary>Instances For</summary>
      <ul class="instances-for-enum"></ul>
  </details>

def ctorToHtml (c : Process.ConstructorInfo) : HtmlM Unit := do
  let shortName := c.name.componentsRev.head!.toString
  let name := c.name.toString
  let args := c.args.forM argToHtml
  if let some doc := c.doc then
    (<li class="constructor" id={name}>
      {shortName} {args} {" : "} {renderedCodeToHtml c.type}
      <div class="inductive_ctor_doc">{docStringToHtml doc name}</div>
    </li>)
  else
    (<li class="constructor" id={name}>
      {shortName} {args} {" : "} {renderedCodeToHtml c.type}
    </li>)

def inductiveToHtml (i : Process.InductiveInfo) : HtmlM Unit := do
  <ul class="constructors">{i.ctors.forM ctorToHtml}</ul>

end Output
end DocGen4
