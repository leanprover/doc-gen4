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

def ctorToHtml (c : Process.NameInfo) (backrefs : Array BackrefItem) :
    HtmlM (Html × Array BackrefItem) := do
  let shortName := c.name.componentsRev.head!.toString
  let name := c.name.toString
  if let some doc := c.doc then
    let (renderedDoc, newBackrefs) ← docStringToHtml doc name backrefs
    pure
      (<li class="constructor" id={name}>
        {shortName} : [← infoFormatToHtml c.type]
        <div class="inductive_ctor_doc">[renderedDoc]</div>
      </li>, newBackrefs)
  else
    pure
      (<li class="constructor" id={name}>
        {shortName} : [← infoFormatToHtml c.type]
      </li>, backrefs)

def inductiveToHtml (i : Process.InductiveInfo) (backrefs : Array BackrefItem) :
    HtmlM (Array Html × Array BackrefItem) := do
  let arr := i.ctors.toArray
  let mut newArr : Array Html := #[]
  let mut newBackrefs := backrefs
  let mut idx : Nat := 0
  while h : LT.lt idx arr.size do
    let (c, b') ← ctorToHtml (arr.get ⟨idx, h⟩) newBackrefs
    newArr := newArr.push c
    newBackrefs := b'
    idx := idx + 1
  let constructorsHtml := <ul class="constructors">[newArr]</ul>
  return ⟨ #[constructorsHtml], newBackrefs ⟩

end Output
end DocGen4
