import DocGen4.Output.Template
import DocGen4.Output.Structure

namespace DocGen4
namespace Output

open scoped DocGen4.Jsx
open Lean

def classInstanceToHtml (name : Name) : HtmlM Html := do
  <li><a href={←declNameToLink name}>{name.toString}</a></li>

def classInstancesToHtml (instances : Array Name) : HtmlM Html := do
  let instancesHtml ← instances.mapM classInstanceToHtml
  return <details «class»="instances">
        <summary>Instances</summary>
        <ul>
          [instancesHtml]
        </ul>
    </details>

def classToHtml (i : ClassInfo) : HtmlM (Array Html) := do
  (←structureToHtml i.toStructureInfo).push (←classInstancesToHtml i.instances)

end Output
end DocGen4
