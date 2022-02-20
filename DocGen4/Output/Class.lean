import DocGen4.Output.Template
import DocGen4.Output.Structure

namespace DocGen4
namespace Output

open scoped DocGen4.Jsx
open Lean

def classInstanceToHtml (name : Name) : HtmlM Html := do
  pure <li><a href={←declNameToLink name}>{name.toString}</a></li>

def classInstancesToHtml (instances : Array Name) : HtmlM Html := do
  let instancesHtml ← instances.mapM classInstanceToHtml
  pure
    <details «class»="instances">
        <summary>Instances</summary>
        <ul>
          [instancesHtml]
        </ul>
    </details>

def classToHtml (i : ClassInfo) : HtmlM (Array Html) := do
  pure $ (←structureToHtml i.toStructureInfo)

end Output
end DocGen4
