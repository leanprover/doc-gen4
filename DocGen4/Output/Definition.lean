import DocGen4.Output.Template

namespace DocGen4
namespace Output

open scoped DocGen4.Jsx
open Lean Widget

def equationToHtml (c : CodeWithInfos) : HtmlM Html := do
  <li «class»="equation">[←infoFormatToHtml c]</li>

def equationsToHtml (i : DefinitionInfo) : HtmlM (Option Html) := do
  if let some eqs ← i.equations then
    let equationsHtml ← eqs.mapM equationToHtml
    return <details>
      <summary>Equations</summary>
      <ul «class»="equations">
        [equationsHtml]
      </ul>
    </details>
  else
    return none

def definitionToHtml (i : DefinitionInfo) : HtmlM (Array Html) := do
  let equationsHtml ← equationsToHtml i
  if let some equationsHtml ← equationsHtml then
    #[equationsHtml]
  else
    #[]

end Output
end DocGen4

