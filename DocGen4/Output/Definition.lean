import DocGen4.Output.Template
import DocGen4.Output.DocString

namespace DocGen4
namespace Output

open scoped DocGen4.Jsx
open Lean Widget

def equationToHtml (c : CodeWithInfos) : HtmlM Html := do
  pure <li «class»="equation">[←infoFormatToHtml c]</li>

def equationsToHtml (i : DefinitionInfo) : HtmlM (Option Html) := do
  if let some eqs := i.equations then
    let equationsHtml ← eqs.mapM equationToHtml
    pure
      <details>
        <summary>Equations</summary>
        <ul «class»="equations">
          [equationsHtml]
        </ul>
      </details>
  else
    pure none

def definitionToHtml (i : DefinitionInfo) : HtmlM (Array Html) := do
  let equationsHtml? ← equationsToHtml i
  match equationsHtml? with
  | some e => pure #[e]
  | none   => pure #[]


end Output
end DocGen4

