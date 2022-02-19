import DocGen4.Output.Template
import DocGen4.Output.DocString

namespace DocGen4
namespace Output

open scoped DocGen4.Jsx
open Lean Widget

def equationToHtml (c : CodeWithInfos) : HtmlM Html := do
  pure <li «class»="equation">[←infoFormatToHtml c]</li>

def equationsToHtml (i : DefinitionInfo) : HtmlM (Array Html) := do
  if let some eqs := i.equations then
    let equationsHtml ← eqs.mapM equationToHtml
    pure #[
      <details>
        <summary>Equations</summary>
        <ul «class»="equations">
          [equationsHtml]
        </ul>
      </details>
    ]
  else
    pure #[]

end Output
end DocGen4

