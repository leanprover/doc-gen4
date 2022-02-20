import DocGen4.Output.Template
import DocGen4.Output.DocString

namespace DocGen4
namespace Output

open scoped DocGen4.Jsx
open Lean Widget

/- This is basically an arbitrary number that seems to work okay. -/
def equationLimit : Nat := 200

def equationToHtml (c : CodeWithInfos) : HtmlM Html := do
  pure <li «class»="equation">[←infoFormatToHtml c]</li>

def equationsToHtml (i : DefinitionInfo) : HtmlM (Array Html) := do
  if let some eqs := i.equations then
    let equationsHtml ← eqs.mapM equationToHtml
    let filteredEquationsHtml := equationsHtml.filter (λ eq => eq.textLength < equationLimit)
    if equationsHtml.size ≠ filteredEquationsHtml.size then
      pure #[
        <details>
          <summary>Equations</summary>
          <ul «class»="equations">
            <li «class»="equation">One or more equations did not get rendered due to their size.</li>
            [filteredEquationsHtml]
          </ul>
        </details>
      ]
    else
      pure #[
        <details>
          <summary>Equations</summary>
          <ul «class»="equations">
            [filteredEquationsHtml]
          </ul>
        </details>
      ]
  else
    pure #[]

end Output
end DocGen4

