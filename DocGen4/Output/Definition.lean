import DocGen4.Output.Template
import DocGen4.Output.DocString
import DocGen4.Process

namespace DocGen4
namespace Output

open scoped DocGen4.Jsx
open Lean Widget

def equationToHtml (c : RenderedCode) : HtmlM Html := do
  return <li class="equation">[← renderedCodeToHtml c]</li>

/--
Attempt to render all `simp` equations for this definition. At a size
defined in `equationLimit` we stop trying since they:
- are too ugly to read most of the time
- take too long
-/
def equationsToHtml (i : Process.DefinitionInfo) : HtmlM (Array Html) := do
  if let some eqs := i.equations then
    if eqs.isEmpty && !i.equationsWereOmitted then return #[]
    let equationsHtml ← eqs.mapM equationToHtml
    if i.equationsWereOmitted then
      return #[
        <details>
          <summary>Equations</summary>
          <ul class="equations">
            <li class="equation">One or more equations did not get rendered due to their size.</li>
            [equationsHtml]
          </ul>
        </details>
      ]
    else
      return #[
        <details>
          <summary>Equations</summary>
          <ul class="equations">
            [equationsHtml]
          </ul>
        </details>
      ]
  else
    return #[]

end Output
end DocGen4

