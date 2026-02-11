import DocGen4.Output.Template
import DocGen4.Output.DocString
import DocGen4.Process

namespace DocGen4
namespace Output

open scoped DocGen4.Jsx
open Lean Widget

def equationToHtml (c : RenderedCode) : HtmlM Unit := do
  <li class="equation">{renderedCodeToHtml c}</li>

/--
Attempt to render all `simp` equations for this definition. At a size
defined in `equationLimit` we stop trying since they:
- are too ugly to read most of the time
- take too long
-/
def equationsToHtml (i : Process.DefinitionInfo) : HtmlM Unit := do
  if let some eqs := i.equations then
    if i.equationsWereOmitted then
      <details>
        <summary>Equations</summary>
        <ul class="equations">
          <li class="equation">One or more equations did not get rendered due to their size.</li>
          {eqs.forM equationToHtml}
        </ul>
      </details>
    else
      <details>
        <summary>Equations</summary>
        <ul class="equations">
          {eqs.forM equationToHtml}
        </ul>
      </details>

end Output
end DocGen4
