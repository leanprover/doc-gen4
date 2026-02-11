import DocGen4.Output.Template
import DocGen4.Output.Structure
import DocGen4.Process

namespace DocGen4
namespace Output

open scoped DocGen4.Jsx
open Lean

def classInstancesToHtml (className : Name) : HtmlM Unit := do
  <details class="instances">
      <summary>Instances</summary>
      <ul id={s!"instances-list-{className}"} class="instances-list"></ul>
  </details>

def classToHtml (i : Process.ClassInfo) : HtmlM Unit := do
  structureToHtml i

end Output
end DocGen4
