import DocGen4.Output.Base

namespace DocGen4
namespace Output

open Lean

/--
Render an `Arg` as HTML, adding opacity effects etc. depending on what
type of binder it has.
-/
def argToHtml (arg : Process.Arg) : HtmlM Html := do
  let node ← renderedCodeToHtml arg.binder
  let inner := html%{<span class="fn">{node}</span>}
  let html := .element "span" #[("class", "decl_args")] #[inner]
  if arg.implicit then
    return html%{<span class="impl_arg">{html}</span>}
  else
    return html

end Output
end DocGen4
