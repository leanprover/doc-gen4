import DocGen4.Output.Base

namespace DocGen4
namespace Output

open scoped DocGen4.Jsx

/--
Render an `Arg` as HTML, adding opacity effects etc. depending on what
type of binder it has.
-/
def argToHtml (arg : Process.Arg) : HtmlM Html := do
  let node ← renderedCodeToHtml arg.binder
  let inner := <span class="fn">[node]</span>
  let html := Html.element "span" false #[("class", "decl_args")] #[inner]
  if arg.implicit then
    return <span class="impl_arg">{html}</span>
  else
    return html

end Output
end DocGen4
