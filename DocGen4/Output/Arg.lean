import DocGen4.Output.Base

namespace DocGen4
namespace Output

open scoped DocGen4.Jsx

/--
Render an `Arg` as HTML, adding opacity effects etc. depending on what
type of binder it has.
-/
def argToHtml (arg : Process.Arg) : HtmlM Unit := do
  let inner : HtmlM Unit :=
    (<span class="decl_args">
      <span class="fn">{renderedCodeToHtml arg.binder}</span>
    </span>)
  if arg.implicit then
    (<span class="impl_arg">{inner}</span>)
  else
    inner

end Output
end DocGen4
