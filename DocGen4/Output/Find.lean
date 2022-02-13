import DocGen4.Output.Template

namespace DocGen4
namespace Output

open scoped DocGen4.Jsx
open Lean

def findRedirectHtml (decl : Name) : HtmlM Html := do
  let res ← getResult
  let url ← declNameToLink decl
  let contentString := s!"0;url={url}"
  pure $ Html.element "meta" false #[("http-equiv", "refresh"), ("content", contentString)] #[]
end Output
end DocGen4
