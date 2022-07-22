import DocGen4.Output.Template

namespace DocGen4
namespace Output

open scoped DocGen4.Jsx
open Lean

def find : BaseHtmlM Html := do
  pure
    <html lang="en">
      <head>
        <link rel="preload" href={s!"{←getRoot}/declarations/declaration-data.bmp"} as="image"/>
        <script>{s!"const SITE_ROOT={String.quote (←getRoot)};"}</script>
        <script type="module" async="true" src="./find.js"></script>
      </head>
      <body></body>
    </html>

end Output
end DocGen4

