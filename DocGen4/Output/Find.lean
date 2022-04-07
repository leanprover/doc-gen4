import DocGen4.Output.Template

namespace DocGen4
namespace Output

open scoped DocGen4.Jsx
open Lean

def find : HtmlM Html := do
  pure
    <html lang="en">
      <head>
        <link rel="preload" href={s!"{←getRoot}declaration-data.bmp"}/>
        <script>{s!"const SITE_ROOT={String.quote (←getRoot)};"}</script>
        <script type="module" async="true" src={s!"./find.js"}></script>
      </head>
      <body></body>
    </html>

end Output
end DocGen4

