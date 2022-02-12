/-
Copyright (c) 2021 Henrik Böving. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Henrik Böving
-/
import DocGen4.ToHtmlFormat
import DocGen4.Output.Template

namespace DocGen4
namespace Output

open scoped DocGen4.Jsx

def notFound : HtmlM Html := do templateExtends (baseHtml "404") $ pure $
  <main>
    <h1>404 Not Found</h1>
    <p> Unfortunately, the page you were looking for is no longer here. </p>
    <div id="howabout"></div>
  </main>

end Output
end DocGen4
