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

def index : HtmlM Html := do templateExtends (baseHtml "Index") $ pure $
  <main>
    <a id="top"></a>
    <h1> Welcome to the documentation page </h1>
    What is up?
  </main>

end Output
end DocGen4
