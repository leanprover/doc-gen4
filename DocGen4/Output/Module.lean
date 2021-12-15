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

def moduleToHtml (module : Module) : HtmlM Html := withReader (setCurrentName module.name) do
  templateExtends (baseHtml module.name.toString) $
    <main>
      <h1>This is the page of {module.name.toString}</h1>
    </main>

end Output
end DocGen4
