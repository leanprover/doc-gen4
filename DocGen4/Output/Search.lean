/-
Copyright (c) 2023 Jeremy Salwen. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Jeremy Salwen
-/
import DocGen4.Output.ToHtmlFormat
import DocGen4.Output.Template

namespace DocGen4
namespace Output

open scoped DocGen4.Jsx

def search : BaseHtmlM Html := do templateExtends (baseHtml "Search") <|
  pure <|
    <main>
      <a id="top"></a>
      <h1> Search Results </h1>
      <label for="search_page_query">Query:</label><input id="search_page_query" />
      <script>
        document.getElementById("search_page_query").value = new URL(window.location.href).searchParams.get("q")
      </script>
      <div id="search_results">
      </div>
    </main>

end Output
end DocGen4
