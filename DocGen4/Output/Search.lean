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
open DocGen4 (Raw)

def search : BaseHtmlM Unit := do
  baseHtmlGenerator "Search" do
    <main>
      <h1> Search Results </h1>
      <label for="search_page_query">Query:</label>
      <input id="search_page_query" />
      <div id="kinds">
        Allowed Kinds:
        <input type="checkbox" id="def_checkbox" class="kind_checkbox" value="def" checked="checked" />
        <label for="def_checkbox">def</label>
        <input type="checkbox" id="theorem_checkbox" class="kind_checkbox" value="theorem" checked="checked" />
        <label for="theorem_checkbox">theorem</label>
        <input type="checkbox" id="inductive_checkbox" class="kind_checkbox" value="inductive" checked="checked" />
        <label for="inductive_checkbox">inductive</label>
        <input type="checkbox" id="structure_checkbox" class="kind_checkbox" value="structure" checked="checked" />
        <label for="structure_checkbox">structure</label>
        <input type="checkbox" id="class_checkbox" class="kind_checkbox" value="class" checked="checked" />
        <label for="class_checkbox">class</label>
        <input type="checkbox" id="instance_checkbox" class="kind_checkbox" value="instance" checked="checked" />
        <label for="instance_checkbox">instance</label>
        <input type="checkbox" id="axiom_checkbox" class="axiom_checkbox" value="axiom" checked="checked" />
        <label for="axiom_checkbox">axiom</label>
        <input type="checkbox" id="opaque_checkbox" class="kind_checkbox" value="opaque" checked="checked" />
        <label for="opaque_checkbox">opaque</label>
      </div>

      <script>
        {Raw.mk "document.getElementById('search_page_query').value = new URL(window.location.href).searchParams.get('q')"}
      </script>
      <div id="search_results">
      </div>
    </main>

end Output
end DocGen4
