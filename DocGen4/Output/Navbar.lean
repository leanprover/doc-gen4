/-
Copyright (c) 2021 Henrik Böving. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Henrik Böving
-/
import Lean
import DocGen4.Output.ToHtmlFormat
import DocGen4.Output.Base

namespace DocGen4
namespace Output

open Lean
open scoped DocGen4.Jsx

def moduleListFile (file : Name) : BaseHtmlM Unit := do
  let cls := if (← getCurrentName) == file then "nav_link visible" else "nav_link"
  (<div class={cls}>
    <a href={← moduleNameToLink file}>{file.getString!}</a>
  </div>)

/--
Build the HTML tree representing the module hierarchy.
-/
partial def moduleListDir (h : Hierarchy) : BaseHtmlM Unit := do
  let children := Array.mk (h.getChildren.toList.map Prod.snd)
  let dirs := children.filter (fun c => c.getChildren.toList.length != 0)
  let files := children.filter (fun c => Hierarchy.isFile c && c.getChildren.toList.length = 0)
    |>.map Hierarchy.getName
  let moduleLink ← moduleNameToLink h.getName
  let openAttr := if (← getCurrentName).any (h.getName.isPrefixOf ·) then #[("open", "")] else #[]
  let detailsAttrs := #[("class", "nav_sect"), ("data-path", moduleLink)] ++ openAttr
  (<details [detailsAttrs]>
    {if h.isFile then
      (<summary>{s!"{h.getName.getString!} ("}<a href={← moduleNameToLink h.getName}>file</a>)</summary>)
    else
      (<summary>{h.getName.getString!}</summary>)}
    {dirs.forM moduleListDir}
    {files.forM moduleListFile}
  </details>)

/--
Return a list of top level modules, linkified and rendered as HTML
-/
def moduleList : BaseHtmlM Unit := do
  let hierarchy ← getHierarchy
  (<div class="module_list">
    {do for (_, cs) in hierarchy.getChildren do
      moduleListDir cs}
  </div>)

/--
The main entry point to rendering the navbar on the left hand side.
-/
def navbar : BaseHtmlM Unit := do
  (<html lang="en">
    <head>
      {baseHtmlHeadDeclarations}

      <script type="module" src={s!"{← getRoot}nav.js"}></script>
      <script type="module" src={s!"{← getRoot}color-scheme.js"}></script>
      <base target="_parent" />
    </head>

    <body>
      <div class="navframe">
      <nav class="nav">
        <h3>General documentation</h3>
        <div class="nav_link"><a href={s!"{← getRoot}"}>index</a></div>
        <div class="nav_link"><a href={s!"{← getRoot}foundational_types.html"}>foundational types</a></div>
        <div class="nav_link"><a href={s!"{← getRoot}tactics.html"}>tactics</a></div>
        {do
          let config ← readThe SiteBaseContext
          if not config.refs.isEmpty then
            (<div class="nav_link"><a href={s!"{← getRoot}references.html"}>references</a></div>)}
        <h3>Library</h3>
        {moduleList}
        <div id="settings" hidden="hidden">
          <h3>Color scheme</h3>
          <form id="color-theme-switcher">
              <label for="color-theme-dark">
                  <input type="radio" name="color_theme" id="color-theme-dark" value="dark" autocomplete="off"/>dark</label>
              <label for="color-theme-system" title="Match system theme settings">
                  <input type="radio" name="color_theme" id="color-theme-system" value="system" autocomplete="off"/>system</label>
              <label for="color-theme-light">
                  <input type="radio" name="color_theme" id="color-theme-light" value="light" autocomplete="off"/>light</label>
          </form>
        </div>
      </nav>
      </div>
    </body>
  </html>)

end Output
end DocGen4
