/-
Copyright (c) 2021 Henrik Böving. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Henrik Böving
-/
import DocGen4.Output.ToHtmlFormat
import DocGen4.Output.Navbar

namespace DocGen4
namespace Output

open scoped DocGen4.Jsx
open DocGen4 (Raw)

/--
The HTML template used for all pages.
-/
def baseHtmlGenerator (title : String) (writeContent : BaseHtmlM Unit) : BaseHtmlM Unit := do
  (<html lang="en">
    <head>
      {baseHtmlHeadDeclarations}

      <title>{title}</title>
      <script defer="true" src={s!"{← getRoot}mathjax-config.js"}></script>
      <script defer="true" src="https://cdnjs.cloudflare.com/polyfill/v3/polyfill.min.js?features=es6"></script>
      <script defer="true" src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"></script>

      <script>{Raw.mk s!"const SITE_ROOT={String.quote (← getRoot)};"}</script>
      {do if let some module := ← getCurrentName then
            (<script>{Raw.mk s!"const MODULE_NAME={String.quote module.toString};"}</script>)}
      <script type="module" src={s!"{← getRoot}jump-src.js"}></script>
      <script type="module" src={s!"{← getRoot}search.js"}></script>
      <script type="module" src={s!"{← getRoot}expand-nav.js"}></script>
      <script type="module" src={s!"{← getRoot}how-about.js"}></script>
      <script type="module" src={s!"{← getRoot}instances.js"}></script>
      <script type="module" src={s!"{← getRoot}importedBy.js"}></script>
    </head>

    <body>

      <input id="nav_toggle" type="checkbox"/>

      <header>
        <h1><label for="nav_toggle"></label><span>Documentation</span></h1>
        <h2 class="header_filename break_within">{breakWithin title}</h2>
        <form id="search_form">
          <input type="text" name="q" autocomplete="off"/>{Raw.mk "&#32;"}
          <button id="search_button" onclick={s!"javascript: form.action='{← getRoot}search.html';"}>Search</button>
        </form>
      </header>

      {writeContent}

      <nav class="nav">
        <iframe src={s!"{← getRoot}navbar.html"} class="navframe" frameBorder="0"></iframe>
      </nav>
    </body>
  </html>)

end Output
end DocGen4
