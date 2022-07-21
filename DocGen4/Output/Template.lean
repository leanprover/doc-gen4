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

/--
The HTML template used for all pages.
-/
def baseHtmlGenerator (title : String) (site : Array Html) : BaseHtmlM Html := do
  pure
    <html lang="en">
      {←baseHtmlHead title}
      <body>

        <input id="nav_toggle" type="checkbox"/>

        <header>
          <h1><label for="nav_toggle"></label>Documentation</h1>
          <p class="header_filename break_within">{title}</p>
          -- TODO: Replace this form with our own search
          <form action="https://google.com/search" method="get" id="search_form">
            <input type="hidden" name="sitesearch" value="https://leanprover-community.github.io/mathlib_docs"/>
            <input type="text" name="q" autocomplete="off"/>
            <button>Google site search</button>
          </form>
        </header>

        [site]

        <nav class="nav">
          <iframe src={s!"{←getRoot}/navbar.html"} class="navframe" frameBorder="0"></iframe>
        </nav>

      </body>

    </html>

/--
A comfortability wrapper around `baseHtmlGenerator`.
-/
def baseHtml (title : String) (site : Html) : BaseHtmlM Html := baseHtmlGenerator title #[site]

end Output
end DocGen4
