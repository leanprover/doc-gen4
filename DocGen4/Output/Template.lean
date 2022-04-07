/-
Copyright (c) 2021 Henrik Böving. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Henrik Böving
-/
import DocGen4.ToHtmlFormat
import DocGen4.Output.Navbar

namespace DocGen4
namespace Output

open scoped DocGen4.Jsx

def baseHtmlArray (title : String) (site : Array Html) : HtmlM Html := do
  pure
    <html lang="en">
      <head>

        <title>{title}</title>

        <meta charset="UTF-8"/>
        <meta name="viewport" content="width=device-width, initial-scale=1"/>

        <link rel="stylesheet" href={s!"{←getRoot}style.css"}/>
        <link rel="stylesheet" href={s!"{←getRoot}pygments.css"}/>
        <link rel="shortcut icon" href={s!"{←getRoot}favicon.ico"}/>
        <link rel="prefetch" href={s!"{←getRoot}declaration-data.bmp"}/>

        <script defer="true" src={s!"{←getRoot}mathjax-config.js"}></script>
        <script defer="true" src="https://polyfill.io/v3/polyfill.min.js?features=es6"></script>
        <script defer="true" src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"></script>

        <script>{s!"const SITE_ROOT={String.quote (←getRoot)};"}</script>
        <script type="module" src={s!"{←getRoot}nav.js"}></script>
        <script type="module" src={s!"{←getRoot}search.js"}></script>
        <script type="module" src={s!"{←getRoot}how-about.js"}></script>

      </head>

      <body>

        <input id="nav_toggle" type="checkbox"/>

        <header>
          <h1><label «for»="nav_toggle"></label>Documentation</h1>
          <p «class»="header_filename break_within">{title}</p>
          -- TODO: Replace this form with our own search
          <form action="https://google.com/search" method="get" id="search_form">
            <input type="hidden" name="sitesearch" value="https://leanprover-community.github.io/mathlib_docs"/>
            <input type="text" name="q" autocomplete="off"/>
            <button>Google site search</button>
          </form>
        </header>

        [site]

        {←navbar}

      </body>

    </html>


def baseHtml (title : String) (site : Html) : HtmlM Html := baseHtmlArray title #[site]

end Output
end DocGen4
