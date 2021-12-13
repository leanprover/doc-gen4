/-
Copyright (c) 2021 Henrik Böving. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Henrik Böving
-/
import Lean
import Std.Data.HashMap
import DocGen4.Process
import DocGen4.ToHtmlFormat
import DocGen4.IncludeStr

namespace DocGen4

open Lean Std
open scoped DocGen4.Jsx
open IO System

structure SiteContext where
  root : String
  result : AnalyzerResult

abbrev HtmlM := Reader SiteContext

def getRoot : HtmlM String := do (←read).root
def getResult : HtmlM AnalyzerResult := do (←read).result

def templateExtends {α β : Type} (base : α → HtmlM β) (new : HtmlM α) : HtmlM β :=
  new >>= base

def nameToUrl (n : Name) : String :=
    (parts.intersperse "/").foldl (· ++ ·) "" ++ ".html"
  where
    parts := n.components.map Name.toString

def nameToDirectory (basePath : FilePath) (n : Name) : FilePath :=
    basePath / parts.foldl (λ acc p => acc / FilePath.mk p) (FilePath.mk ".")
  where
    parts := n.components.dropLast.map Name.toString

def moduleListFile (file : Name) : HtmlM Html := do
  <div «class»="nav_link">
    <a href={s!"{←getRoot}{nameToUrl file}"}>{file.toString}</a>
  </div>

partial def moduleListDir (h : Hierarchy) : HtmlM Html := do
  let children := Array.mk (h.getChildren.toList.map Prod.snd)
  let dirs := children.filter (λ c => c.getChildren.toList.length != 0)
  let files := children.filter Hierarchy.isFile |>.map Hierarchy.getName
  return <details «class»="nav_sect" «data-path»={nameToUrl h.getName}>
    <summary>{h.getName.toString}</summary>
    [(←(dirs.mapM moduleListDir))]
    [(←(files.mapM moduleListFile))]
  </details>

def moduleList : HtmlM (Array Html) := do
  let hierarchy := (←getResult).hierarchy
  let mut list := Array.empty
  for (n, cs) in hierarchy.getChildren do
    list := list.push <h4>{n.toString}</h4>
    list := list.push $ ←moduleListDir cs
  list

def navbar : HtmlM Html := do
  <nav «class»="nav">
    <h3>General documentation</h3>
    <div «class»="nav_link"><a href={s!"{←getRoot}"}>index</a></div>
    /-
    TODO: Add these in later
    <div «class»="nav_link"><a href={s!"{←getRoot}tactics.html"}>tactics</a></div>
    <div «class»="nav_link"><a href={s!"{←getRoot}commands.html"}>commands</a></div>
    <div «class»="nav_link"><a href={s!"{←getRoot}hole_commands.html"}>hole commands</a></div>
    <div «class»="nav_link"><a href={s!"{←getRoot}attributes.html"}>attributes</a></div>
    <div «class»="nav_link"><a href={s!"{←getRoot}notes.html"}>notes</a></div>
    <div «class»="nav_link"><a href={s!"{←getRoot}references.html"}>references</a></div>
    -/
    <h3>Library</h3>
    [←moduleList]
  </nav>

def baseHtml (title : String) (site : Html) : HtmlM Html := do
  <html lang="en">
    <head>
      <link rel="stylesheet" href={s!"{←getRoot}style.css"}/>
      <link rel="stylesheet" href={s!"{←getRoot}pygments.css"}/>
      <link rel="shortcut icon" href={s!"{←getRoot}favicon.ico"}/>
      <title>{title}</title>
      <meta charset="UTF-8"/>
      <meta name="viewport" content="width=device-width, initial-scale=1"/>
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

    <nav «class»="internal_nav"></nav>

    {site}
    
    {←navbar}
    -- TODO Add the js stuff

    </body>
  </html>

def notFound : HtmlM Html := do templateExtends (baseHtml "404") $
  <main>
    <h1>404 Not Found</h1>
    <p> Unfortunately, the page you were looking for is no longer here. </p>
    <div id="howabout"></div>
  </main>

def index : HtmlM Html := do templateExtends (baseHtml "Index") $
  <main>
    <a id="top"></a>
    <h1> Welcome to the documentation page </h1>
    What is up?
  </main>

def styleCss : String := include_str "./static/style.css"

def moduleToHtml (module : Module) : HtmlM Html := do templateExtends (baseHtml module.name.toString) $
  <main>
    <h1>This is the page of {module.name.toString}</h1>
  </main>

def htmlOutput (result : AnalyzerResult) : IO Unit := do
  -- TODO: parameterize this
  let config := { root := "/", result := result }
  let basePath := FilePath.mk "./build/doc/"
  let indexHtml := ReaderT.run index config 
  let notFoundHtml := ReaderT.run notFound config
  FS.createDirAll basePath
  FS.writeFile (basePath / "index.html") indexHtml.toString
  FS.writeFile (basePath / "style.css") styleCss
  FS.writeFile (basePath / "404.html") notFoundHtml.toString
  for (module, content) in result.modules.toArray do
    let moduleHtml := ReaderT.run (moduleToHtml content) config
    let path := basePath / (nameToUrl module)
    FS.createDirAll $ nameToDirectory basePath module
    FS.writeFile path moduleHtml.toString

end DocGen4

