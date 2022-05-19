/-
Copyright (c) 2021 Henrik Böving. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Henrik Böving
-/
import DocGen4.Process
import DocGen4.IncludeStr
import DocGen4.Output.ToHtmlFormat

namespace DocGen4.Output

open scoped DocGen4.Jsx
open Lean System Widget Elab Process

/--
The context used in the `HtmlM` monad for HTML templating.
-/
structure SiteContext where
  /--
  The full analysis result from the Process module.
  -/
  result : AnalyzerResult
  /--
  How far away we are from the page root, used for relative links to the root.
  -/
  depthToRoot: Nat
  /--
  The name of the current module if there is one, there exist a few
  pages that don't have a module name.
  -/
  currentName : Option Name
  /--
  A function to link declaration names to their source URLs, usually Github ones.
  -/
  sourceLinker : Name → Option DeclarationRange → String

def setCurrentName (name : Name) (ctx : SiteContext) := {ctx with currentName := some name}

abbrev HtmlT := ReaderT SiteContext
abbrev HtmlM := HtmlT Id

/--
Obtains the root URL as a relative one to the current depth.
-/
def getRoot : HtmlM String := do
  let rec go: Nat -> String
  | 0 => "./"
  | Nat.succ n' => "../" ++ go n'
  let d <- SiteContext.depthToRoot <$> read
  return (go d)

def getResult : HtmlM AnalyzerResult := do pure (←read).result
def getCurrentName : HtmlM (Option Name) := do pure (←read).currentName
def getSourceUrl (module : Name) (range : Option DeclarationRange): HtmlM String := do pure $ (←read).sourceLinker module range

/--
If a template is meant to be extended because it for example only provides the
header but no real content this is the way to fill the template with content.
-/
def templateExtends {α β : Type} (base : α → HtmlM β) (new : HtmlM α) : HtmlM β :=
  new >>= base

/--
Returns the doc-gen4 link to a module name.
-/
def moduleNameToLink (n : Name) : HtmlM String := do
  let parts := n.components.map Name.toString
  pure $ (← getRoot) ++ (parts.intersperse "/").foldl (· ++ ·) "" ++ ".html"

/--
Returns the HTML doc-gen4 link to a module name.
-/
def moduleToHtmlLink (module : Name) : HtmlM Html := do
  pure <a href={←moduleNameToLink module}>{module.toString}</a>

/--
Returns the path to the HTML file that contains information about a module.
-/
def moduleNameToFile (basePath : FilePath) (n : Name) : FilePath :=
  let parts := n.components.map Name.toString
  FilePath.withExtension (basePath / parts.foldl (· / ·) (FilePath.mk ".")) "html"

/--
Returns the directory of the HTML file that contains information about a module.
-/
def moduleNameToDirectory (basePath : FilePath) (n : Name) : FilePath :=
  let parts := n.components.dropLast.map Name.toString
  basePath / parts.foldl (· / ·) (FilePath.mk ".")


section Static
/-!
The following section contains all the statically included files that
are used in documentation generation, notably JS and CSS ones.
-/
  def styleCss : String := include_str "../../static/style.css"
  def declarationDataCenterJs : String := include_str "../../static/declaration-data.js"
  def navJs : String := include_str "../../static/nav.js"
  def howAboutJs : String := include_str "../../static/how-about.js"
  def searchJs : String := include_str "../../static/search.js"
  def findJs : String := include_str "../../static/find/find.js"
  def mathjaxConfigJs : String := include_str "../../static/mathjax-config.js"
end Static

/--
Returns the doc-gen4 link to a declaration name.
-/
def declNameToLink (name : Name) : HtmlM String := do
  let res ← getResult
  let module := res.moduleNames[res.name2ModIdx.find! name]
  pure $ (←moduleNameToLink module) ++ "#" ++ name.toString

/--
Returns the HTML doc-gen4 link to a declaration name.
-/
def declNameToHtmlLink (name : Name) : HtmlM Html := do
  let link ← declNameToLink name
  pure <a href={←declNameToLink name}>{name.toString}</a>

/--
Returns the HTML doc-gen4 link to a declaration name with "break_within"
set as class.
-/
def declNameToHtmlBreakWithinLink (name : Name) : HtmlM Html := do
  let link ← declNameToLink name
  pure <a class="break_within" href={←declNameToLink name}>{name.toString}</a>

/--
In Lean syntax declarations the following pattern is quite common:
```
syntax term " + " term : term
```
that is, we place spaces around the operator in the middle. When the
`InfoTree` framework provides us with information about what source token
corresponds to which identifier it will thus say that `" + "` corresponds to
`HAdd.hadd`. This is however not the way we want this to be linked, in the HTML
only `+` should be linked, taking care of this is what this function is
responsible for.
-/
def splitWhitespaces (s : String) : (String × String × String) := Id.run do
  let front := "".pushn ' ' $ s.offsetOfPos (s.find (!Char.isWhitespace ·))
  let mut s := s.trimLeft
  let back := "".pushn ' ' (s.length - s.offsetOfPos (s.find Char.isWhitespace))
  s := s.trimRight
  (front, s, back)

/--
Turns a `CodeWithInfos` object, that is basically a Lean syntax tree with
information about what the identifiers mean, into an HTML object that links
to as much information as possible.
-/
partial def infoFormatToHtml (i : CodeWithInfos) : HtmlM (Array Html) := do
  match i with
  | TaggedText.text t => pure #[Html.escape t]
  | TaggedText.append tt => tt.foldlM (λ acc t => do pure $ acc ++ (←infoFormatToHtml t)) #[]
  | TaggedText.tag a t =>
    match a.info.val.info with
    | Info.ofTermInfo i =>
      match i.expr.consumeMData with
      | Expr.const name _ _ =>
         match t with
         | TaggedText.text t =>
           let (front, t, back) := splitWhitespaces $ Html.escape t
           let elem := Html.element "a" true #[("href", ←declNameToLink name)] #[t]
           pure #[Html.text front, elem, Html.text back]
         | _ =>
           -- TODO: Is this ever reachable?
           pure #[Html.element "a" true #[("href", ←declNameToLink name)] (←infoFormatToHtml t)]
      | _ =>
         pure #[Html.element "span" true #[("class", "fn")] (←infoFormatToHtml t)]
    | _ => pure #[Html.element "span" true #[("class", "fn")] (←infoFormatToHtml t)]

end DocGen4.Output
