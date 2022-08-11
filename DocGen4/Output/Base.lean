/-
Copyright (c) 2021 Henrik Böving. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Henrik Böving
-/
import DocGen4.Process
import DocGen4.Output.ToHtmlFormat

namespace DocGen4.Output

open scoped DocGen4.Jsx
open Lean System Widget Elab Process

def basePath := FilePath.mk "." / "build" / "doc"
def srcBasePath := basePath / "src"
def declarationsBasePath := basePath / "declarations"

/--
The context used in the `BaseHtmlM` monad for HTML templating.
-/
structure SiteBaseContext where

  /--
  The module hierarchy as a tree structure.
  -/
  hierarchy : Hierarchy
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
The context used in the `HtmlM` monad for HTML templating.
-/
structure SiteContext where
  /--
  The full analysis result from the Process module.
  -/
  result : AnalyzerResult
  /--
  A function to link declaration names to their source URLs, usually Github ones.
  -/
  sourceLinker : Name → Option DeclarationRange → String
  /--
  Whether LeanInk is enabled
  -/
  leanInkEnabled : Bool

def setCurrentName (name : Name) (ctx : SiteBaseContext) := {ctx with currentName := some name}

abbrev BaseHtmlT := ReaderT SiteBaseContext
abbrev BaseHtmlM := BaseHtmlT Id

abbrev HtmlT (m) := ReaderT SiteContext (BaseHtmlT m)
abbrev HtmlM := HtmlT Id

def HtmlT.run (x : HtmlT m α) (ctx : SiteContext) (baseCtx : SiteBaseContext) : m α :=
  ReaderT.run x ctx |>.run baseCtx

def HtmlM.run (x : HtmlM α) (ctx : SiteContext) (baseCtx : SiteBaseContext) : α :=
  ReaderT.run x ctx |>.run baseCtx |>.run

instance [Monad m] : MonadLift HtmlM (HtmlT m) where
  monadLift x := do pure <| x.run (←readThe SiteContext) (←readThe SiteBaseContext)

instance [Monad m] : MonadLift BaseHtmlM (BaseHtmlT m) where
  monadLift x := do pure <| x.run (←readThe SiteBaseContext)

/--
Obtains the root URL as a relative one to the current depth.
-/
def getRoot : BaseHtmlM String := do
  let rec go: Nat -> String
  | 0 => "./"
  | Nat.succ n' => "../" ++ go n'
  let d <- SiteBaseContext.depthToRoot <$> read
  return (go d)

def getHierarchy : BaseHtmlM Hierarchy := do pure (←read).hierarchy
def getCurrentName : BaseHtmlM (Option Name) := do pure (←read).currentName
def getResult : HtmlM AnalyzerResult := do pure (←read).result
def getSourceUrl (module : Name) (range : Option DeclarationRange): HtmlM String := do pure <| (←read).sourceLinker module range
def leanInkEnabled? : HtmlM Bool := do pure (←read).leanInkEnabled

/--
If a template is meant to be extended because it for example only provides the
header but no real content this is the way to fill the template with content.
This is untyped so HtmlM and BaseHtmlM can be mixed.
-/
def templateExtends {α β} {m} [Bind m] (base : α → m β) (new : m α) : m β :=
  new >>= base

def templateLiftExtends {α β} {m n} [Bind m] [MonadLift n m] (base : α → n β) (new : m α) : m β :=
  new >>= (monadLift ∘ base)
/--
Returns the doc-gen4 link to a module name.
-/
def moduleNameToLink (n : Name) : BaseHtmlM String := do
  let parts := n.components.map Name.toString
  pure <| (← getRoot) ++ (parts.intersperse "/").foldl (· ++ ·) "" ++ ".html"

/--
Returns the HTML doc-gen4 link to a module name.
-/
def moduleToHtmlLink (module : Name) : BaseHtmlM Html := do
  pure <a href={←moduleNameToLink module}>{module.toString}</a>

/--
Returns the LeanInk link to a module name.
-/
def moduleNameToInkLink (n : Name) : BaseHtmlM String := do
  let parts := "src" :: n.components.map Name.toString
  pure <| (← getRoot) ++ (parts.intersperse "/").foldl (· ++ ·) "" ++ ".html"

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
  def instancesJs : String := include_str "../../static/instances.js"
  def importedByJs : String := include_str "../../static/importedBy.js"
  def findJs : String := include_str "../../static/find/find.js"
  def mathjaxConfigJs : String := include_str "../../static/mathjax-config.js"
  
  def alectryonCss : String := include_str "../../static/alectryon/alectryon.css"
  def alectryonJs : String := include_str "../../static/alectryon/alectryon.js"
  def docUtilsCss : String  := include_str "../../static/alectryon/docutils_basic.css"
  def pygmentsCss : String  := include_str "../../static/alectryon/pygments.css"
end Static

/--
Returns the doc-gen4 link to a declaration name.
-/
def declNameToLink (name : Name) : HtmlM String := do
  let res ← getResult
  let module := res.moduleNames[res.name2ModIdx.find! name |>.toNat]!
  pure <| (←moduleNameToLink module) ++ "#" ++ name.toString

/--
Returns the HTML doc-gen4 link to a declaration name.
-/
def declNameToHtmlLink (name : Name) : HtmlM Html := do
  pure <a href={←declNameToLink name}>{name.toString}</a>

/--
Returns the LeanInk link to a declaration name.
-/
def declNameToInkLink (name : Name) : HtmlM String := do
  let res ← getResult
  let module := res.moduleNames[res.name2ModIdx.find! name |>.toNat]!
  pure <| (←moduleNameToInkLink module) ++ "#" ++ name.toString

/--
Returns the HTML doc-gen4 link to a declaration name with "break_within"
set as class.
-/
def declNameToHtmlBreakWithinLink (name : Name) : HtmlM Html := do
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
  let front := "".pushn ' ' <| s.offsetOfPos (s.find (!Char.isWhitespace ·))
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
  | TaggedText.append tt => tt.foldlM (λ acc t => do pure <| acc ++ (←infoFormatToHtml t)) #[]
  | TaggedText.tag a t =>
    match a.info.val.info with
    | Info.ofTermInfo i =>
      let cleanExpr :=  i.expr.consumeMData
      if let Expr.const name _ := cleanExpr then
        -- TODO: this is some very primitive blacklisting but real Blacklisting needs MetaM
        -- find a better solution
        if (←getResult).name2ModIdx.contains name then
          match t with
          | TaggedText.text t =>
            let (front, t, back) := splitWhitespaces <| Html.escape t
            let elem := <a href={←declNameToLink name}>{t}</a>
            pure #[Html.text front, elem, Html.text back]
          | _ =>
            pure #[<a href={←declNameToLink name}>[←infoFormatToHtml t]</a>]
        else
         pure #[<span class="fn">[←infoFormatToHtml t]</span>]
      else
         pure #[<span class="fn">[←infoFormatToHtml t]</span>]
    | _ => pure #[<span class="fn">[←infoFormatToHtml t]</span>]

def baseHtmlHeadDeclarations : BaseHtmlM (Array Html) := do
  pure #[
    <meta charset="UTF-8"/>,
    <meta name="viewport" content="width=device-width, initial-scale=1"/>,
    <link rel="stylesheet" href={s!"{←getRoot}style.css"}/>,
    <link rel="stylesheet" href={s!"{←getRoot}src/pygments.css"}/>,
    <link rel="shortcut icon" href={s!"{←getRoot}favicon.ico"}/>,
    <link rel="prefetch" href={s!"{←getRoot}/declarations/declaration-data.bmp"} as="image"/>
  ]

end DocGen4.Output
