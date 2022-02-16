/-
Copyright (c) 2021 Henrik Böving. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Henrik Böving
-/
import DocGen4.Process
import DocGen4.IncludeStr
import DocGen4.ToHtmlFormat

namespace DocGen4
namespace Output

open scoped DocGen4.Jsx
open Lean System Widget Elab

structure SiteContext where
  result : AnalyzerResult
  basePath: FilePath
  currentPath: FilePath
  currentName : Option Name
  -- Generates a URL pointing to the source of the given module Name
  sourceLinker : Name → Option DeclarationRange → String

def setCurrentName (name : Name) (ctx : SiteContext) := {ctx with currentName := some name}

abbrev HtmlT := ReaderT SiteContext
abbrev HtmlM := HtmlT Id

partial def getDepthToBase (base: FilePath) (cur: FilePath): Nat :=
  if cur == base
  then 0
  else match cur.parent with
      | some cur' => 1 + getDepthToBase base cur'
      | none => panic! "path (" ++ cur.toString ++ ")must have parent"


unsafe def unsafePerformIO [Inhabited a] (io: IO a): a :=
  match unsafeIO io with
  | Except.ok a    =>  a
  | Except.error e => panic! "expected io computation to never fail"

@[implementedBy unsafePerformIO]
def performIO [Inhabited a] (io: IO a): a := Inhabited.default

def getCurrentDepth : HtmlM Nat := do
    let currentPath <- (SiteContext.currentPath <$> read)
    let basePath <- SiteContext.basePath <$> read
    let basePath :=  performIO $  IO.FS.realPath basePath
    let currentPath := performIO $  IO.FS.realPath currentPath
    assert! (basePath.components.length <= currentPath.components.length)
    return getDepthToBase basePath currentPath

def getRoot : HtmlM String := do
  let rec go: Nat -> String
  | 0 => "./"
  | Nat.succ n' => "../" ++ go n'
  let d <- getCurrentDepth
  return (go d)

def getResult : HtmlM AnalyzerResult := do pure (←read).result
def getCurrentName : HtmlM (Option Name) := do pure (←read).currentName
def getSourceUrl (module : Name) (range : Option DeclarationRange): HtmlM String := do pure $ (←read).sourceLinker module range

def templateExtends {α β : Type} (base : α → HtmlM β) (new : HtmlM α) : HtmlM β :=
  new >>= base

def moduleNameToLink (n : Name) : HtmlM String := do
  let parts := n.components.map Name.toString
  pure $ (<- getRoot) ++ (parts.intersperse "/").foldl (. ++ ·) "" ++ ".html"

def moduleNameToFile (basePath : FilePath) (n : Name) : FilePath :=
  let parts := n.components.map Name.toString
  -- FilePath.withExtension (basePath / parts.foldl (· / ·) (FilePath.mk ".")) "html"
  FilePath.withExtension (basePath / parts.foldl (· / ·) (FilePath.mk ".")) "html"



def moduleNameToDirectory (basePath : FilePath) (n : Name) : FilePath :=
  let parts := n.components.dropLast.map Name.toString
  basePath / parts.foldl (· / ·) (FilePath.mk ".")

section Static
  def styleCss : String := include_str "./static/style.css"
  def navJs : String := include_str "./static/nav.js"
  def searchJs : String := include_str "./static/search.js"
end Static

def declNameToLink (name : Name) : HtmlM String := do
  let res ← getResult
  let module := res.moduleNames[res.name2ModIdx.find! name]
  pure $ (←moduleNameToLink module) ++ "#" ++ name.toString

def splitWhitespaces (s : String) : (String × String × String) := Id.run do
  let front := "".pushn ' ' (s.find (!Char.isWhitespace ·))
  let mut s := s.trimLeft
  let back := "".pushn ' ' (s.length - s.offsetOfPos (s.find Char.isWhitespace))
  s:= s.trimRight
  (front, s, back)

partial def infoFormatToHtml (i : CodeWithInfos) : HtmlM (Array Html) := do
  match i with
  | TaggedText.text t => pure #[t]
  | TaggedText.append tt => tt.foldlM (λ acc t => do pure $ acc ++ (←infoFormatToHtml t)) #[]
  | TaggedText.tag a t =>
    match a.info.val.info with
    | Info.ofTermInfo i =>
      match i.expr.consumeMData with
      | Expr.const name _ _ =>
         match t with
         | TaggedText.text t =>
           let (front, t, back) := splitWhitespaces t
           let elem := Html.element "a" true #[("href", ←declNameToLink name)] #[t]
           pure #[Html.text front, elem, Html.text back]
         | _ =>
           -- TODO: Is this ever reachable?
           pure #[Html.element "a" true #[("href", ←declNameToLink name)] (←infoFormatToHtml t)]
      | _ =>
         pure #[Html.element "span" true #[("class", "fn")] (←infoFormatToHtml t)]
    | _ => pure #[Html.element "span" true #[("class", "fn")] (←infoFormatToHtml t)]

end Output
end DocGen4
