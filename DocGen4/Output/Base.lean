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
  root : String
  result : AnalyzerResult
  currentName : Option Name

def setCurrentName (name : Name) (ctx : SiteContext) := {ctx with currentName := some name}

abbrev HtmlT := ReaderT SiteContext
abbrev HtmlM := HtmlT Id

def getRoot : HtmlM String := do (←read).root
def getResult : HtmlM AnalyzerResult := do (←read).result
def getCurrentName : HtmlM (Option Name) := do (←read).currentName

def templateExtends {α β : Type} (base : α → HtmlM β) (new : HtmlM α) : HtmlM β :=
  new >>= base

-- TODO: Change this to HtmlM and auto add the root URl
def moduleNameToLink (n : Name) : HtmlM String := do
  let parts := n.components.map Name.toString
  (←getRoot) ++ (parts.intersperse "/").foldl (· ++ ·) "" ++ ".html"

def moduleNameToFile (basePath : FilePath) (n : Name) : FilePath :=
    FilePath.withExtension (basePath / parts.foldl (· / ·) (FilePath.mk ".")) "html"
  where
    parts := n.components.map Name.toString

def moduleNameToDirectory (basePath : FilePath) (n : Name) : FilePath :=
    basePath / parts.foldl (· / ·) (FilePath.mk ".")
  where
    parts := n.components.dropLast.map Name.toString

section Static
  def styleCss : String := include_str "./static/style.css"
  def navJs : String := include_str "./static/nav.js"
end Static

def declNameToLink (name : Name) : HtmlM String := do
  let res ← getResult
  let module := res.moduleNames[res.name2ModIdx.find! name]
  (←moduleNameToLink module) ++ "#" ++ name.toString

def splitWhitespaces (s : String) : (String × String × String) := Id.run do
  let front := "".pushn ' ' (s.find (!Char.isWhitespace ·))
  let mut s := s.trimLeft
  let back := "".pushn ' ' (s.length - s.offsetOfPos (s.find Char.isWhitespace))
  s:= s.trimRight
  (front, s, back)

partial def infoFormatToHtml (i : CodeWithInfos) : HtmlM (Array Html) := do
  match i with
  | TaggedText.text t => return #[t]
  | TaggedText.append tt => tt.foldlM (λ acc t => do acc ++ (←infoFormatToHtml t)) #[]
  | TaggedText.tag a t =>
    match a.info.val.info with
    | Info.ofTermInfo i =>
      match i.expr.consumeMData with
      | Expr.const name _ _ =>
         match t with
         | TaggedText.text t =>
           let (front, t, back) := splitWhitespaces t
           let elem := Html.element "a" true #[("href", ←declNameToLink name)] #[t]
           #[Html.text front, elem, Html.text back]
         | _ =>
           -- TODO: Is this ever reachable?
           #[Html.element "a" true #[("href", ←declNameToLink name)] (←infoFormatToHtml t)]
      | _ =>
         #[Html.element "span" true #[("class", "fn")] (←infoFormatToHtml t)]
    | _ => #[Html.element "span" true #[("class", "fn")] (←infoFormatToHtml t)]

end Output
end DocGen4
