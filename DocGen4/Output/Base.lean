/-
Copyright (c) 2021 Henrik Böving. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Henrik Böving
-/
import Lean
import DocGen4.Process
import DocGen4.IncludeStr

namespace DocGen4
namespace Output

open Lean System

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
def moduleNameToUrl (n : Name) : String :=
    (parts.intersperse "/").foldl (· ++ ·) "" ++ ".html"
  where
    parts := n.components.map Name.toString

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

end Output
end DocGen4
