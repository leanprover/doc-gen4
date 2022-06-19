/-
Copyright (c) 2022 Henrik Böving. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Henrik Böving, Xubai Wang
-/
import DocGen4.Output.Base
import DocGen4.Output.ToHtmlFormat
import DocGen4.LeanInk.Process
import Lean.Data.Json
import LeanInk.Annotation.Alectryon

namespace LeanInk.Annotation.Alectryon

open DocGen4 Output
open scoped DocGen4.Jsx

def TypeInfo.toHtml : TypeInfo → HtmlM Html := sorry

def Token.toHtml : Token → HtmlM Html := sorry

def Contents.toHtml : Contents → HtmlM Html
  | .string value => sorry
  | .experimentalTokens value => sorry

def Hypothesis.toHtml : Hypothesis → HtmlM Html := sorry

def Goal.toHtml : Goal → HtmlM Html := sorry

def Message.toHtml : Message → HtmlM Html := sorry

def Sentence.toHtml : Sentence → HtmlM Html := sorry

def Text.toHtml : Text → HtmlM Html := sorry

def Fragment.toHtml : Fragment → HtmlM Html
  | .text value => sorry
  | .sentence value => sorry

end LeanInk.Annotation.Alectryon

namespace DocGen4.Output.LeanInk

open Lean
open LeanInk.Annotation.Alectryon
open scoped DocGen4.Jsx

def moduleToHtml (module : Process.Module) (inkPath : System.FilePath) (sourceFilePath : System.FilePath) : HtmlT IO Html := withReader (setCurrentName module.name) do
  let json ← runInk inkPath sourceFilePath
  let fragments : Except String (Array Fragment) := fromJson? json
  match fragments with
  | .ok fragments => pure $ <div>hello</div>
  | .error err => throw $ IO.userError s!"Error while parsing LeanInk Output: {err}"

end DocGen4.Output.LeanInk
