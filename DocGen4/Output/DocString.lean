import CMark
import DocGen4.Output.Template
import Lean.Data.Parsec

open Lean

namespace DocGen4
namespace Output

open Xml Parser Lean Parsec

instance : Inhabited Element := ⟨"", Std.RBMap.empty, #[]⟩

def manyDocument : Parsec (Array Element) := many (prolog *> element <* many Misc) <* eof

partial def elementToPlainText : Xml.Element → String
| (Element.Element name attrs contents) => 
  "".intercalate (contents.toList.map contentToPlainText)
  where
    contentToPlainText c := match c with
    | Content.Element el => elementToPlainText el
    | Content.Comment _ => ""
    | Content.Character s => s

def dropAllCharWhile (s : String) (p : Char → Bool) : String :=
  ⟨ s.data.filter p ⟩ 

def textToIdAttribute (s : String) : String :=
  dropAllCharWhile s (λ c => (c.isAlphanum ∨ c = '-' ∨ c = ' '))
  |>.toLower
  |>.replace " " "-"

def possibleNameToLink (s : String) : HtmlM (Option String) := do
  let res ← getResult
  let name := s.splitOn "." |>.foldl (λ acc part => Name.mkStr acc part) Name.anonymous
  -- with exactly the same name
  if res.name2ModIdx.contains name then
     declNameToLink name
  -- find similar name in the same module
  else
    match (← getCurrentName) with
    | some currentName =>
      match (res.moduleInfo.find! currentName).members.find? (·.getName.toString.endsWith s) with
      | some info => 
        declNameToLink info.getName
      | _ => pure none
    | _ => pure none

def extendRelativeLink (s : String)  : HtmlM String := do
  -- for relative doc links
  if s.startsWith "#" then
    if let some link ← possibleNameToLink (s.drop 1) then
      pure link
    else
      pure s
  -- for absolute and relative urls
  else if s.startsWith "http" then 
    pure s
  else pure ((←getRoot) ++ s)

def possibleNameToAnchor (s : String) : HtmlM Content := do
  let link? ← possibleNameToLink s
  match link? with
  | some link => 
    let res ← getResult
    let attributes := Std.RBMap.empty.insert "href" link
    pure $ Content.Element $ Element.Element "a" attributes #[Content.Character s]
  | none => pure $ Content.Character s

partial def modifyElement (element : Element) (linkCode : Bool := true) : HtmlM Element :=
  match element with
  | el@(Element.Element name attrs contents) => do
    -- add id and class to <h_></h_>
    if name = "h1" ∨ name = "h2" ∨ name = "h3" ∨ name = "h4" ∨ name = "h5" ∨ name = "h6" then
      let id := textToIdAttribute (elementToPlainText el)
      let anchorAttributes := Std.RBMap.empty
        |>.insert "class" "hover-link"
        |>.insert "href" s!"#{id}"
      let anchor := Element.Element "a" anchorAttributes #[Content.Character "#"]
      let newAttrs := attrs
        |>.insert "id" id
        |>.insert "class" "markdown-heading"
      let newContents := (← 
        contents.mapM (λ c => match c with
        | Content.Element e => (modifyElement e).map (Content.Element)
        | _ => pure c))
        |>.push (Content.Character " ")
        |>.push (Content.Element anchor)
      pure ⟨ name, newAttrs, newContents⟩ 
    -- extend relative href for <a></a>
    else if name = "a" then
      let root ← getRoot
      let newAttrs ← match (attrs.find? "href").map (extendRelativeLink) with
      | some href => href.map (attrs.insert "href")
      | none => pure attrs
      pure ⟨ name, newAttrs, contents⟩
    -- auto link for inline <code></code>
    else if name = "code" ∧ linkCode then
      let mut newContents := #[]
      for c in contents do
        match c with
        | Content.Character s =>
          newContents := newContents ++ (← s.splitOn.mapM possibleNameToAnchor)
        | _ => newContents := newContents.push c
      pure ⟨ name, attrs, newContents⟩
    -- recursively modify
    else
      let newContents ← contents.mapM λ c => match c with
        -- disable auto link for code blocks
        | Content.Element e => (modifyElement e (name ≠ "pre")).map Content.Element
        | _ => pure c
      pure ⟨ name, attrs, newContents ⟩ 

def docStringToHtml (s : String) : HtmlM (Array Html) := do
  let rendered := CMark.renderHtml s
  match manyDocument rendered.mkIterator with
  | Parsec.ParseResult.success _ res => 
    res.mapM λ x => do
      pure (Html.text $ toString (← modifyElement x))
  | _ => pure #[Html.text rendered]

end Output
end DocGen4
