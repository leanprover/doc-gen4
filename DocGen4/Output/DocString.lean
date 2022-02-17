import CMark
import DocGen4.Output.Template
import Lean.Data.Parsec

open Lean

namespace DocGen4
namespace Output

open Xml Parser Lean Parsec

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

def extendRelativeLink (link : String) (root : String) : String := 
  -- HACK: better way to detect absolute links
  if link.startsWith "http" then
    link
  else root ++ link

partial def addAttributes : Element → HtmlM Element
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
      | Content.Element e => (addAttributes e).map (Content.Element)
      | _ => pure c))
      |>.push (Content.Element anchor)
    pure ⟨ name, newAttrs, newContents⟩ 
  -- extend relative href for <a></a>
  else if name = "a" then
    let root ← getRoot
    let newAttrs := match attrs.find? "href" with
    | some href => attrs.insert "href" (extendRelativeLink href root)
    | none => attrs
    pure ⟨ name, newAttrs, contents⟩
  -- recursively modify
  else
    let newContents ← contents.mapM λ c => match c with
      | Content.Element e => (addAttributes e).map Content.Element
      | _ => pure c
    pure ⟨ name, attrs, newContents ⟩ 

def docStringToHtml (s : String) : HtmlM (Array Html) := do
  let rendered := CMark.renderHtml s
  match manyDocument rendered.mkIterator with
  | Parsec.ParseResult.success _ res => 
    res.mapM λ x => do
      pure (Html.text $ toString (← addAttributes x))
  | _ => pure #[Html.text rendered]

end Output
end DocGen4
