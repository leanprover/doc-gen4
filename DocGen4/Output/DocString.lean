import CMark
import DocGen4.Output.Template
import Lean.Data.Parsec

open Lean

namespace DocGen4
namespace Output

open Xml Parser Lean Parsec

def manyDocument : Parsec (Array Element) := many (prolog *> element <* many Misc) <* eof

partial def addAttributes : Element → Element
| (Element.Element name attrs contents) => 
  -- heading only
  if name = "h1" ∨ name = "h2" ∨ name = "h3" ∨ name = "h4" ∨ name = "h5" ∨ name = "h6" then
    let id := "".intercalate (contents.map toString).toList 
      |>.dropWhile (λ c => !(c.isAlphanum ∨ c = '-'))
      |>.toLower
      |>.replace " " "-"
    let anchorAttributes := Std.RBMap.empty
      |>.insert "class" "hover-link"
      |>.insert "href" s!"#{id}"
    let anchor := Element.Element "a" anchorAttributes #[Content.Character "#"]
    let newAttrs := attrs
      |>.insert "id" id
      |>.insert "class" "markdown-heading"
    let newContents :=
      contents.map (λ c => match c with
      | Content.Element e => Content.Element (addAttributes e)
      | _ => c)
      |>.push (Content.Element anchor)
    ⟨ name, newAttrs, newContents⟩ 
  else
    let newContents := contents.map λ c => match c with
      | Content.Element e => Content.Element (addAttributes e)
      | _ => c
    ⟨ name, attrs, newContents ⟩ 

def docStringToHtml (s : String) : Html := 
  let rendered := CMark.renderHtml s
  let attributed := match manyDocument rendered.mkIterator with
  | Parsec.ParseResult.success _ res =>  "".intercalate (res.map addAttributes |>.map toString).toList
  | _ => rendered
  Html.text attributed

end Output
end DocGen4
