import CMark
import DocGen4.Output.Template
import Lean.Data.Parsec
import Unicode.General.GeneralCategory

open Lean Unicode

namespace DocGen4
namespace Output

open Xml Parser Lean Parsec

instance : Inhabited Element := ⟨"", Std.RBMap.empty, #[]⟩

/-- Parse an array of Xml/Html document from String. -/
def manyDocument : Parsec (Array Element) := many (prolog *> element <* many Misc) <* eof

/--
  Generate id for heading elements, with the following rules:

  1. Characters in `letter`, `number` and `symbol` unicode categories are preserved.
  2. Any sequences of Characters in `mark`, `punctuation`, `separator` and `other` categories are replaced by a single dash.
  3. Cases (upper and lower) are preserved.
  4. Xml/Html tags are ignored.
-/
partial def xmlGetHeadingId (el : Xml.Element) : String :=
  elementToPlainText el |> replaceCharSeq unicodeToDrop "-"
  where
    elementToPlainText el := match el with 
    | (Element.Element name attrs contents) => 
      "".intercalate (contents.toList.map contentToPlainText)
    contentToPlainText c := match c with
    | Content.Element el => elementToPlainText el
    | Content.Comment _ => ""
    | Content.Character s => s
    replaceCharSeq pattern replacement s :=
      s.split pattern
      |>.filter (!·.isEmpty)
      |> replacement.intercalate
    unicodeToDrop char := 
      charInGeneralCategory char GeneralCategory.mark ||
      charInGeneralCategory char GeneralCategory.punctuation ||
      charInGeneralCategory char GeneralCategory.separator ||
      charInGeneralCategory char GeneralCategory.other  

/--
  This function try to find the given name, both globally and in current module.

  For global search, a precise name is need. If the global search fails, the function
  tries to find a local one that ends with the given search name.
-/
def nameToLink? (s : String) : HtmlM (Option String) := do
  let res ← getResult
  let name := s.splitOn "." |>.foldl (λ acc part => Name.mkStr acc part) Name.anonymous
  -- with exactly the same name
  if res.name2ModIdx.contains name then
     declNameToLink name
  -- find similar name in the same module
  else
    match (← getCurrentName) with
    | some currentName =>
      match (res.moduleInfo.find! currentName).members.find? (sameEnd ·.getName.toString s) with
      | some info => 
        declNameToLink info.getName
      | _ => pure none
    | _ => pure none
  where
    sameEnd n1 n2 :=
      n1.endsWith n2 ||
      (n2.endsWith (n1.splitOn ".").getLast!)


/--
  Extend links with following rules:

  1. if the link starts with `##`, a name search is used and will panic if not found
  2. if the link starts with `#`, it's treated as id link, no modification
  3. if the link starts with `http`, it's an absolute one, no modification
  4. otherwise it's a relative link, extend it with base url
-/
def extendLink (s : String)  : HtmlM String := do
  -- for intra doc links
  if s.startsWith "##" then
    if let some link ← nameToLink? (s.drop 2) then
      pure link
    else
      panic! s!"Cannot find {s.drop 2}, only full name and abbrev in current module is supported"
  -- for id 
  else if s.startsWith "#" then
    pure s
  -- for absolute and relative urls
  else if s.startsWith "http" then 
    pure s
  else pure ((←getRoot) ++ s)

/-- Add attributes for heading. -/
def addHeadingAttributes (el : Element) (modifyElement : Element → HtmlM Element) : HtmlM Element := do
  match el with
  | Element.Element name attrs contents => do
    let id := xmlGetHeadingId el
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

/-- Extend anchor links. -/
def extendAnchor (el : Element) : HtmlM Element := do
  match el with
  | Element.Element name attrs contents =>
    let root ← getRoot
    let newAttrs ← match (attrs.find? "href").map extendLink with
    | some href => href.map (attrs.insert "href")
    | none => pure attrs
    pure ⟨ name, newAttrs, contents⟩

/-- Automatically add intra documentation link for inline code span. -/
def autoLink (el : Element) : HtmlM Element := do
  match el with
  | Element.Element name attrs contents =>
    let mut newContents := #[]
    for c in contents do
      match c with
      | Content.Character s =>
        newContents := newContents ++ (← s.splitOn.mapM linkify).intersperse (Content.Character " ")
      | _ => newContents := newContents.push c
    pure ⟨ name, attrs, newContents ⟩
  where
    linkify s := do
      let link? ← nameToLink? s
      match link? with
      | some link => 
        let res ← getResult
        let attributes := Std.RBMap.empty.insert "href" link
        pure $ Content.Element $ Element.Element "a" attributes #[Content.Character s]
      | none => pure $ Content.Character s

/-- Core function of modifying the cmark rendered docstring html. -/
partial def modifyElement (element : Element) (linkCode : Bool := true) : HtmlM Element :=
  match element with
  | el@(Element.Element name attrs contents) => do
    -- add id and class to <h_></h_>
    if name = "h1" ∨ name = "h2" ∨ name = "h3" ∨ name = "h4" ∨ name = "h5" ∨ name = "h6" then
      addHeadingAttributes el modifyElement
    -- extend relative href for <a></a>
    else if name = "a" then
      extendAnchor el
    -- auto link for inline <code></code>
    else if name = "code" ∧ linkCode = true then
      autoLink el
    -- recursively modify
    else
      let newContents ← contents.mapM λ c => match c with
        -- disable auto link for code blocks
        | Content.Element e => (modifyElement e (name ≠ "pre")).map Content.Element
        | _ => pure c
      pure ⟨ name, attrs, newContents ⟩ 

/-- Convert docstring to Html. -/
def docStringToHtml (s : String) : HtmlM (Array Html) := do
  let rendered := CMark.renderHtml s
  match manyDocument rendered.mkIterator with
  | Parsec.ParseResult.success _ res => 
    res.mapM λ x => do
      pure (Html.text $ toString (← modifyElement x))
  | _ => pure #[Html.text rendered]

end Output
end DocGen4
