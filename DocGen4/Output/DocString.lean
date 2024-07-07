import MD4Lean
import DocGen4.Output.Template
import Lean.Data.Parsec
import UnicodeBasic

open Lean Xml Parser Parsec DocGen4.Process

namespace DocGen4
namespace Output

/-- Auxiliary function for `splitAround`. -/
@[specialize] partial def splitAroundAux (s : String) (p : Char → Bool) (b i : String.Pos) (r : List String) : List String :=
  if s.atEnd i then
    let r := (s.extract b i)::r
    r.reverse
  else
    let c := s.get i
    if p c then
      let i := s.next i
      splitAroundAux s p i i (c.toString::s.extract b (i-⟨1⟩)::r)
    else
      splitAroundAux s p b (s.next i) r

/--
  Similar to `String.split` in Lean core, but keeps the separater.
  e.g. `splitAround "a,b,c" (fun c => c = ',') = ["a", ",", "b", ",", "c"]`
-/
def splitAround (s : String) (p : Char → Bool) : List String := splitAroundAux s p 0 0 []

instance : Inhabited Element := ⟨"", Lean.RBMap.empty, #[]⟩

/-- Parse an array of Xml/Html document from String. -/
def manyDocument : Parsec (Array Element) := many (prolog *> element <* many Misc) <* eof

/--
  Generate id for heading elements, with the following rules:

  1. Characters in `letter`, `mark`, `number` and `symbol` unicode categories are preserved.
  2. Any sequences of Characters in `punctuation`, `separator` and `other` categories are replaced by a single dash.
  3. Cases (upper and lower) are preserved.
  4. Xml/Html tags are ignored.
-/
partial def xmlGetHeadingId (el : Xml.Element) : String :=
  elementToPlainText el |> replaceCharSeq unicodeToDrop "-"
  where
    elementToPlainText el := match el with
    | (Element.Element _ _ contents) =>
      "".intercalate (contents.toList.map contentToPlainText)
    contentToPlainText c := match c with
    | Content.Element el => elementToPlainText el
    | Content.Comment _ => ""
    | Content.Character s => s
    replaceCharSeq pattern replacement s :=
      s.split pattern
      |>.filter (!·.isEmpty)
      |> replacement.intercalate
    unicodeToDrop (c : Char) : Bool :=
      let cats := [
        Unicode.GeneralCategory.P, -- punctuation
        Unicode.GeneralCategory.Z, -- separator
        Unicode.GeneralCategory.C -- other
      ]
      cats.any (Unicode.isInGeneralCategory c)

/--
  This function try to find the given name, both globally and in current module.

  For global search, a precise name is need. If the global search fails, the function
  tries to find a local one that ends with the given search name.
-/
def nameToLink? (s : String) : HtmlM (Option String) := do
  let res ← getResult
  if s.endsWith ".lean" && s.contains '/' then
    return (← getRoot) ++ s.dropRight 5 ++ ".html"
  else if let some name := Lean.Syntax.decodeNameLit ("`" ++ s) then
    -- with exactly the same name
    if res.name2ModIdx.contains name then
      declNameToLink name
    -- module name
    else if res.moduleNames.contains name then
      moduleNameToLink name
    -- find similar name in the same module
    else
      match (← getCurrentName) with
      | some currentName =>
        match res.moduleInfo.find! currentName |>.members |> filterDocInfo |>.find? (sameEnd ·.getName name) with
        | some info =>
          declNameToLink info.getName
        | _ => return none
      | _ => return none
  else
    return none
  where
    -- check if two names have the same ending components
    sameEnd n1 n2 :=
      List.zip n1.componentsRev n2.componentsRev
      |>.all fun ⟨a, b⟩ => a == b

/--
  Extend links with following rules:

  1. if the link starts with `##`, a name search is used, and will use `find` if not found
  2. if the link starts with `#`, it's treated as id link, no modification
  3. if the link starts with `http`, it's an absolute one, no modification
  4. otherwise it's a relative link, extend it with base url
-/
def extendLink (s : String)  : HtmlM String := do
  -- for intra doc links
  if s.startsWith "##" then
    if let some link ← nameToLink? (s.drop 2) then
      return link
    else
      return s!"{← getRoot}find/?pattern={s.drop 2}#doc"
  -- for id
  else if s.startsWith "#" then
    return s
  -- for absolute and relative urls
  else if s.startsWith "http" then
    return s
  else return ((← getRoot) ++ s)

/-- Add attributes for heading. -/
def addHeadingAttributes (el : Element) (modifyElement : Element → HtmlM Element) : HtmlM Element := do
  match el with
  | Element.Element name attrs contents => do
    let id := xmlGetHeadingId el
    let anchorAttributes := Lean.RBMap.empty
      |>.insert "class" "hover-link"
      |>.insert "href" s!"#{id}"
    let anchor := Element.Element "a" anchorAttributes #[Content.Character "#"]
    let newAttrs := attrs
      |>.insert "id" id
      |>.insert "class" "markdown-heading"
    let newContents := (←
      contents.mapM (fun c => match c with
      | Content.Element e => return Content.Element (← modifyElement e)
      | _ => pure c))
      |>.push (Content.Character " ")
      |>.push (Content.Element anchor)
    return ⟨ name, newAttrs, newContents⟩

/-- Extend anchor links. -/
def extendAnchor (el : Element) : HtmlM Element := do
  match el with
  | Element.Element name attrs contents =>
    let newAttrs ← match attrs.find? "href" with
    | some href => pure (attrs.insert "href" (← extendLink href))
    | none => pure attrs
    return ⟨ name, newAttrs, contents⟩

/-- Automatically add intra documentation link for inline code span. -/
def autoLink (el : Element) : HtmlM Element := do
  match el with
  | Element.Element name attrs contents =>
    let mut newContents := #[]
    for c in contents do
      match c with
      | Content.Character s =>
        newContents := newContents ++ (← splitAround s unicodeToSplit |>.mapM linkify).join
      | _ => newContents := newContents.push c
    return ⟨ name, attrs, newContents ⟩
  where
    linkify s := do
      let link? ← nameToLink? s
      match link? with
      | some link =>
        let attributes := Lean.RBMap.empty.insert "href" link
        return [Content.Element <| Element.Element "a" attributes #[Content.Character s]]
      | none =>
        let sHead := s.dropRightWhile (· != '.')
        let sTail := s.takeRightWhile (· != '.')
        let link'? ← nameToLink? sTail
        match link'? with
        | some link' =>
          let attributes := Lean.RBMap.empty.insert "href" link'
          return [
            Content.Character sHead,
            Content.Element <| Element.Element "a" attributes #[Content.Character sTail]
          ]
        | none =>
          return [Content.Character s]
    unicodeToSplit (c : Char) : Bool :=
      let cats := [
        Unicode.GeneralCategory.Z, -- separator
        Unicode.GeneralCategory.C -- other
      ]
      cats.any (Unicode.isInGeneralCategory c)

/-- Core function of modifying the cmark rendered docstring html. -/
partial def modifyElement (element : Element) : HtmlM Element :=
  match element with
  | el@(Element.Element name attrs contents) => do
    -- add id and class to <h_></h_>
    if name = "h1" ∨ name = "h2" ∨ name = "h3" ∨ name = "h4" ∨ name = "h5" ∨ name = "h6" then
      addHeadingAttributes el modifyElement
    -- extend relative href for <a></a>
    else if name = "a" then
      extendAnchor el
    -- auto link for inline <code></code>
    else if name = "code" ∧
      -- don't linkify code blocks explicitly tagged with a language other than lean
      (((attrs.find? "class").getD "").splitOn.all (fun s => s == "language-lean" || !s.startsWith "language-")) then
      autoLink el
    -- recursively modify
    else
      let newContents ← contents.mapM fun c => match c with
        | Content.Element e => return Content.Element (← modifyElement e)
        | _ => pure c
      return ⟨ name, attrs, newContents ⟩

-- TODO: remove the following 3 functions
-- once <https://github.com/leanprover/lean4/issues/4411> is fixed

private def _root_.Lean.Xml.Attributes.toStringEscaped (as : Attributes) : String :=
  as.fold (fun s n v => s ++ s!" {n}=\"{Html.escape v}\"") ""

mutual

private partial def _root_.Lean.Xml.eToStringEscaped : Element → String
| Element.Element n a c => s!"<{n}{a.toStringEscaped}>{c.map cToStringEscaped |>.foldl (· ++ ·) ""}</{n}>"

private partial def _root_.Lean.Xml.cToStringEscaped : Content → String
| Content.Element e => eToStringEscaped e
| Content.Comment c => s!"<!--{c}-->"
| Content.Character c => Html.escape c

end

/-- Convert docstring to Html. -/
def docStringToHtml (s : String) : HtmlM (Array Html) := do
  let rendered := match MD4Lean.renderHtml s with
    | .some res => res
    | _ => "" -- TODO: should print some error message
  match manyDocument rendered.mkIterator with
  | Parsec.ParseResult.success _ res =>
    -- TODO: use `toString` instead of `eToStringEscaped`
    -- once <https://github.com/leanprover/lean4/issues/4411> is fixed
    res.mapM fun x => do return Html.raw <| eToStringEscaped (← modifyElement x)
  | _ => return #[Html.raw rendered]

end Output
end DocGen4
