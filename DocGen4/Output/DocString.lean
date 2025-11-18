import MD4Lean
import DocGen4.Output.Template
import Std.Internal.Parsec
import UnicodeBasic

open Lean Xml Parser DocGen4.Process

open Std.Internal.Parsec
open Std.Internal.Parsec.String

namespace DocGen4
namespace Output

/-- Auxiliary function for `splitAround`. -/
@[specialize] partial def splitAroundAux (s : String) (p : Char → Bool) (b i : String.Pos.Raw) (r : List String) : List String :=
  if String.Pos.Raw.atEnd s i then
    let r := (String.Pos.Raw.extract s b i)::r
    r.reverse
  else
    let c := String.Pos.Raw.get s i
    if p c then
      let i := String.Pos.Raw.next s i
      splitAroundAux s p i i (c.toString :: String.Pos.Raw.extract s b (i.decreaseBy 1) :: r)
    else
      splitAroundAux s p b (String.Pos.Raw.next s i) r

/--
  Similar to `String.split` in Lean core, but keeps the separater.
  e.g. `splitAround "a,b,c" (fun c => c = ',') = ["a", ",", "b", ",", "c"]`
-/
def splitAround (s : String) (p : Char → Bool) : List String := splitAroundAux s p 0 0 []

instance : Inhabited Element := ⟨"", Std.TreeMap.empty, #[]⟩

/-- Parse an array of Xml/Html document from String. -/
def manyDocument : Parser (Array Element) := many (prolog *> element <* many Misc) <* eof

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
      s.splitToList pattern
      |>.filter (!·.isEmpty)
      |> replacement.intercalate
    unicodeToDrop (c : Char) : Bool :=
      -- punctuation (`P`), separator (`Z`), other (`C`)
      c ∈ Unicode.GC.P ||| Unicode.GC.Z ||| Unicode.GC.C

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
        match res.moduleInfo[currentName]! |>.members |> filterDocInfo |>.find? (sameEnd ·.getName name) with
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

/-- Apply function `modifyElement` to an array of `Lean.Xml.Content`s. -/
def modifyContents (contents : Array Content) (funName : String)
    (modifyElement : Element → String → HtmlM Element) :
    HtmlM (Array Content) := do
  let modifyContent (c : Content) : HtmlM Content := do
    match c with
    | Content.Element e =>
      pure (.Element (← modifyElement e funName))
    | _ =>
      pure c
  contents.mapM modifyContent

/-- Apply function `modifyElement` to an array of `Lean.Xml.Element`s. -/
def modifyElements (elements : Array Element) (funName : String)
    (modifyElement : Element → String → HtmlM Element) :
    HtmlM (Array Element) := do
  elements.mapM (modifyElement · funName)

/-- Add attributes for heading. -/
def addHeadingAttributes (el : Element) (funName : String)
    (modifyElement : Element → String → HtmlM Element) :
    HtmlM Element := do
  match el with
  | Element.Element name attrs contents => do
    let id := xmlGetHeadingId el
    let anchorAttributes := Std.TreeMap.empty
      |>.insert "class" "hover-link"
      |>.insert "href" s!"#{id}"
    let anchor := Element.Element "a" anchorAttributes #[Content.Character "#"]
    let newAttrs := attrs
      |>.insert "id" id
      |>.insert "class" "markdown-heading"
    let newContents ← modifyContents contents funName modifyElement
    return ⟨ name, newAttrs, newContents
      |>.push (Content.Character " ")
      |>.push (Content.Element anchor) ⟩

/-- Find a bibitem if `href` starts with `thePrefix`. -/
def findBibitem? (href : String) (thePrefix : String := "") : HtmlM (Option BibItem) := do
  if href.startsWith thePrefix then
    pure <| (← read).refsMap[href.drop thePrefix.length]?
  else
    pure .none

/-- Extend anchor links. -/
def extendAnchor (el : Element) (funName : String) : HtmlM Element := do
  match el with
  | Element.Element name attrs contents =>
    match attrs.get? "href" with
    | some href =>
      let bibitem ← findBibitem? href "references.html#ref_"
      let attrs := attrs.insert "href" (← extendLink href)
      match bibitem with
      | .some bibitem =>
        let newBackref ← addBackref bibitem.citekey funName
        let changeName : Bool :=
          if let #[.Character s] := contents then
            s == bibitem.citekey
          else
            false
        let newContents : Array Content :=
          if changeName then #[.Character bibitem.tag] else contents
        let attrs := attrs.insert "title" bibitem.plaintext
          |>.insert "id" s!"_backref_{newBackref.index}"
        return ⟨ name, attrs, newContents ⟩
      | .none =>
        return ⟨ name, attrs, contents ⟩
    | none => return ⟨ name, attrs, contents ⟩

/-- Automatically add intra documentation link for inline code span. -/
def autoLink (el : Element) : HtmlM Element := do
  match el with
  | Element.Element name attrs contents =>
    let mut newContents := #[]
    for c in contents do
      match c with
      | Content.Character s =>
        newContents := newContents ++ (← splitAround s unicodeToSplit |>.mapM linkify).flatten
      | _ => newContents := newContents.push c
    return ⟨ name, attrs, newContents ⟩
  where
    linkify s := do
      let link? ← nameToLink? s
      match link? with
      | some link =>
        let attributes := Std.TreeMap.empty.insert "href" link
        return [Content.Element <| Element.Element "a" attributes #[Content.Character s]]
      | none =>
        let sHead := s.dropRightWhile (· != '.')
        let sTail := s.takeRightWhile (· != '.')
        let link'? ← nameToLink? sTail
        match link'? with
        | some link' =>
          let attributes := Std.TreeMap.empty.insert "href" link'
          return [
            Content.Character sHead,
            Content.Element <| Element.Element "a" attributes #[Content.Character sTail]
          ]
        | none =>
          return [Content.Character s]
    unicodeToSplit (c : Char) : Bool :=
      -- separator (`Z`), other (`C`)
      c ∈ Unicode.GC.Z ||| Unicode.GC.C

/-- Core function of modifying the cmark rendered docstring html. -/
partial def modifyElement (element : Element) (funName : String) : HtmlM Element :=
  match element with
  | el@(Element.Element name attrs contents) => do
    -- add id and class to <h_></h_>
    if name = "h1" ∨ name = "h2" ∨ name = "h3" ∨ name = "h4" ∨ name = "h5" ∨ name = "h6" then
      addHeadingAttributes el funName modifyElement
    -- extend relative href for <a></a>
    else if name = "a" then
      extendAnchor el funName
    -- auto link for inline <code></code>
    else if name = "code" ∧
      -- don't linkify code blocks explicitly tagged with a language other than lean
      (((attrs.get? "class").getD "").splitOn.all (fun s => s == "language-lean" || !s.startsWith "language-")) then
      autoLink el
    -- recursively modify
    else
      return ⟨ name, attrs, ← modifyContents contents funName modifyElement ⟩

/-- Find all references in a markdown text. -/
partial def findAllReferences (refsMap : Std.HashMap String BibItem) (s : String) (i : String.Pos.Raw := 0)
    (ret : Std.HashSet String := ∅) : Std.HashSet String :=
  let lps := s.posOfAux '[' s.rawEndPos i
  if lps < s.rawEndPos then
    let lpe := s.posOfAux ']' s.rawEndPos lps
    if lpe < s.rawEndPos then
      let citekey := Substring.Raw.toString ⟨s, ⟨lps.1 + 1⟩, lpe⟩
      match refsMap[citekey]? with
      | .some _ => findAllReferences refsMap s lpe (ret.insert citekey)
      | .none => findAllReferences refsMap s lpe ret
    else
      ret
  else
    ret

/-- Convert docstring to Html. -/
def docStringToHtml (docString : String) (funName : String) : HtmlM (Array Html) := do
  let refsMarkdown := "\n\n" ++ (String.join <|
    (findAllReferences (← read).refsMap docString).toList.map fun s =>
      s!"[{s}]: references.html#ref_{s}\n")
  match MD4Lean.renderHtml (docString ++ refsMarkdown) with
  | .some rendered =>
    match manyDocument rendered.mkIterator with
    | .success _ res =>
      let newRes ← modifyElements res funName modifyElement
      -- TODO: use `toString` instead of `eToStringEscaped`
      -- once <https://github.com/leanprover/lean4/issues/4411> is fixed
      return (newRes.map fun x => Html.raw (eToStringEscaped x))
    | _ =>
      addError <| "Error: failed to postprocess HTML:\n" ++ rendered
      return #[.raw "<span style='color:red;'>Error: failed to postprocess: </span>", .raw rendered]
  | .none =>
    addError <| "Error: failed to parse markdown:\n" ++ docString
    return #[.raw "<span style='color:red;'>Error: failed to parse markdown: </span>", .text docString]

end Output
end DocGen4
