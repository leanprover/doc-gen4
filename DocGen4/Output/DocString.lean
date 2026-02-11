import MD4Lean
import DocGen4.Output.Template
import UnicodeBasic

open Lean DocGen4.Process

namespace DocGen4
namespace Output

open scoped DocGen4.Jsx
open DocGen4 (Raw escape)

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

/--
  This function try to find the given name, both globally and in current module.

  For global search, a precise name is need. If the global search fails, the function
  tries to find a local one that ends with the given search name.
-/
def nameToLink? (s : String) : HtmlM (Option String) := do
  let res ← getResult
  if s.endsWith ".lean" && s.contains '/' then
    return (← getRoot) ++ s.dropEnd 5 ++ ".html"
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
    if let some link ← nameToLink? (s.drop 2).copy then
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

/-- Find a bibitem if `href` starts with `thePrefix`. -/
def findBibitem? (href : String) (thePrefix : String := "") : HtmlM (Option BibItem) := do
  if href.startsWith thePrefix then
    pure <| (← read).refsMap[href.drop thePrefix.length |>.copy]?
  else
    pure .none

/-- Flattens an array of `MD4Lean.AttrText` to a plain `String`. -/
def attrTextToString (ts : Array MD4Lean.AttrText) : String :=
  String.join <|
  ts.toList.map fun
    | .normal s => s
    | .entity s => s
    -- CommonMark requires that null characters be replaced by the Unicode replacement character.
    | .nullchar => "\uFFFD"

mutual
/-- Extracts plain text from a single `MD4Lean.Text`, ignoring all formatting. -/
def textToPlaintext (t : MD4Lean.Text) : String :=
  match t with
  | .normal s => s
  | .nullchar => "\uFFFD"
  | .br _ => "\n"
  | .softbr _ => "\n"
  | .entity s => s
  | .em ts => textsToPlaintext ts
  | .strong ts => textsToPlaintext ts
  | .u ts => textsToPlaintext ts
  | .del ts => textsToPlaintext ts
  | .a _ _ _ ts => textsToPlaintext ts
  | .img _ _ alt => textsToPlaintext alt
  | .code ss => String.join ss.toList
  | .latexMath ss => String.join ss.toList
  | .latexMathDisplay ss => String.join ss.toList
  | .wikiLink _ ts => textsToPlaintext ts
termination_by sizeOf t

/-- Extract plain text from an array of `MD4Lean.Text`. -/
def textsToPlaintext (ts : Array MD4Lean.Text) : String :=
  ts.foldl (init := "") fun str t => str ++ textToPlaintext t
termination_by sizeOf ts
end

/--
  Generates an `id` attribute for heading elements, with the following rules:

  1. Characters in `letter`, `mark`, `number` and `symbol` Unicode categories are preserved.
  2. Any sequences of characters in `punctuation`, `separator` and `other` categories are replaced by a single dash.
  3. Cases (upper and lower) are preserved.
-/
def mdGetHeadingId (texts : Array MD4Lean.Text) : String :=
  textsToPlaintext texts |> replaceCharSeq unicodeToDrop "-"
  where
    textsToPlaintext ts := String.join (ts.toList.map textToPlaintext)
    replaceCharSeq pattern replacement s :=
      s.splitToList pattern
      |>.filter (!·.isEmpty)
      |> replacement.intercalate
    unicodeToDrop (c : Char) : Bool :=
      -- punctuation (`P`), separator (`Z`), other (`C`)
      c ∈ Unicode.GC.P ||| Unicode.GC.Z ||| Unicode.GC.C

/-- Checks whether a fenced code block language allows auto-linking. -/
def isLeanCode (lang : Array MD4Lean.AttrText) : Bool :=
  let s := attrTextToString lang
  s.isEmpty || s == "lean"

/--
Automatically adds intra-documentation links for code content.
-/
def autoLinkInline (ss : Array String) : HtmlM Unit := do
  for s in ss do
    let parts := splitAround s unicodeToSplit
    for part in parts do
      match ← nameToLink? part with
      | some link =>
        (<a href={link}>{part}</a>)
      | none =>
        let sHead := part.dropEndWhile (· != '.') |>.copy
        let sTail := part.takeEndWhile (· != '.') |>.copy
        match ← nameToLink? sTail with
        | some link =>
          if !sHead.isEmpty then
            Html.text sHead
          (<a href={link}>{sTail}</a>)
        | none =>
          Html.text part
  where
    unicodeToSplit (c : Char) : Bool :=
      -- separator (`Z`), other (`C`)
      c ∈ Unicode.GC.Z ||| Unicode.GC.C

mutual

/--
Renders a single `MD4Lean.Text` inline element to HTML, while processing custom extensions such as
bibliography items. `inLink` suppresses auto-linking inside `<a>` to avoid nested anchors.
-/
partial def renderText (t : MD4Lean.Text) (funName : String) (inLink : Bool := false) : HtmlM Unit := do
  match t with
  | .normal s => Html.text s
  | .nullchar => Html.rawText "\uFFFD"
  | .br _ => Html.rawText "<br>\n" -- This avoids <br></br>, which is incorrect HTML5
  | .softbr _ => Html.rawText "\n"
  | .entity s => Html.rawText s
  | .em ts => (<em>{renderTexts ts funName inLink}</em>)
  | .strong ts => (<strong>{renderTexts ts funName inLink}</strong>)
  | .u ts => (<u>{renderTexts ts funName inLink}</u>)
  | .del ts => (<del>{renderTexts ts funName inLink}</del>)
  | .a href title _isAuto children =>
    let hrefStr := attrTextToString href
    let titleStr := attrTextToString title
    let bibitem ← findBibitem? hrefStr "references.html#ref_"
    let extHref ← extendLink hrefStr
    match bibitem with
    | .some bibitem =>
      let newBackref ← addBackref bibitem.citekey funName
      let changeName : Bool :=
        if let #[.normal s] := children then
          s == bibitem.citekey
        else
          false
      (<a href={extHref} title={bibitem.plaintext} id={s!"_backref_{newBackref.index}"}>
        {if changeName then Html.text bibitem.tag
         else renderTexts children funName (inLink := true)}
      </a>)
    | .none =>
      let mut attrs : Array (String × String) := #[("href", extHref)]
      if !titleStr.isEmpty then
        attrs := attrs.push ("title", titleStr)
      (<a [attrs]>{renderTexts children funName (inLink := true)}</a>)
  | .img src title alt =>
    let srcStr := escape (attrTextToString src)
    let titleStr := escape (attrTextToString title)
    let altTexts := alt.toList.map textToPlaintext
    let altStr := escape (String.join altTexts)
    let mut s := s!"<img src=\"{srcStr}\" alt=\"{altStr}\""
    if !titleStr.isEmpty then
      s := s ++ s!" title=\"{titleStr}\""
    s := s ++ ">"
    Html.rawText s
  | .code ss =>
    (<code>
      {if inLink then Html.text (String.join ss.toList)
       else autoLinkInline ss}
    </code>)
  -- Math is rendered with dollar signs because MathJax will later render them
  | .latexMath ss =>
    let content := String.join ss.toList
    Html.rawText s!"${escape content}$"
  -- Math is rendered with dollar signs because MathJax will later render them
  | .latexMathDisplay ss =>
    let content := String.join ss.toList
    Html.rawText s!"$${escape content}$$"
  | .wikiLink target children =>
    let targetStr := attrTextToString target
    Html.element "x-wikilink" #[("data-target", targetStr)] (renderTexts children funName inLink)

/-- Render an array of `MD4Lean.Text` inline elements to HTML. -/
partial def renderTexts (texts : Array MD4Lean.Text) (funName : String) (inLink : Bool := false) : HtmlM Unit := do
  for t in texts do
    renderText t funName inLink

/-- Render a single `MD4Lean.Block` element to HTML. -/
partial def renderBlock (block : MD4Lean.Block) (funName : String) (tight : Bool := false) : HtmlM Unit := do
  match block with
  | .p texts =>
    if tight then
      renderTexts texts funName
    else
      (<p>{renderTexts texts funName}</p>)
  | .ul isTight _mark items =>
    (<ul>{items.forM fun item => renderLi item funName isTight}</ul>)
  | .ol isTight start _mark items =>
    let attrs : Array (String × String) :=
      if start != 1 then #[("start", toString start)] else #[]
    (<ol [attrs]>{items.forM fun item => renderLi item funName isTight}</ol>)
  | .hr => Html.rawText "<hr>\n"
  | .header level texts =>
    -- Dynamic tag name requires Html.element
    let id := mdGetHeadingId texts
    let tag := s!"h{level}"
    Html.element tag #[("id", id), ("class", "markdown-heading")] do
      renderTexts texts funName
      Html.text " "
      (<a class="hover-link" href={s!"#{id}"}>#</a>)
  | .code _info lang _fenceChar content =>
    let codeAttrs : Array (String × String) :=
      let langStr := attrTextToString lang
      if !langStr.isEmpty then #[("class", s!"language-{langStr}")] else #[]
    (<pre><code [codeAttrs]>
      {if isLeanCode lang then autoLinkInline content
       else Html.text (String.join content.toList)}
    </code></pre>)
  | .html content =>
    Html.rawText (String.join content.toList)
  | .blockquote blocks =>
    (<blockquote>{blocks.forM (renderBlock · funName)}</blockquote>)
  | .table head body =>
    (<table>
      <thead>
        <tr>{head.forM fun cell => (<th>{renderTexts cell funName}</th>)}</tr>
      </thead>
      <tbody>
        {body.forM fun row =>
          (<tr>{row.forM fun cell => (<td>{renderTexts cell funName}</td>)}</tr>)}
      </tbody>
    </table>)

/-- Render a list item to HTML. -/
partial def renderLi (li : MD4Lean.Li MD4Lean.Block) (funName : String) (tight : Bool) : HtmlM Unit := do
  (<li>
    {do if li.isTask then
          let checked := li.taskChar == some 'x' || li.taskChar == some 'X'
          if checked then
            Html.rawText "<input type=\"checkbox\" checked=\"\" disabled=\"\">"
          else
            Html.rawText "<input type=\"checkbox\" disabled=\"\">"}
    {li.contents.forM fun b => renderBlock b funName tight}
  </li>)

end

/-- Find all references in a markdown text. -/
partial def findAllReferences (refsMap : Std.HashMap String BibItem) (s : String) (i : s.Pos := s.startPos)
    (ret : Std.HashSet String := ∅) : Std.HashSet String :=
  let lps := i.find '['
  if hs : lps ≠ s.endPos then
    let lpe := lps.find ']'
    if lpe ≠ s.endPos then
      let citekey := s.extract (lps.next hs) lpe
      match refsMap[citekey]? with
      | .some _ => findAllReferences refsMap s lpe (ret.insert citekey)
      | .none => findAllReferences refsMap s lpe ret
    else
      ret
  else
    ret

/-- Convert docstring to Html, writing directly to stream. -/
def docStringToHtml (docString : String ⊕ VersoDocString) (funName : String) : HtmlM Unit := do
  let docString :=
    match docString with
    | .inl md => md
    -- TODO: natively render Verso docstrings
    | .inr v => versoDocToMarkdown v
  let refsMarkdown := "\n\n" ++ (String.join <|
    (findAllReferences (← read).refsMap docString).toList.map fun s =>
      s!"[{s}]: references.html#ref_{s}\n")
  let flags := MD4Lean.MD_DIALECT_GITHUB ||| MD4Lean.MD_FLAG_LATEXMATHSPANS ||| MD4Lean.MD_FLAG_NOHTML
  match MD4Lean.parse (docString ++ refsMarkdown) flags with
  | .some doc =>
    for block in doc.blocks do
      renderBlock block funName
  | .none =>
    addError <| "Error: failed to parse markdown:\n" ++ docString
    (<span style="color:red;">Error: failed to parse markdown: </span>)
    Html.text docString

end Output
end DocGen4
