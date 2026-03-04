/-
Copyright (c) 2021 Henrik Böving. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Henrik Böving, David Thrane Christiansen
-/
import Lean
import SQLite

/-!
# Rendered Code and Format Code

Lean's pretty printer produces `CodeWithInfos` (a `TaggedText SubexprInfo`), which carries rich
metadata such as expression types, universe levels, elaboration state, etc. This is too large to
serialize to disk.

## FormatCode (stored in the database)

`FormatCode` preserves the pretty printer's document structure — groups, nesting, soft line breaks —
so that clients can render at any width. It stores a `Lean.Format` with the `Format.tag` indices
replaced by dense, de-duplicated indices into a `Array RenderedCode.Tag`. It is produced from the
output of `ppExprWithInfos` by `toFormatCode` and rendered to `RenderedCode` (at any width) by
`FormatCode.render`.

## RenderedCode (used for HTML output)

`RenderedCode` is a `TaggedText RenderedCode.Tag` that keeps only the information needed for HTML
rendering: which tokens are declaration references (for linking), which are sorts (for linking to
the foundational types page), and which are keywords or strings (for syntax highlighting). It is
produced either by `FormatCode.render` (from the database) or by `renderTagged` (directly from a
`CodeWithInfos` for live rendering without going through the database).
-/

namespace DocGen4

open Lean Widget Elab Meta
open SQLite.Blob

/--
Used in `RenderedCode.Tag` to track what kind of sort this is.
-/
inductive SortFormer where
  | type | prop | sort
deriving ToJson, FromJson, BEq, Hashable, Repr, ToBinary, FromBinary

/--
Tags for code elements in rendered code. Used to indicate semantic meaning
for HTML rendering (linking, syntax highlighting).
-/
inductive RenderedCode.Tag where
  | keyword
  | string
  | const (name : Lean.Name)
  | sort (former : Option SortFormer)
  | otherExpr
  /-- A local variable occurrence. `idx` indexes into `FormatCode.localVars`;
  `isBinder` is true at the binding site and false at use sites. -/
  | localVar (idx : Nat) (isBinder : Bool)
deriving BEq, Hashable, Repr, ToBinary, FromBinary

partial instance [ToBinary α] : ToBinary (Lean.Widget.TaggedText α) where
  serializer := go
where
  go
    | .text s, b => b.push 0 |> ToBinary.serializer s
    | .tag a t, b => b.push 1 |> ToBinary.serializer a |> go t
    | .append xs, b =>
      have : ToBinary (Lean.Widget.TaggedText α) := ⟨go⟩
      b.push 2 |> ToBinary.serializer xs

partial instance [FromBinary α] : FromBinary (Lean.Widget.TaggedText α) where
  deserializer := go
where
  go := do
    match (← .byte) with
    | 0 => .text <$> FromBinary.deserializer
    | 1 => .tag <$> FromBinary.deserializer <*> go
    | 2 =>
      have : FromBinary (Lean.Widget.TaggedText α) := ⟨go⟩
      .append <$> FromBinary.deserializer
    | other => throw s!"Expected 0...3 for `TaggedText`, got {other}"

/--
A simplified representation of code with semantic tags for rendering.
Unlike `CodeWithInfos`, this only contains the information needed for HTML rendering
(links to declarations, syntax highlighting) and can be serialized to/from the database.
-/
def RenderedCode := Lean.Widget.TaggedText RenderedCode.Tag
deriving Inhabited, BEq, Repr, ToBinary, FromBinary

def RenderedCode.empty : RenderedCode := .append #[]

partial def RenderedCode.textLength : RenderedCode → Nat
  | .text s => s.length
  | .tag _ inner => textLength inner
  | .append xs => xs.foldl (init := 0) fun len x => len + textLength x

open Lean.Widget in
mutual
partial def RenderedCode.pushRight (xs : Array RenderedCode) (x : RenderedCode) : Array RenderedCode :=
  if xs.size = 0 then #[x]
  else xs.modify (xs.size - 1) (·.appendImpl x)

partial def RenderedCode.pushLeft (x : RenderedCode) (xs : Array RenderedCode)  : Array RenderedCode :=
  if xs.size = 0 then #[x]
  else xs.modify 0 x.appendImpl

partial def RenderedCode.appendImpl : RenderedCode → RenderedCode → RenderedCode
  | .text "", x => x
  | x, .text "" => x
  | .append #[], x => x
  | x, .append #[] => x
  | .append xs, .append ys => .append (xs ++ ys)
  | .append xs, y => .append (pushRight xs y)
  | x, .append ys => .append (pushLeft x ys)
  | .text x, .text y => .text (x ++ y)
  | x, y => .append #[x, y]
end

instance : Append RenderedCode := ⟨RenderedCode.appendImpl⟩

/--
In Lean syntax declarations the following pattern is quite common:
```
syntax term " + " term : term
```
that is, we place spaces around the operator in the middle. When the
`InfoTree` framework provides us with information about what source token
corresponds to which identifier it will thus say that `" + "` corresponds to
`HAdd.hadd`. This is however not the way we want this to be linked, in the HTML
only `+` should be linked, taking care of this is what this function is
responsible for.
-/
def splitWhitespaces (s : String) : String × String × String :=
  let length := s.length
  let s := s.trimAsciiStart
  let front := "".pushn ' ' (length - s.positions.length)
  let length := s.positions.length
  let s := s.trimAsciiEnd.copy
  let back := "".pushn ' ' (length - s.length)
  (front, s, back)

private def findWs (s : String.Slice) : s.Pos := go s.startPos
where
  go (i : s.Pos) : s.Pos :=
    if h : i = s.endPos then i
    else if (i.get h).isWhitespace then go (i.next h)
    else i
  termination_by i

-- This doesn't fail on malformed strings because it's better to give the user some feedback than
-- none here. This tokenization is just to highlight keywords correctly.
private def findString (s : String.Slice) : s.Pos := start s.startPos
where
  start (i : s.Pos) : s.Pos :=
    if h : i = s.endPos then i
    else if (i.get h) == '"' then contents (i.next h)
    else i
  contents (i : s.Pos) : s.Pos :=
    if h : i = s.endPos then i
    else if (i.get h) == '\\' then escape (i.next h)
    else if (i.get h) == '"' then i.next h
    else contents (i.next h)
    termination_by i
  escape (i : s.Pos) : s.Pos :=
    if h : i = s.endPos then i
    else contents (i.next h)
    termination_by i

private def findOther (s : String.Slice) : s.Pos := go s.startPos
where
  go (i : s.Pos) : s.Pos :=
    if h : i = s.endPos then i
    else
      let c := i.get h
      if c == '"' then i
      else if c.isWhitespace then i
      else go (i.next h)
  termination_by i

/--
Tokenizes plain text for basic syntax highlighting. This only runs on untagged `.text` nodes from
`CodeWithInfos`: tokens that the pretty printer did not associate with any semantic information.
Lean keywords like `let`, `fun`, `match`, etc. commonly appear as untagged text in pretty-printed
output, so we tag them here so the HTML renderer can style them.

The keyword list is intentionally conservative (common expression-level keywords only) and doesn't
attempt to cover all Lean syntax. For new keywords that occur in terms, this list may need updating,
but missing a keyword only means it won't be highlighted. Linking is unaffected.
-/
private def kws : List String :=
  ["let", "fun", "do", "match", "with", "if", "then", "else", "break", "continue", "for", "in", "mut"]

private def tokenize (txt : String) : RenderedCode := Id.run do
  let mut todo := txt.drop 0
  let mut toks : RenderedCode := .empty
  while !todo.isEmpty do
    if todo.startsWith Char.isWhitespace then
      let i := findWs todo
      let ws := todo.sliceTo i
      todo := todo.sliceFrom i
      toks := toks ++ .text ws.copy
      continue
    else if todo.startsWith '"' then
      let i := findString todo
      let str := todo.sliceTo i
      todo := todo.sliceFrom i
      toks := toks ++ .tag .string (.text str.copy)
    else
      let i := findOther todo
      let tok := todo.sliceTo i
      todo := todo.sliceFrom i
      let tok := tok.copy
      if tok ∈ kws then
        toks := toks ++ .tag .keyword (.text tok)
      else
        toks := toks ++ .text tok
      continue
  return toks

/--
Convert `CodeWithInfos` (from Lean's pretty printer) to `RenderedCode`
by extracting only the information needed for HTML rendering.
-/
partial def renderTagged (doc : CodeWithInfos) : RenderedCode := Id.run do
  match doc with
  | .text txt =>
    return tokenize txt
  | .tag i t =>
    let {ctx := _, info, children := _} := i.info.val
    match info with
    | .ofTermInfo termInfo =>
      match termInfo.expr.consumeMData with
      | .const n _ =>
        -- TODO replicate blacklist logic
        match t with
          | .text t =>
            let (front, t, back) := splitWhitespaces t
            return .append #[.text front, .tag (.const n) (.text t), .text back]
          | _ =>
            .tag (.const n) <$> renderTagged t
      | .sort _u =>
        match t with
        | .text t =>
          let sortPrefix :: rest := t.splitOn " " | unreachable!
          let sortFormer := match sortPrefix with
            | "Type" => some .type
            | "Prop" => some .prop
            | "Sort" => some .sort
            | _ => none
          let mut restStr := String.intercalate " " rest
          if restStr.length != 0 then
            restStr := " " ++ restStr
          return .append #[.tag (.sort sortFormer) (.text sortPrefix), .text restStr]
        | _ =>
          .tag (.sort none) <$> renderTagged t
      | _ => .tag .otherExpr <$> renderTagged t
    | _ => .tag .otherExpr <$> renderTagged t
  | .append xs => xs.mapM renderTagged <&> (·.foldl (init := .empty) (· ++ ·))

/--
A pretty-printer document with semantic tag annotations, for width-flexible rendering.
The `Format.tag n` values in `fmt` and in any type format in `localVars` are all indices into
`tags`. `localVars` contains one entry per distinct local variable: its user-facing name and the
pretty-printed format of its type, or `none` if the type could not be pretty-printed.
-/
structure FormatCode where
  fmt       : Lean.Format
  tags      : Array RenderedCode.Tag
  localVars : Array (Lean.Name × Option Lean.Format) := #[]
deriving ToBinary, FromBinary, Inhabited

/--
Returns `true` if the rendered text length of this `FormatCode` will certainly exceed `limit`.
Short-circuits once the limit is reached, avoiding computing the full length.
-/
def FormatCode.exceedsLimit (doc : FormatCode) (limit : Nat) : Bool :=
  (go doc.fmt limit).isNone
where
  go : Lean.Format → Nat → Option Nat
    | .nil, n
    | .align _, n => some n
    | .line, n => if n == 0 then failure else return (n - 1)
    | .text s, n => if s.length > n then failure else return (n - s.length)
    | .nest _ f, n
    | .group f _, n
    | .tag _ f, n => go f n
    | .append a b, n => go a n >>= go b

private def initTagIndex : Std.HashMap RenderedCode.Tag Nat :=
  Std.HashMap.emptyWithCapacity 3 |>.insert .keyword 0 |>.insert .string 1 |>.insert .otherExpr 2

private structure NormState where
  tags : Array RenderedCode.Tag := #[.keyword, .string, .otherExpr]
  tagIndex : Std.HashMap RenderedCode.Tag Nat := initTagIndex
  localVars : Array (Lean.Name × Option Lean.Format) := #[]
  fvarMap : Std.HashMap FVarId Nat := {}

/--
Finds the canonical representative of an equivalence class of `FVarId`s, induced by `FVarAliasInfo`.
-/
private partial def findCanonical (id : FVarId) : StateM (Std.HashMap FVarId FVarId) FVarId := do
  match (← get).get? id with
  | none => return id
  | some parent =>
    let root ← findCanonical parent
    if root != parent then modify (·.insert id root)
    return root

/-- Traverses the format tree, building a mapping from `FVarId`s to their canonical representative. -/
private def collectAliasBase (infos : PrettyPrinter.InfoPerPos) : StateM (Std.HashMap FVarId FVarId) Unit := do
  for (_, i) in infos do
    if let .ofFVarAliasInfo ai := i then
      let canon ← findCanonical ai.baseId
      modify (·.insert ai.id canon)

private structure NormContext where
  getInfo : Nat → Option Info
  canonicalFVars : Std.HashMap FVarId FVarId
  shallow : Bool

private abbrev NormM := ReaderT NormContext (StateT NormState MetaM)

/-- Emits a tag into the shared array (deduplicating) and wraps it around the given format. -/
private def addTag (tag : RenderedCode.Tag) (f' : Std.Format) : NormM Std.Format := do
  let s ← get
  match s.tagIndex.get? tag with
  | some idx => return .tag idx f'
  | none =>
    let idx := s.tags.size
    modify fun s => { s with tags := s.tags.push tag, tagIndex := s.tagIndex.insert tag idx }
    return .tag idx f'

/-- Tokenizes untagged text nodes in a format, tagging keywords and string literals. -/
private partial def normalizeText (txt : String) : NormM Std.Format := do
  -- Early exit with no allocations if there are no tokens to extract
  unless txt.any (fun c => c == '"' || c.isAlpha) do return .text txt
  let mut todo := txt.drop 0
  let mut result : Std.Format := .nil
  while !todo.isEmpty do
    if todo.startsWith Char.isWhitespace then
      let i := findWs todo
      result := result ++ .text (todo.sliceTo i).copy
      todo := todo.sliceFrom i
    else if todo.startsWith '"' then
      let i := findString todo
      result := result ++ (← addTag .string (.text (todo.sliceTo i).copy))
      todo := todo.sliceFrom i
    else
      let i := findOther todo
      let tok := (todo.sliceTo i).copy
      todo := todo.sliceFrom i
      if tok ∈ kws then
        result := result ++ (← addTag .keyword (.text tok))
      else
        result := result ++ .text tok
  return result

/--
Normalizes a `Std.Format`, discarding information that won't be saved.
-/
private partial def normalizeFormat : (fmt : Std.Format) →  NormM Std.Format
  | .nil => return .nil
  | .line => return .line
  | .align force => return .align force
  | .text s => normalizeText s
  | .nest indent f => return .nest indent (← normalizeFormat f)
  | .append a b => return .append (← normalizeFormat a) (← normalizeFormat b)
  | .group f beh => return .group (← normalizeFormat f) beh
  | .tag n f => do
    let f' ← normalizeFormat f
    match (← read).getInfo n with
    | none | some (.ofFVarAliasInfo _) => return f'
    | some (.ofTermInfo ti) =>
      match ti.expr.consumeMData with
      | .fvar fvarId =>
        if (← read).shallow then
          addTag .otherExpr f'
        else
          let canonId := (← read).canonicalFVars.getD fvarId fvarId
          if let some localVarIdx := (← get).fvarMap.get? canonId then
            addTag (.localVar localVarIdx ti.isBinder) f'
          else
            let some decl := ti.lctx.find? canonId
              | addTag .otherExpr f'
            -- The Lean pretty printer ignores the local instance array, so we can just pass #[]
            -- here. We save/restore the name generator to prevent these inner calls from advancing
            -- the counter and causing names to differ in subsequent prettyPrintTerm calls - this
            -- makes it easier to compare outputs from different doc-gen4 versions during
            -- development.
            let savedNGen ← getNGen
            let typeFmt? ←
              try
                let ⟨typeFmt, typeInfos⟩ ←
                  try withLCtx ti.lctx #[] (PrettyPrinter.ppExprWithInfos decl.type)
                  finally setNGen savedNGen
                let f ← withReader ({ · with getInfo := typeInfos.get?, shallow := true }) do
                  normalizeFormat typeFmt
                pure (some f)
              catch _ => pure none

            let localVarIdx := (← get).localVars.size
            modify fun s => { s with
              localVars := s.localVars.push (decl.userName, typeFmt?)
              fvarMap := s.fvarMap.insert canonId localVarIdx }
            addTag (.localVar localVarIdx ti.isBinder) f'
      | .const n _ => addTag (.const n) f'
      | .sort level =>
        let former := if level.isZero then .prop else if level.isSucc then .type else .sort
        addTag (.sort (some former)) f'
      | _ => addTag .otherExpr f'
    | some _ => addTag .otherExpr f'

/--
Convert a `Lean.Format` and its associated info map to a `FormatCode`.
The `lookup` function resolves tag indices to elaboration info.

Two passes are made:
1. `FVarAliasInfo` is collected in order to discover equivalence classes of local variables
2. Semantic tags are simplified and normalized, de-duplicating them and discarding unneeded information.
-/
def toFormatCode (fmt : Lean.Format) (infos : PrettyPrinter.InfoPerPos) : MetaM FormatCode := do
  let lookup := infos.get?
  let canonMap := (collectAliasBase infos |>.run {}).2
  let ctx : NormContext := { getInfo := lookup, canonicalFVars := canonMap, shallow := false }
  let (fmt', state) ← normalizeFormat fmt |>.run ctx |>.run {}
  return { state with fmt := fmt' }

private partial def renderFormatCode (tags : Array RenderedCode.Tag) :
    TaggedText (Nat × Nat) → RenderedCode
  | .text s => .text s
  | .append xs => xs.foldl (fun acc x => acc ++ renderFormatCode tags x) .empty
  | .tag (n, _) inner =>
    match tags[n]? with
    | none => renderFormatCode tags inner
    | some (.const name) =>
      match inner with
      | .text t =>
        let (front, t', back) := splitWhitespaces t
        .append #[.text front, .tag (.const name) (.text t'), .text back]
      | _ => .tag (.const name) (renderFormatCode tags inner)
    | some (.sort former) =>
      match inner with
      | .text t =>
        match t.splitOn " " with
        | [] => .tag (.sort former) (renderFormatCode tags inner)
        | sortPrefix :: rest =>
          let restStr := if rest.isEmpty then "" else " " ++ String.intercalate " " rest
          .append #[.tag (.sort former) (.text sortPrefix), .text restStr]
      | _ => .tag (.sort former) (renderFormatCode tags inner)
    | some tag => .tag tag (renderFormatCode tags inner)

/--
Render a `FormatCode` to `RenderedCode` at the given width (default: `Std.Format.defWidth`).
-/
def FormatCode.render (doc : FormatCode) (width : Nat := Std.Format.defWidth) : RenderedCode :=
  renderFormatCode doc.tags (TaggedText.prettyTagged doc.fmt (w := width))

end DocGen4
