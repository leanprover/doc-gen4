/-
Copyright (c) 2021 Henrik Böving. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Henrik Böving
-/
import Lean
import SQLite

namespace DocGen4

open Lean Widget Elab
open SQLite.Blob

/--
Used in `RenderedCode.Tag` to track what kind of sort this is.
-/
inductive SortFormer where
  | type | prop | sort
deriving ToJson, FromJson, BEq, Hashable, Repr

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
deriving BEq, Hashable, Repr

instance : ToBinary RenderedCode.Tag where
  serializer
    | .keyword, b => b.push 0
    | .string, b => b.push 1
    | .const n, b => b.push 2 |> ToBinary.serializer n
    | .sort none, b => b.push 3
    | .sort (some .type), b => b.push 4
    | .sort (some .prop), b => b.push 5
    | .sort (some .sort), b => b.push 6
    | .otherExpr, b => b.push 7

instance : FromBinary RenderedCode.Tag where
  deserializer := do
    match (← .byte) with
    | 0 => return .keyword
    | 1 => return .string
    | 2 => .const <$> FromBinary.deserializer
    | 3 => return .sort none
    | 4 => return .sort (some .type)
    | 5 => return .sort (some .prop)
    | 6 => return .sort (some .sort)
    | 7 => return .otherExpr
    | other => throw s!"Expected 0...8 for `Tag`, got {other}"

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

deriving instance Hashable for TaggedText

/--
A simplified representation of code with semantic tags for rendering.
Unlike `CodeWithInfos`, this only contains the information needed for HTML rendering
(links to declarations, syntax highlighting) and can be serialized to/from the database.
-/
def RenderedCode := Lean.Widget.TaggedText RenderedCode.Tag
deriving Inhabited, BEq, Repr, ToBinary, FromBinary, Hashable

def RenderedCode.empty : RenderedCode := .append #[]

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
  let front := "".pushn ' ' (length - s.positions.count)
  let length := s.positions.count
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
where
  kws := ["let", "fun", "do", "match", "with", "if", "then", "else", "break", "continue", "for", "in", "mut"]

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
      match termInfo.expr with
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

end DocGen4
