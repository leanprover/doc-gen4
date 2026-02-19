/-
Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: David Thrane Christiansen
-/
import DocGen4.Process
import DocGen4.RenderedCode
import SQLite
import DocGen4.Helpers

/-!
# Verso Docstring Serialization

Verso docstrings (`VersoDocString`) contain a tree of `Doc.Block`/`Doc.Inline` nodes, which can
include *extension points* (`ElabInline`/`ElabBlock`) which are opaque `Dynamic` values identified
by `Name`. Different Lean packages can register their own extension types, so we can't know all
possible types at compile time.

The serialization strategy is:
* For each `ElabInline`/`ElabBlock`, look up a handler by name in `DocstringValues`.
* If a handler exists, serialize the payload with it (tag byte `1`, then name, then length-prefixed
  payload). On deserialization, the same handler reconstructs the value.
* If no handler exists (the extension type is unknown), serialize just the name (tag byte `0`). On
  deserialization, unknown extensions are replaced with an `Unknown` sentinel value and their
  payload bytes are skipped. This means the database remains readable even if extension types are
  added or removed between versions. In Verso docstrings, the content underneath a custom inline or
  block node represents an alternative plain representation.

`builtinDocstringValues` (defined at the bottom of this file) registers the handlers for extension
types that ship with Lean. If a downstream package defines custom Verso extensions, it would need to
provide its own `DocstringValues` with additional handlers. There's presently no API for this, but
the code is designed to allow plugins that provide handlers in the future.
-/

namespace DocGen4.DB

open Lean
open SQLite.Blob

/-- Serializer/deserializer pair for a single Verso extension type. -/
structure DocstringDataHandler where
  serialize : Serializer Dynamic
  deserialize : Deserializer Dynamic

/-- Registry of known Verso extension types, keyed by `Name`. -/
structure DocstringValues where
  handlers : NameMap DocstringDataHandler := {}

private def toBinaryElab (vals : DocstringValues) (name : Name) (val : Dynamic) (b : ByteArray) : ByteArray :=
  match vals.handlers.get? name with
  | none => b.push 0 |> ToBinary.serializer name
  | some s =>
    let payload := s.serialize val .empty
    b.push 1 |> ToBinary.serializer name |> ToBinary.serializer payload.size |> (· ++ payload)

def toBinaryElabInline (vals : DocstringValues) : Serializer ElabInline
  | { name, val }, b => toBinaryElab vals name val b

def toBinaryElabBlock (vals : DocstringValues) : Serializer ElabBlock
  | { name, val }, b => toBinaryElab vals name val b

structure Unknown where
deriving BEq, Hashable, Ord, DecidableEq, Inhabited, TypeName

instance : Subsingleton Unknown where
  allEq := by intros; rfl

private def fromBinaryElab (vals : DocstringValues) (label : String) : Deserializer (Name × Dynamic) := do
  match (← Deserializer.byte) with
  | 0 =>
    let name ← FromBinary.deserializer
    pure (`unknown ++ name, .mk Unknown.mk)
  | 1 =>
    let name ← FromBinary.deserializer
    let len : Nat ← FromBinary.deserializer
    match vals.handlers.get? name with
    | none =>
      let _ ← Deserializer.nbytes len
      pure (`unknown ++ name, .mk Unknown.mk)
    | some d =>
      let val ← d.deserialize
      pure (name, val)
  | other => throw s!"Expected 0 or 1 for `{label}`'s tag, got `{other}`"

def fromBinaryElabInline (vals : DocstringValues) : Deserializer ElabInline := do
  let (name, val) ← fromBinaryElab vals "ElabInline"
  pure { name, val }

def fromBinaryElabBlock (vals : DocstringValues) : Deserializer ElabBlock := do
  let (name, val) ← fromBinaryElab vals "ElabBlock"
  pure { name, val }

partial instance [ToBinary i] : ToBinary (Doc.Inline i) where
  serializer := go
where
  go
    | .text s, b => b.push 0 |> ToBinary.serializer s
    | .linebreak s, b => b.push 1 |> ToBinary.serializer s
    | .emph xs, b =>
      have : ToBinary (Doc.Inline i) := ⟨go⟩
      b.push 2 |> ToBinary.serializer xs
    | .bold xs, b =>
      have : ToBinary (Doc.Inline i) := ⟨go⟩
      b.push 3 |> ToBinary.serializer xs
    | .code s, b =>
      b.push 4 |> ToBinary.serializer s
    | .math .inline s, b => b.push 5 |> ToBinary.serializer s
    | .math .display s, b => b.push 6 |> ToBinary.serializer s
    | .link xs url, b =>
      have : ToBinary (Doc.Inline i) := ⟨go⟩
      b.push 7 |> ToBinary.serializer xs |> ToBinary.serializer url
    | .footnote name xs, b =>
      have : ToBinary (Doc.Inline i) := ⟨go⟩
      b.push 8 |> ToBinary.serializer name |> ToBinary.serializer xs
    | .image alt url, b => b.push 9 |> ToBinary.serializer alt |> ToBinary.serializer url
    | .concat xs, b =>
      have : ToBinary (Doc.Inline i) := ⟨go⟩
      b.push 10 |> ToBinary.serializer xs
    | .other container content, b =>
      have : ToBinary (Doc.Inline i) := ⟨go⟩
      b.push 11 |> ToBinary.serializer container |> ToBinary.serializer content

partial instance [FromBinary i] : FromBinary (Doc.Inline i) where
  deserializer := go
where go := do
  have : FromBinary (Doc.Inline i) := ⟨go⟩
  match (← .byte) with
  | 0 => .text <$> FromBinary.deserializer
  | 1 => .linebreak <$> FromBinary.deserializer
  | 2 => .emph <$> FromBinary.deserializer
  | 3 => .bold <$> FromBinary.deserializer
  | 4 => .code <$> FromBinary.deserializer
  | 5 => .math .inline <$> FromBinary.deserializer
  | 6 => .math .display <$> FromBinary.deserializer
  | 7 => .link <$> FromBinary.deserializer <*> FromBinary.deserializer
  | 8 => .footnote <$> FromBinary.deserializer <*> FromBinary.deserializer
  | 9 => .image <$> FromBinary.deserializer <*> FromBinary.deserializer
  | 10 => .concat <$> FromBinary.deserializer
  | 11 => .other <$> FromBinary.deserializer <*> FromBinary.deserializer
  | other => throw s!"Expected a tag for `Doc.Inline` in 0...12, got {other}"


partial instance [ToBinary i] [ToBinary b] : ToBinary (Doc.Block i b) where
  serializer := go
where
  go
    | .para xs, bs => bs.push 0 |> ToBinary.serializer xs
    | .code s, bs => bs.push 1 |> ToBinary.serializer s
    | .concat xs, bs =>
      have : ToBinary (Doc.Block i b) := ⟨go⟩
      bs.push 2 |> ToBinary.serializer xs
    | .ul xs, bs =>
      have : ToBinary (Doc.Block i b) := ⟨go⟩
      bs.push 3 |> ToBinary.serializer (xs.map (·.contents))
    | .ol n xs, bs =>
      have : ToBinary (Doc.Block i b) := ⟨go⟩
      bs.push 4 |> ToBinary.serializer n |> ToBinary.serializer (xs.map (·.contents))
    | .dl xs, bs =>
      have : ToBinary (Doc.Block i b) := ⟨go⟩
      bs.push 5 |> ToBinary.serializer (xs.map (fun i => (i.term, i.desc)))
    | .blockquote xs, bs =>
      have : ToBinary (Doc.Block i b) := ⟨go⟩
      bs.push 6 |> ToBinary.serializer xs
    | .other container content, bs =>
      have : ToBinary (Doc.Block i b) := ⟨go⟩
      bs.push 7 |> ToBinary.serializer container |> ToBinary.serializer content


partial instance [FromBinary i] [FromBinary b] : FromBinary (Doc.Block i b) where
  deserializer := go
where go := do
  have : FromBinary (Doc.Block i b) := ⟨go⟩
  match (← .byte) with
  | 0 => .para <$> FromBinary.deserializer
  | 1 => .code <$> FromBinary.deserializer
  | 2 => .concat <$> FromBinary.deserializer
  | 3 =>
    let xss : Array (Array (Doc.Block i b)) ← FromBinary.deserializer
    return .ul <| xss.map (⟨·⟩)
  | 4 =>
    let n ← FromBinary.deserializer
    let xss : Array (Array (Doc.Block i b)) ← FromBinary.deserializer
    return .ol n <| xss.map (⟨·⟩)
  | 5 =>
    let items : Array (_ × _) ← FromBinary.deserializer
    return .dl <| items.map (fun x => Doc.DescItem.mk x.1 x.2)
  | 6 => .blockquote <$> FromBinary.deserializer
  | 7 => .other <$> FromBinary.deserializer <*> FromBinary.deserializer
  | other => throw s!"Expected a tag for `Doc.Block` in 0...8, got {other}"

partial instance [ToBinary i] [ToBinary b] [ToBinary p] : ToBinary (Doc.Part i p b) where
  serializer := go
where
  go
    | .mk title titleString metadata content subParts, bs =>
      have : ToBinary (Doc.Part i p b) := ⟨go⟩
      bs
        |> ToBinary.serializer title
        |> ToBinary.serializer titleString
        |> ToBinary.serializer metadata
        |> ToBinary.serializer content
        |> ToBinary.serializer subParts

partial instance [FromBinary i] [FromBinary b] [FromBinary p] : FromBinary (Doc.Part i p b) where
  deserializer := go
where
  go := do
    have : FromBinary (Doc.Part i p b) := ⟨go⟩
    .mk
      <$> FromBinary.deserializer
      <*> FromBinary.deserializer
      <*> FromBinary.deserializer
      <*> FromBinary.deserializer
      <*> FromBinary.deserializer

def versoDocStringToBinary (values : DocstringValues) : ToBinary VersoDocString where
  serializer
    | {text, subsections}, b =>
      -- TODO customizable handling of Verso docstring extension data
      have : ToBinary ElabInline := ⟨toBinaryElabInline values⟩
      have : ToBinary ElabBlock := ⟨toBinaryElabBlock values⟩
      b |> ToBinary.serializer text |> ToBinary.serializer subsections

def versoDocStringFromBinary (values : DocstringValues) : FromBinary VersoDocString where
  deserializer := do
    -- TODO customizable handling of Verso docstring extension data
    have : FromBinary ElabInline := ⟨fromBinaryElabInline values⟩
    have : FromBinary ElabBlock := ⟨fromBinaryElabBlock values⟩
    .mk <$> FromBinary.deserializer <*> FromBinary.deserializer

def versoDocStringQueryParam (values : DocstringValues) : SQLite.QueryParam VersoDocString :=
  have := versoDocStringToBinary values
  .asBlob

/-! ## Builtin Data.* handlers -/

section BuiltinHandlers
open Doc

private def mkHandler (α : Type) [TypeName α] [ToBinary α] [FromBinary α] : Name × DocstringDataHandler :=
  (TypeName.typeName α,
   { serialize := fun dyn b =>
       match dyn.get? α with
       | some val => ToBinary.serializer val b
       | none => b  -- should not happen if names match
     deserialize := Dynamic.mk <$> (FromBinary.deserializer : Deserializer α) })

-- SourceInfo: original → canonical synthetic (lossy), synthetic and none preserved
private instance : ToBinary SourceInfo where
  serializer
    | .original _leading pos _trailing endPos, b =>
      b.push 0 |> ToBinary.serializer pos.byteIdx |> ToBinary.serializer endPos.byteIdx |> ToBinary.serializer true
    | .synthetic pos endPos canonical, b =>
      b.push 0 |> ToBinary.serializer pos.byteIdx |> ToBinary.serializer endPos.byteIdx |> ToBinary.serializer canonical
    | .none, b => b.push 1
private instance : FromBinary SourceInfo where
  deserializer := do
    match (← Deserializer.byte) with
    | 0 =>
      let pos ← FromBinary.deserializer
      let endPos ← FromBinary.deserializer
      let canonical ← FromBinary.deserializer
      pure (.synthetic ⟨pos⟩ ⟨endPos⟩ canonical)
    | 1 => pure .none
    | other => throw s!"Expected tag 0 or 1 for SourceInfo, got {other}"

private instance : ToBinary Substring.Raw := .via (Substring.Raw.toString)

private instance : FromBinary Substring.Raw :=
  .via fun (s : String) => { str := s, startPos := s.rawStartPos, stopPos := s.rawEndPos : Substring.Raw}

deriving instance ToBinary, FromBinary for Syntax.Preresolved

deriving instance ToBinary, FromBinary for Syntax

deriving instance ToBinary, FromBinary for DataValue

deriving instance ToBinary, FromBinary for KVMap

deriving instance ToBinary, FromBinary for Expr

deriving instance ToBinary, FromBinary for DocHighlight

deriving instance ToBinary, FromBinary for DocCode

deriving instance ToBinary, FromBinary for Data.Const

deriving instance ToBinary, FromBinary for Data.Tactic

deriving instance ToBinary, FromBinary for Data.ConvTactic

deriving instance ToBinary, FromBinary for Data.SyntaxCat

deriving instance ToBinary, FromBinary for Data.ModuleName

deriving instance ToBinary, FromBinary for Data.Option

deriving instance ToBinary, FromBinary for Data.Attributes

deriving instance ToBinary, FromBinary for Data.Attribute

deriving instance ToBinary, FromBinary for Data.Syntax

deriving instance ToBinary, FromBinary for Data.LeanBlock

deriving instance ToBinary, FromBinary for Data.LeanTerm

deriving instance ToBinary, FromBinary for Data.SetOption

def builtinDocstringValues : DocstringValues where
  handlers := Id.run do
    let mut m : NameMap DocstringDataHandler := {}
    for (n, h) in [
      mkHandler Data.Const,
      mkHandler Data.Tactic,
      mkHandler Data.ConvTactic,
      mkHandler Data.Attributes,
      mkHandler Data.Attribute,
      mkHandler Data.Option,
      mkHandler Data.SyntaxCat,
      mkHandler Data.Syntax,
      mkHandler Data.ModuleName,
      mkHandler Data.LeanBlock,
      mkHandler Data.LeanTerm,
      mkHandler Data.SetOption
    ] do
      m := m.insert n h
    m

end BuiltinHandlers
