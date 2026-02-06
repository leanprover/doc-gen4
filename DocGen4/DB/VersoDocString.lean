import DocGen4.Process
import DocGen4.RenderedCode
import SQLite
import DocGen4.Helpers

namespace DocGen4.DB

open Lean
open SQLite.Blob



structure DocstringDataHandler where
  serialize : Serializer Dynamic
  deserialize : Deserializer Dynamic

structure DocstringValues where
  inlines : NameMap DocstringDataHandler := {}
  blocks : NameMap DocstringDataHandler := {}

def toBinaryElabInline (vals : DocstringValues) : Serializer ElabInline
  | { name, val }, b =>
    match vals.inlines.get? name with
    | none => b.push 0 |> ToBinary.serializer name
    | some s => b.push 1 |> ToBinary.serializer name |> s.serialize val

def toBinaryElabBlock (vals : DocstringValues) : Serializer ElabBlock
  | { name, val }, b =>
    match vals.blocks.get? name with
    | none => b.push 0 |> ToBinary.serializer name
    | some s => b.push 1 |> ToBinary.serializer name |> s.serialize val

structure Unknown where
deriving BEq, Hashable, Ord, DecidableEq, Inhabited, TypeName

instance : Subsingleton Unknown where
  allEq := by intros; rfl

def fromBinaryElabInline (vals : DocstringValues) : Deserializer ElabInline := do
  match (← Deserializer.byte) with
  | 0 =>
    let name ← FromBinary.deserializer
    pure { name := `unknown ++ name, val := .mk Unknown.mk }
  | 1 =>
    let name ← FromBinary.deserializer
    match vals.inlines.get? name with
    | none => pure { name := `unknown ++ name, val := .mk Unknown.mk }
    | some d =>
      let val ← d.deserialize
      pure { name, val }
  | other => throw s!"Expected 0 or 1 for `ElabInline`'s tag, got `{other}`"

def fromBinaryElabBlock (vals : DocstringValues) : Deserializer ElabBlock := do
  match (← Deserializer.byte) with
  | 0 =>
    let name ← FromBinary.deserializer
    pure { name := `unknown ++ name, val := .mk Unknown.mk }
  | 1 =>
    let name ← FromBinary.deserializer
    match vals.blocks.get? name with
    | none => pure { name := `unknown ++ name, val := .mk Unknown.mk }
    | some d =>
      let val ← d.deserialize
      pure { name, val }
  | other => throw s!"Expected 0 or 1 for `ElabBlock`'s tag, got `{other}`"

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
