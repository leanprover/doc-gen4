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
  handlers : NameMap DocstringDataHandler := {}

private def toBinaryElab (vals : DocstringValues) (name : Name) (val : Dynamic) (b : ByteArray) : ByteArray :=
  match vals.handlers.get? name with
  | none => b.push 0 |> ToBinary.serializer name
  | some s => b.push 1 |> ToBinary.serializer name |> s.serialize val

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
    match vals.handlers.get? name with
    | none => pure (`unknown ++ name, .mk Unknown.mk)
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

partial instance : ToBinary Format where
  serializer := go
where go
  | .nil, b => b.push 0
  | .line, b => b.push 1
  | .align force, b => b.push 2 |> ToBinary.serializer force
  | .text s, b => b.push 3 |> ToBinary.serializer s
  | .nest n f, b =>
    have : ToBinary Format := ⟨go⟩
    b.push 4 |> ToBinary.serializer n |> ToBinary.serializer f
  | .append f₁ f₂, b =>
    have : ToBinary Format := ⟨go⟩
    b.push 5 |> ToBinary.serializer f₁ |> ToBinary.serializer f₂
  | .group f behavior, b =>
    have : ToBinary Format := ⟨go⟩
    let behaviorTag : UInt8 := match behavior with | .allOrNone => 0 | .fill => 1
    b.push 6 |> ToBinary.serializer f |> (·.push behaviorTag)
  | .tag n f, b =>
    have : ToBinary Format := ⟨go⟩
    b.push 7 |> ToBinary.serializer n |> ToBinary.serializer f

partial instance : FromBinary Format where
  deserializer := go
where go := do
  have : FromBinary Format := ⟨go⟩
  match (← Deserializer.byte) with
  | 0 => pure .nil
  | 1 => pure .line
  | 2 => .align <$> FromBinary.deserializer
  | 3 => .text <$> FromBinary.deserializer
  | 4 => .nest <$> FromBinary.deserializer <*> FromBinary.deserializer
  | 5 => .append <$> FromBinary.deserializer <*> FromBinary.deserializer
  | 6 =>
    let f ← go
    let behaviorTag ← Deserializer.byte
    let behavior := if behaviorTag == 1 then .fill else .allOrNone
    pure (.group f behavior)
  | 7 => .tag <$> FromBinary.deserializer <*> FromBinary.deserializer
  | other => throw s!"Expected tag 0..7 for Format, got {other}"

private instance : ToBinary FVarId where
  serializer fv b := ToBinary.serializer fv.name b
private instance : FromBinary FVarId where
  deserializer := FVarId.mk <$> FromBinary.deserializer

private instance : ToBinary MVarId where
  serializer mv b := ToBinary.serializer mv.name b
private instance : FromBinary MVarId where
  deserializer := MVarId.mk <$> FromBinary.deserializer

private instance : ToBinary LevelMVarId where
  serializer lmv b := ToBinary.serializer lmv.name b
private instance : FromBinary LevelMVarId where
  deserializer := LevelMVarId.mk <$> FromBinary.deserializer

partial instance : ToBinary Level where
  serializer := go
where go
  | .zero, b => b.push 0
  | .succ l, b =>
    have : ToBinary Level := ⟨go⟩
    b.push 1 |> ToBinary.serializer l
  | .max l₁ l₂, b =>
    have : ToBinary Level := ⟨go⟩
    b.push 2 |> ToBinary.serializer l₁ |> ToBinary.serializer l₂
  | .imax l₁ l₂, b =>
    have : ToBinary Level := ⟨go⟩
    b.push 3 |> ToBinary.serializer l₁ |> ToBinary.serializer l₂
  | .param name, b => b.push 4 |> ToBinary.serializer name
  | .mvar id, b => b.push 5 |> ToBinary.serializer id

partial instance : FromBinary Level where
  deserializer := go
where go := do
  have : FromBinary Level := ⟨go⟩
  match (← Deserializer.byte) with
  | 0 => pure .zero
  | 1 => .succ <$> FromBinary.deserializer
  | 2 => .max <$> FromBinary.deserializer <*> FromBinary.deserializer
  | 3 => .imax <$> FromBinary.deserializer <*> FromBinary.deserializer
  | 4 => .param <$> FromBinary.deserializer
  | 5 => .mvar <$> FromBinary.deserializer
  | other => throw s!"Expected tag 0..5 for Level, got {other}"

private instance : ToBinary Literal where
  serializer
    | .natVal n, b => b.push 0 |> ToBinary.serializer n
    | .strVal s, b => b.push 1 |> ToBinary.serializer s
private instance : FromBinary Literal where
  deserializer := do
    match (← Deserializer.byte) with
    | 0 => .natVal <$> FromBinary.deserializer
    | 1 => .strVal <$> FromBinary.deserializer
    | other => throw s!"Expected tag 0..1 for Literal, got {other}"

private instance : ToBinary BinderInfo where
  serializer
    | .default, b => b.push 0
    | .implicit, b => b.push 1
    | .strictImplicit, b => b.push 2
    | .instImplicit, b => b.push 3
private instance : FromBinary BinderInfo where
  deserializer := do
    match (← Deserializer.byte) with
    | 0 => pure .default
    | 1 => pure .implicit
    | 2 => pure .strictImplicit
    | 3 => pure .instImplicit
    | other => throw s!"Expected tag 0..3 for BinderInfo, got {other}"

-- SourceInfo: original → canonical synthetic (lossy), synthetic and none preserved
private instance : ToBinary SourceInfo where
  serializer
    | .original _leading pos _trailing endPos, b =>
      b.push 0 |> ToBinary.serializer pos.byteIdx |> ToBinary.serializer endPos.byteIdx
    | .synthetic pos endPos canonical, b =>
      b.push 1 |> ToBinary.serializer pos.byteIdx |> ToBinary.serializer endPos.byteIdx |> ToBinary.serializer canonical
    | .none, b => b.push 2
private instance : FromBinary SourceInfo where
  deserializer := do
    match (← Deserializer.byte) with
    | 0 =>
      let pos ← FromBinary.deserializer
      let endPos ← FromBinary.deserializer
      pure (.synthetic ⟨pos⟩ ⟨endPos⟩ true)
    | 1 =>
      let pos ← FromBinary.deserializer
      let endPos ← FromBinary.deserializer
      let canonical ← FromBinary.deserializer
      pure (.synthetic ⟨pos⟩ ⟨endPos⟩ canonical)
    | 2 => pure .none
    | other => throw s!"Expected tag 0..2 for SourceInfo, got {other}"

private instance : ToBinary Substring.Raw where
  serializer ss b := b |> ToBinary.serializer ss.str |> ToBinary.serializer ss.startPos.byteIdx |> ToBinary.serializer ss.stopPos.byteIdx
private instance : FromBinary Substring.Raw where
  deserializer := do
    let str ← FromBinary.deserializer
    let startPos ← FromBinary.deserializer
    let stopPos ← FromBinary.deserializer
    pure ⟨str, ⟨startPos⟩, ⟨stopPos⟩⟩

private instance : ToBinary Syntax.Preresolved where
  serializer
    | .namespace name, b => b.push 0 |> ToBinary.serializer name
    | .decl name fields, b => b.push 1 |> ToBinary.serializer name |> ToBinary.serializer fields
private instance : FromBinary Syntax.Preresolved where
  deserializer := do
    match (← Deserializer.byte) with
    | 0 => .namespace <$> FromBinary.deserializer
    | 1 => .decl <$> FromBinary.deserializer <*> FromBinary.deserializer
    | other => throw s!"Expected tag 0..1 for Syntax.Preresolved, got {other}"

partial instance : ToBinary Syntax where
  serializer := go
where go
  | .missing, b => b.push 0
  | .node info kind args, b =>
    have : ToBinary Syntax := ⟨go⟩
    b.push 1 |> ToBinary.serializer info |> ToBinary.serializer kind |> ToBinary.serializer args
  | .atom info val, b => b.push 2 |> ToBinary.serializer info |> ToBinary.serializer val
  | .ident info rawVal val preresolved, b =>
    b.push 3 |> ToBinary.serializer info |> ToBinary.serializer rawVal |> ToBinary.serializer val |> ToBinary.serializer preresolved

partial instance : FromBinary Syntax where
  deserializer := go
where go := do
  have : FromBinary Syntax := ⟨go⟩
  match (← Deserializer.byte) with
  | 0 => pure .missing
  | 1 => .node <$> FromBinary.deserializer <*> FromBinary.deserializer <*> FromBinary.deserializer
  | 2 => .atom <$> FromBinary.deserializer <*> FromBinary.deserializer
  | 3 => .ident <$> FromBinary.deserializer <*> FromBinary.deserializer <*> FromBinary.deserializer <*> FromBinary.deserializer
  | other => throw s!"Expected tag 0..3 for Syntax, got {other}"

private instance : ToBinary DataValue where
  serializer
    | .ofString s, b => b.push 0 |> ToBinary.serializer s
    | .ofBool v, b => b.push 1 |> ToBinary.serializer v
    | .ofName n, b => b.push 2 |> ToBinary.serializer n
    | .ofNat n, b => b.push 3 |> ToBinary.serializer n
    | .ofInt i, b => b.push 4 |> ToBinary.serializer i
    | .ofSyntax stx, b => b.push 5 |> ToBinary.serializer stx
private instance : FromBinary DataValue where
  deserializer := do
    match (← Deserializer.byte) with
    | 0 => .ofString <$> FromBinary.deserializer
    | 1 => .ofBool <$> FromBinary.deserializer
    | 2 => .ofName <$> FromBinary.deserializer
    | 3 => .ofNat <$> FromBinary.deserializer
    | 4 => .ofInt <$> FromBinary.deserializer
    | 5 => .ofSyntax <$> FromBinary.deserializer
    | other => throw s!"Expected tag 0..5 for DataValue, got {other}"

private instance : ToBinary KVMap where
  serializer kv b := ToBinary.serializer kv.entries b
private instance : FromBinary KVMap where
  deserializer := KVMap.mk <$> FromBinary.deserializer

partial instance : ToBinary Expr where
  serializer := go
where go
  | .bvar n, b => b.push 0 |> ToBinary.serializer n
  | .fvar id, b => b.push 1 |> ToBinary.serializer id
  | .mvar id, b => b.push 2 |> ToBinary.serializer id
  | .sort l, b => b.push 3 |> ToBinary.serializer l
  | .const name ls, b => b.push 4 |> ToBinary.serializer name |> ToBinary.serializer ls
  | .app f a, b =>
    have : ToBinary Expr := ⟨go⟩
    b.push 5 |> ToBinary.serializer f |> ToBinary.serializer a
  | .lam name ty body bi, b =>
    have : ToBinary Expr := ⟨go⟩
    b.push 6 |> ToBinary.serializer name |> ToBinary.serializer ty |> ToBinary.serializer body |> ToBinary.serializer bi
  | .forallE name ty body bi, b =>
    have : ToBinary Expr := ⟨go⟩
    b.push 7 |> ToBinary.serializer name |> ToBinary.serializer ty |> ToBinary.serializer body |> ToBinary.serializer bi
  | .letE name ty val body nonDep, b =>
    have : ToBinary Expr := ⟨go⟩
    b.push 8 |> ToBinary.serializer name |> ToBinary.serializer ty |> ToBinary.serializer val |> ToBinary.serializer body |> ToBinary.serializer nonDep
  | .lit l, b => b.push 9 |> ToBinary.serializer l
  | .mdata md e, b =>
    have : ToBinary Expr := ⟨go⟩
    b.push 10 |> ToBinary.serializer md |> ToBinary.serializer e
  | .proj name idx e, b =>
    have : ToBinary Expr := ⟨go⟩
    b.push 11 |> ToBinary.serializer name |> ToBinary.serializer idx |> ToBinary.serializer e

partial instance : FromBinary Expr where
  deserializer := go
where go := do
  have : FromBinary Expr := ⟨go⟩
  match (← Deserializer.byte) with
  | 0 => .bvar <$> FromBinary.deserializer
  | 1 => .fvar <$> FromBinary.deserializer
  | 2 => .mvar <$> FromBinary.deserializer
  | 3 => .sort <$> FromBinary.deserializer
  | 4 => .const <$> FromBinary.deserializer <*> FromBinary.deserializer
  | 5 => .app <$> FromBinary.deserializer <*> FromBinary.deserializer
  | 6 => .lam <$> FromBinary.deserializer <*> FromBinary.deserializer <*> FromBinary.deserializer <*> FromBinary.deserializer
  | 7 => .forallE <$> FromBinary.deserializer <*> FromBinary.deserializer <*> FromBinary.deserializer <*> FromBinary.deserializer
  | 8 => .letE <$> FromBinary.deserializer <*> FromBinary.deserializer <*> FromBinary.deserializer <*> FromBinary.deserializer <*> FromBinary.deserializer
  | 9 => .lit <$> FromBinary.deserializer
  | 10 => .mdata <$> FromBinary.deserializer <*> FromBinary.deserializer
  | 11 => .proj <$> FromBinary.deserializer <*> FromBinary.deserializer <*> FromBinary.deserializer
  | other => throw s!"Expected tag 0..11 for Expr, got {other}"

private instance : ToBinary LocalDeclKind where
  serializer
    | .default, b => b.push 0
    | .implDetail, b => b.push 1
    | .auxDecl, b => b.push 2
private instance : FromBinary LocalDeclKind where
  deserializer := do
    match (← Deserializer.byte) with
    | 0 => pure .default
    | 1 => pure .implDetail
    | 2 => pure .auxDecl
    | other => throw s!"Expected tag 0..2 for LocalDeclKind, got {other}"

private instance : ToBinary LocalDecl where
  serializer
    | .cdecl idx fvarId userName type bi kind, b =>
      b.push 0 |> ToBinary.serializer idx |> ToBinary.serializer fvarId |> ToBinary.serializer userName
        |> ToBinary.serializer type |> ToBinary.serializer bi |> ToBinary.serializer kind
    | .ldecl idx fvarId userName type val nonDep kind, b =>
      b.push 1 |> ToBinary.serializer idx |> ToBinary.serializer fvarId |> ToBinary.serializer userName
        |> ToBinary.serializer type |> ToBinary.serializer val |> ToBinary.serializer nonDep |> ToBinary.serializer kind
private instance : FromBinary LocalDecl where
  deserializer := do
    match (← Deserializer.byte) with
    | 0 => .cdecl <$> FromBinary.deserializer <*> FromBinary.deserializer <*> FromBinary.deserializer
        <*> FromBinary.deserializer <*> FromBinary.deserializer <*> FromBinary.deserializer
    | 1 => .ldecl <$> FromBinary.deserializer <*> FromBinary.deserializer <*> FromBinary.deserializer
        <*> FromBinary.deserializer <*> FromBinary.deserializer <*> FromBinary.deserializer <*> FromBinary.deserializer
    | other => throw s!"Expected tag 0..1 for LocalDecl, got {other}"

private instance : ToBinary LocalContext where
  serializer lctx b :=
    let decls := lctx.decls.toArray
    let auxDecls := lctx.auxDeclToFullName.toArray
    b |> ToBinary.serializer decls |> ToBinary.serializer auxDecls
private instance : FromBinary LocalContext where
  deserializer := do
    let decls : Array (Option LocalDecl) ← FromBinary.deserializer
    let auxDecls : Array (FVarId × Name) ← FromBinary.deserializer
    let mut lctx := LocalContext.empty
    for d? in decls do
      match d? with
      | some d => lctx := lctx.addDecl d
      | none => lctx := { lctx with decls := lctx.decls.push none }
    for (fv, name) in auxDecls do
      lctx := { lctx with auxDeclToFullName := lctx.auxDeclToFullName.insert fv name }
    pure lctx

private instance : ToBinary DocHighlight where
  serializer
    | .const name sig, b => b.push 0 |> ToBinary.serializer name |> ToBinary.serializer sig
    | .var userName fvarId type, b => b.push 1 |> ToBinary.serializer userName |> ToBinary.serializer fvarId |> ToBinary.serializer type
    | .field name sig, b => b.push 2 |> ToBinary.serializer name |> ToBinary.serializer sig
    | .option name declName, b => b.push 3 |> ToBinary.serializer name |> ToBinary.serializer declName
    | .keyword, b => b.push 4
    | .literal kind type?, b => b.push 5 |> ToBinary.serializer kind |> ToBinary.serializer type?
private instance : FromBinary DocHighlight where
  deserializer := do
    match (← Deserializer.byte) with
    | 0 => .const <$> FromBinary.deserializer <*> FromBinary.deserializer
    | 1 => .var <$> FromBinary.deserializer <*> FromBinary.deserializer <*> FromBinary.deserializer
    | 2 => .field <$> FromBinary.deserializer <*> FromBinary.deserializer
    | 3 => .option <$> FromBinary.deserializer <*> FromBinary.deserializer
    | 4 => pure .keyword
    | 5 => .literal <$> FromBinary.deserializer <*> FromBinary.deserializer
    | other => throw s!"Expected tag 0..5 for DocHighlight, got {other}"

private instance : ToBinary DocCode where
  serializer dc b := ToBinary.serializer dc.code b
private instance : FromBinary DocCode where
  deserializer := DocCode.mk <$> FromBinary.deserializer

-- Simple single-Name types
private instance : ToBinary Data.Const where
  serializer d b := ToBinary.serializer d.name b
private instance : FromBinary Data.Const where
  deserializer := .mk <$> FromBinary.deserializer

private instance : ToBinary Data.Tactic where
  serializer d b := ToBinary.serializer d.name b
private instance : FromBinary Data.Tactic where
  deserializer := .mk <$> FromBinary.deserializer

private instance : ToBinary Data.ConvTactic where
  serializer d b := ToBinary.serializer d.name b
private instance : FromBinary Data.ConvTactic where
  deserializer := .mk <$> FromBinary.deserializer

private instance : ToBinary Data.SyntaxCat where
  serializer d b := ToBinary.serializer d.name b
private instance : FromBinary Data.SyntaxCat where
  deserializer := .mk <$> FromBinary.deserializer

private instance : ToBinary Data.ModuleName where
  serializer d b := ToBinary.serializer d.module b
private instance : FromBinary Data.ModuleName where
  deserializer := .mk <$> FromBinary.deserializer

-- Two-Name type
private instance : ToBinary Data.Option where
  serializer d b := b |> ToBinary.serializer d.name |> ToBinary.serializer d.declName
private instance : FromBinary Data.Option where
  deserializer := .mk <$> FromBinary.deserializer <*> FromBinary.deserializer

-- Syntax types
private instance : ToBinary Data.Attributes where
  serializer d b := ToBinary.serializer d.stx b
private instance : FromBinary Data.Attributes where
  deserializer := .mk <$> FromBinary.deserializer

private instance : ToBinary Data.Attribute where
  serializer d b := ToBinary.serializer d.stx b
private instance : FromBinary Data.Attribute where
  deserializer := .mk <$> FromBinary.deserializer

private instance : ToBinary Data.Syntax where
  serializer d b := b |> ToBinary.serializer d.category |> ToBinary.serializer d.stx
private instance : FromBinary Data.Syntax where
  deserializer := .mk <$> FromBinary.deserializer <*> FromBinary.deserializer

private instance : ToBinary Data.Local where
  serializer d b := b |> ToBinary.serializer d.name |> ToBinary.serializer d.fvarId
    |> ToBinary.serializer d.lctx |> ToBinary.serializer d.type
private instance : FromBinary Data.Local where
  deserializer := .mk <$> FromBinary.deserializer <*> FromBinary.deserializer
    <*> FromBinary.deserializer <*> FromBinary.deserializer

-- DocCode types
private instance : ToBinary Data.LeanBlock where
  serializer d b := ToBinary.serializer d.commands b
private instance : FromBinary Data.LeanBlock where
  deserializer := .mk <$> FromBinary.deserializer

private instance : ToBinary Data.LeanTerm where
  serializer d b := ToBinary.serializer d.term b
private instance : FromBinary Data.LeanTerm where
  deserializer := .mk <$> FromBinary.deserializer

private instance : ToBinary Data.SetOption where
  serializer d b := ToBinary.serializer d.term b
private instance : FromBinary Data.SetOption where
  deserializer := .mk <$> FromBinary.deserializer

def builtinDocstringValues : DocstringValues where
  handlers := Id.run do
    let mut m : NameMap DocstringDataHandler := {}
    for (n, h) in [
      mkHandler Data.Const,
      mkHandler Data.Local,
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
