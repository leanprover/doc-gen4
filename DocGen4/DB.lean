import DocGen4.Process
import SQLite

namespace DocGen4.DB

section
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

instance : ToBinary VersoDocString where
  serializer
    | {text, subsections}, b =>
      -- TODO customizable handling of Verso docstring extension data
      have : ToBinary ElabInline := ⟨toBinaryElabInline {}⟩
      have : ToBinary ElabBlock := ⟨toBinaryElabBlock {}⟩
      b |> ToBinary.serializer text |> ToBinary.serializer subsections

instance : FromBinary VersoDocString where
  deserializer := do
    -- TODO customizable handling of Verso docstring extension data
    have : FromBinary ElabInline := ⟨fromBinaryElabInline {}⟩
    have : FromBinary ElabBlock := ⟨fromBinaryElabBlock {}⟩
    .mk <$> FromBinary.deserializer <*> FromBinary.deserializer

instance : SQLite.QueryParam VersoDocString := .asBlob

end
section
open Lean Widget Elab
open SQLite.Blob

inductive SortFormer where
  | type | prop | sort
deriving ToJson, FromJson, BEq, Hashable, Repr

inductive RenderedCode.Tag where
  | keyword
  | string
  | const (name : Lean.Name)
  | sort (former : Option SortFormer)
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
    | other => throw s!"Expected 0...7 for `Tag`, got {other}"

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

def RenderedCode := Lean.Widget.TaggedText RenderedCode.Tag
deriving Inhabited, BEq, Repr, ToBinary, FromBinary

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
-- TODO dedup with original location
def splitWhitespaces (s : String) : String × String × String :=
  let length := s.length
  let s := s.trimAsciiStart
  let front := "".pushn ' ' (length - s.positions.count)
  let length := s.positions.count
  let s := s.trimAsciiEnd.copy
  let back := "".pushn ' ' (length - s.length)
  (front, s, back)

def findWs (s : String.Slice) : s.Pos := go s.startPos
where
  go (i : s.Pos) : s.Pos :=
    if h : i = s.endPos then i
    else if (i.get h).isWhitespace then go (i.next h)
    else i
  termination_by i

-- This doesn't fail on malformed strings because it's better to give the user some feedback than
-- none here. This tokenization is just to highlight keywords correctly.
def findString (s : String.Slice) : s.Pos := start s.startPos
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

def findOther (s : String.Slice) : s.Pos := go s.startPos
where
  go (i : s.Pos) : s.Pos :=
    if h : i = s.endPos then i
    else
      let c := i.get h
      if c == '"' then i
      else if c.isWhitespace then i
      else go (i.next h)
  termination_by i

def tokenize (txt : String) : RenderedCode := Id.run do
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
  tokenEnder (str : String.Slice) : Bool := str.front?.map Char.isAlphanum |>.getD true
  kws := ["let", "fun", "do", "match", "with", "if", "then", "else", "break", "continue", "for", "in", "mut"]

partial def renderTagged
    (doc : CodeWithInfos) :
    RenderedCode := Id.run do
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
      | _ => renderTagged t
    | _ => renderTagged t
  | .append xs => xs.mapM renderTagged <&> (·.foldl (init := .empty) (· ++ ·))

end

def getDb (dbFile : System.FilePath) : IO SQLite := do
  -- SQLite atomically creates the DB file, and the schema and journal settings here are applied
  -- idempotently. This avoids DB creation race conditions.
  let db ← SQLite.openWith dbFile .readWriteCreate
  db.exec "PRAGMA busy_timeout = 5000"
  db.exec "PRAGMA journal_mode = WAL"
  db.exec "PRAGMA foreign_keys = ON"
  db.transaction (db.exec ddl)
  return db
where
  ddl :=
    r#"
PRAGMA journal_mode = WAL;

-- Modules table
CREATE TABLE IF NOT EXISTS modules (
  name TEXT PRIMARY KEY,
  source_url TEXT
);

-- Direct imports
CREATE TABLE IF NOT EXISTS module_imports (
  importer TEXT NOT NULL,
  imported TEXT NOT NULL,
  PRIMARY KEY (importer, imported),
  FOREIGN KEY (importer) REFERENCES modules(name) ON DELETE CASCADE
  -- There's no
  -- FOREIGN KEY (imported) REFERENCES modules(name)
  -- because docs are built incrementally.
);

-- Index for reverse queries: "what imports this module?"
CREATE INDEX IF NOT EXISTS idx_module_imports_imported ON module_imports(imported);

CREATE TABLE IF NOT EXISTS module_items (
  module_name TEXT NOT NULL,
  position INTEGER NOT NULL,
  item_type TEXT NOT NULL,
  PRIMARY KEY (module_name, position),
  FOREIGN KEY (module_name) REFERENCES modules(name) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS declaration_ranges (
  module_name TEXT NOT NULL,
  position INTEGER NOT NULL,
  start_line INTEGER NOT NULL,
  start_column INTEGER NOT NULL,
  start_utf16 INTEGER NOT NULL,
  end_line INTEGER NOT NULL,
  end_column INTEGER NOT NULL,
  end_utf16 INTEGER NOT NULL,
  PRIMARY KEY (module_name, position),
  FOREIGN KEY (module_name) REFERENCES modules(name) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS markdown_docstrings (
  module_name TEXT NOT NULL,
  position INTEGER NOT NULL,
  text TEXT NOT NULL,
  PRIMARY KEY (module_name, position),
  FOREIGN KEY (module_name) REFERENCES modules(name) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS verso_docstrings (
  module_name TEXT NOT NULL,
  position INTEGER NOT NULL,
  content BLOB NOT NULL,
  PRIMARY KEY (module_name, position),
  FOREIGN KEY (module_name) REFERENCES modules(name) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS name_info (
  module_name TEXT NOT NULL,
  position INTEGER NOT NULL,
  kind TEXT,
  name TEXT NOT NULL,
  type TEXT NOT NULL,
  sorried INTEGER NOT NULL,
  render INTEGER NOT NULL,
  PRIMARY KEY (module_name, position),
  FOREIGN KEY (module_name) REFERENCES modules(name) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS axioms (
  module_name TEXT NOT NULL,
  position INTEGER NOT NULL,
  is_unsafe INTEGER NOT NULL,
  PRIMARY KEY (module_name, position),
  FOREIGN KEY (module_name, position) REFERENCES name_info(module_name, position) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS constructors (
  module_name TEXT NOT NULL,
  position INTEGER NOT NULL,
  type_position INTEGER NOT NULL,
  PRIMARY KEY (module_name, position),
  FOREIGN KEY (module_name, position) REFERENCES name_info(module_name, position) ON DELETE CASCADE
  FOREIGN KEY (module_name, type_position) REFERENCES name_info(module_name, position) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS inductives (
  module_name TEXT NOT NULL,
  position INTEGER NOT NULL,
  is_unsafe INTEGER NOT NULL,
  PRIMARY KEY (module_name, position),
  FOREIGN KEY (module_name, position) REFERENCES name_info(module_name, position) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS class_inductives (
  module_name TEXT NOT NULL,
  position INTEGER NOT NULL,
  is_unsafe INTEGER NOT NULL,
  PRIMARY KEY (module_name, position),
  FOREIGN KEY (module_name, position) REFERENCES name_info(module_name, position) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS opaques (
  module_name TEXT NOT NULL,
  position INTEGER NOT NULL,
  safety TEXT NOT NULL,
  PRIMARY KEY (module_name, position),
  FOREIGN KEY (module_name, position) REFERENCES name_info(module_name, position) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS definitions (
  module_name TEXT NOT NULL,
  position INTEGER NOT NULL,
  is_unsafe INTEGER NOT NULL,
  hints TEXT NOT NULL,
  is_noncomputable INTEGER NOT NULL,
  PRIMARY KEY (module_name, position),
  FOREIGN KEY (module_name, position) REFERENCES name_info(module_name, position) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS definition_equations (
  module_name TEXT NOT NULL,
  position INTEGER NOT NULL,
  code TEXT NOT NULL,
  sequence INTEGER NOT NULL,
  PRIMARY KEY (module_name, position, sequence),
  FOREIGN KEY (module_name, position) REFERENCES name_info(module_name, position) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS instances (
  module_name TEXT NOT NULL,
  position INTEGER NOT NULL,
  class_name TEXT NOT NULL,
  PRIMARY KEY (module_name, position),
  FOREIGN KEY (module_name, position) REFERENCES name_info(module_name, position) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS instance_args (
  module_name TEXT NOT NULL,
  position INTEGER NOT NULL,
  sequence INTEGER NOT NULL,
  type_name TEXT NOT NULL,
  PRIMARY KEY (module_name, position, sequence),
  FOREIGN KEY (module_name, position) REFERENCES instances(module_name, position) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS structures (
  module_name TEXT NOT NULL,
  position INTEGER NOT NULL,
  is_class INTEGER NOT NULL,
  PRIMARY KEY (module_name, position),
  FOREIGN KEY (module_name, position) REFERENCES name_info(module_name, position) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS structure_parents (
  module_name TEXT NOT NULL,
  position INTEGER NOT NULL,
  sequence INTEGER NOT NULL,
  projection_fn TEXT NOT NULL,
  type TEXT NOT NULL,
  PRIMARY KEY (module_name, position, sequence),
  FOREIGN KEY (module_name, position) REFERENCES structures(module_name, position) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS structure_constructors (
  module_name TEXT NOT NULL,
  position INTEGER NOT NULL, -- The structure's position
  ctor_position INTEGER NOT NULL,
  name TEXT NOT NULL,
  type BLOB NOT NULL,
  PRIMARY KEY (module_name, position),
  FOREIGN KEY (module_name, position) REFERENCES name_info(module_name, position) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS structure_fields (
  module_name TEXT NOT NULL,
  position INTEGER NOT NULL,
  name TEXT NOT NULL,
  type TEXT NOT NULL,
  render INTEGER NOT NULL,
  sequence INTEGER NOT NULL,
  is_direct INTEGER NOT NULL,
  PRIMARY KEY (module_name, position, sequence),
  FOREIGN KEY (module_name, position) REFERENCES name_info(module_name, position) ON DELETE CASCADE
);
"#

def withTableName (tableName : String) (act : IO α) : IO α :=
  try
    act
  catch
    | e => throw <| .userError s!"Exception while modifying `{tableName}`: {e.toString}"

structure DB where
  sqlite : SQLite
  deleteModule (modName : String) : IO Unit
  saveModule (modName : String) (sourceUrl? : Option String) : IO Unit
  saveImport (modName : String) (imported : Lean.Name) : IO Unit
  saveMarkdownDocstring (modName : String) (position : Int64) (text : String) : IO Unit
  saveVersoDocstring (modName : String) (position : Int64) (text : Lean.VersoDocString) : IO Unit
  saveDeclarationRange (modName : String) (position : Int64) (declRange : Lean.DeclarationRange) : IO Unit
  saveInfo (modName : String) (position : Int64) (kind : String) (info : Process.Info) : IO Unit
  saveAxiom (modName : String) (position : Int64) (isUnsafe : Bool) : IO Unit
  saveOpaque (modName : String) (position : Int64) (safety : Lean.DefinitionSafety) : IO Unit
  saveDefinition (modName : String) (position : Int64) (isUnsafe : Bool) (hints : Lean.ReducibilityHints) (isNonComputable : Bool) : IO Unit
  saveDefinitionEquation (modName : String) (position : Int64) (code : Lean.Widget.CodeWithInfos) (sequence : Int64) : IO Unit
  saveInstance (modName : String) (position : Int64) (className : String) : IO Unit
  saveInstanceArg (modName : String) (position : Int64) (sequence : Int64) (typeName : String) : IO Unit
  saveInductive (modName : String) (position : Int64) (isUnsafe : Bool) : IO Unit
  saveConstructor (modName : String) (position : Int64) (typePosition : Int64) : IO Unit
  saveClassInductive (modName : String) (position : Int64) (isUnsafe : Bool) : IO Unit
  saveStructure (modName : String) (position : Int64) (isClass : Bool) : IO Unit
  saveStructureConstructor (modName : String) (position : Int64) (ctorPos : Int64) (info : Process.NameInfo) : IO Unit
  saveStructureParent (modName : String) (position : Int64) (sequence : Int32) (projectionFn : String) (type : Lean.Widget.CodeWithInfos) : IO Unit
  saveStructureField (modName : String) (position : Int64) (sequence : Int64) (name : String) (type : Lean.Widget.CodeWithInfos) (render : Bool) (isDirect : Bool) : IO Unit

def DB.saveDocstring (db : DB) (modName : String) (position : Int64) (text : String ⊕ Lean.VersoDocString) : IO Unit :=
  match text with
  | .inl md => db.saveMarkdownDocstring modName position md
  | .inr v => db.saveVersoDocstring modName position v

instance : Coe DB SQLite where
  coe := DB.sqlite

private def run (stmt : SQLite.Stmt) : IO Unit := do
  stmt.exec
  stmt.reset
  stmt.clearBindings

def _root_.SQLite.Stmt.bind [SQLite.NullableQueryParam α] (stmt : SQLite.Stmt) (index : Int32) (param : α) : IO Unit := do
  SQLite.NullableQueryParam.bind stmt index param

instance : SQLite.QueryParam Lean.DefinitionSafety where
  bind stmt index safety :=
    SQLite.QueryParam.bind stmt index <|
      match safety with
      | .safe => "safe"
      | .unsafe => "unsafe"
      | .partial => "partial"

instance : SQLite.QueryParam Lean.ReducibilityHints where
  bind stmt index
    | .opaque => SQLite.QueryParam.bind stmt index "opaque"
    | .abbrev => SQLite.QueryParam.bind stmt index "abbrev"
    | .regular i => SQLite.QueryParam.bind stmt index i.toNat.toInt64

open SQLite.Blob in
instance : SQLite.QueryParam Lean.Widget.CodeWithInfos where
  bind stmt index code := Id.run do
    let str := ToBinary.serializer (renderTagged code) .empty
    SQLite.QueryParam.bind stmt index str

def ensureDb (dbFile : System.FilePath) : IO DB := do
  let sqlite ← getDb dbFile
  let deleteModuleStmt ← sqlite.prepare "DELETE FROM modules WHERE name = ?"
  let deleteModule modName := withTableName "modules" do
    deleteModuleStmt.bind 1 modName
    run deleteModuleStmt
  let saveModuleStmt ← sqlite.prepare "INSERT INTO modules (name, source_url) VALUES (?, ?)"
  let saveModule modName sourceUrl? := withTableName "modules" do
    saveModuleStmt.bind 1 modName
    saveModuleStmt.bind 2 sourceUrl?
    run saveModuleStmt
  -- This is INSERT OR IGNORE because the module system often results in multiple imports of the same module (e.g. as meta)
  let saveImportStmt ← sqlite.prepare "INSERT OR IGNORE INTO module_imports (importer, imported) VALUES (?, ?)"
  let saveImport modName imported := withTableName "module_imports" do
    saveImportStmt.bind 1 modName
    saveImportStmt.bind 2 imported.toString
    run saveImportStmt
  let saveMarkdownDocstringStmt ← sqlite.prepare "INSERT INTO markdown_docstrings (module_name, position, text) VALUES (?, ?, ?)"
  let saveMarkdownDocstring modName position text := withTableName "markdown_docstrings" do
    saveMarkdownDocstringStmt.bind 1 modName
    saveMarkdownDocstringStmt.bind 2 position
    saveMarkdownDocstringStmt.bind 3 text
    run saveMarkdownDocstringStmt
  let saveVersoDocstringStmt ← sqlite.prepare "INSERT INTO verso_docstrings (module_name, position, content) VALUES (?, ?, ?)"
  let saveVersoDocstring modName position text := withTableName "verso_docstrings" do
    saveVersoDocstringStmt.bind 1 modName
    saveVersoDocstringStmt.bind 2 position
    saveVersoDocstringStmt.bind 3 text
    run saveVersoDocstringStmt
  let saveDeclarationRangeStmt ←
    sqlite.prepare
      "INSERT INTO declaration_ranges (module_name, position, start_line, start_column, start_utf16, end_line, end_column, end_utf16) VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
  let saveDeclarationRange modName position (declRange : Lean.DeclarationRange) := withTableName "declaration_ranges" do
    saveDeclarationRangeStmt.bind 1 modName
    saveDeclarationRangeStmt.bind 2 position
    saveDeclarationRangeStmt.bind 3 declRange.pos.line
    saveDeclarationRangeStmt.bind 4 declRange.pos.column
    saveDeclarationRangeStmt.bind 5 declRange.charUtf16
    saveDeclarationRangeStmt.bind 6 declRange.endPos.line
    saveDeclarationRangeStmt.bind 7 declRange.endPos.column
    saveDeclarationRangeStmt.bind 8 declRange.endCharUtf16
    run saveDeclarationRangeStmt
  let saveInfoStmt ← sqlite.prepare "INSERT INTO name_info (module_name, position, kind, name, type, sorried, render) VALUES (?, ?, ?, ?, ?, ?, ?)"
  let saveInfo modName position kind (info : Process.Info) := withTableName "name_info" do
    saveInfoStmt.bind 1 modName
    saveInfoStmt.bind 2 position
    saveInfoStmt.bind 3 kind
    saveInfoStmt.bind 4 info.name.toString
    saveInfoStmt.bind 5 info.type
    saveInfoStmt.bind 6 info.sorried
    saveInfoStmt.bind 7 info.render
    run saveInfoStmt
    match info.doc with
    | some (.inl md) => saveMarkdownDocstring modName position md
    | some (.inr v) => saveVersoDocstring modName position v
    | none => pure ()
  let saveAxiomStmt ← sqlite.prepare "INSERT INTO axioms (module_name, position, is_unsafe) VALUES (?, ?, ?)"
  let saveAxiom modName position isUnsafe := withTableName "axioms" do
    saveAxiomStmt.bind 1 modName
    saveAxiomStmt.bind 2 position
    saveAxiomStmt.bind 3 isUnsafe
    run saveAxiomStmt
  let saveOpaqueStmt ← sqlite.prepare "INSERT INTO opaques (module_name, position, safety) VALUES (?, ?, ?)"
  let saveOpaque modName position safety := withTableName "opaques" do
    saveOpaqueStmt.bind 1 modName
    saveOpaqueStmt.bind 2 position
    saveOpaqueStmt.bind 3 safety
    run saveOpaqueStmt
  let saveDefinitionStmt ← sqlite.prepare "INSERT INTO definitions (module_name, position, is_unsafe, hints, is_noncomputable) VALUES (?, ?, ?, ?, ?)"
  let saveDefinition modName position isUnsafe hints isNonComputable := withTableName "definitions" do
    saveDefinitionStmt.bind 1 modName
    saveDefinitionStmt.bind 2 position
    saveDefinitionStmt.bind 3 isUnsafe
    saveDefinitionStmt.bind 4 hints
    saveDefinitionStmt.bind 5 isNonComputable
    run saveDefinitionStmt
  let saveDefinitionEquationStmt ← sqlite.prepare "INSERT INTO definition_equations (module_name, position, code, sequence) VALUES (?, ?, ?, ?)"
  let saveDefinitionEquation modName position code sequence := withTableName "definition_equations" do
    saveDefinitionEquationStmt.bind 1 modName
    saveDefinitionEquationStmt.bind 2 position
    saveDefinitionEquationStmt.bind 3 code
    saveDefinitionEquationStmt.bind 4 sequence
    run saveDefinitionEquationStmt
  let saveInstanceStmt ← sqlite.prepare "INSERT INTO instances (module_name, position, class_name) VALUES (?, ?, ?)"
  let saveInstance modName position className := withTableName "instances" do
    saveInstanceStmt.bind 1 modName
    saveInstanceStmt.bind 2 position
    saveInstanceStmt.bind 3 className
    run saveInstanceStmt
  let saveInstanceArgStmt ← sqlite.prepare "INSERT INTO instance_args (module_name, position, sequence, type_name) VALUES (?, ?, ?, ?)"
  let saveInstanceArg modName position sequence typeName := withTableName "instance_args" do
    saveInstanceArgStmt.bind 1 modName
    saveInstanceArgStmt.bind 2 position
    saveInstanceArgStmt.bind 3 sequence
    saveInstanceArgStmt.bind 4 typeName
    run saveInstanceArgStmt
  let saveInductiveStmt ← sqlite.prepare "INSERT INTO inductives (module_name, position, is_unsafe) VALUES (?, ?, ?)"
  let saveInductive modName position isUnsafe := withTableName "inductives" do
    saveInductiveStmt.bind 1 modName
    saveInductiveStmt.bind 2 position
    saveInductiveStmt.bind 3 isUnsafe
    run saveInductiveStmt
  let saveConstructorStmt ← sqlite.prepare "INSERT INTO constructors (module_name, position, type_position) VALUES (?, ?, ?)"
  let saveConstructor modName position typePosition := withTableName "constructors" do
    saveConstructorStmt.bind 1 modName
    saveConstructorStmt.bind 2 position
    saveConstructorStmt.bind 3 typePosition
    run saveConstructorStmt
  let saveClassInductiveStmt ← sqlite.prepare "INSERT INTO class_inductives (module_name, position, is_unsafe) VALUES (?, ?, ?)"
  let saveClassInductive modName position isUnsafe := withTableName "class_inductives" do
    saveClassInductiveStmt.bind 1 modName
    saveClassInductiveStmt.bind 2 position
    saveClassInductiveStmt.bind 3 isUnsafe
    run saveClassInductiveStmt
  let saveStructureStmt ← sqlite.prepare "INSERT INTO structures (module_name, position, is_class) VALUES (?, ?, ?)"
  let saveStructure modName position isClass := withTableName "structures" do
    saveStructureStmt.bind 1 modName
    saveStructureStmt.bind 2 position
    saveStructureStmt.bind 3 isClass
    run saveStructureStmt
  let saveStructureConstructorStmt ← sqlite.prepare "INSERT INTO structure_constructors (module_name, position, ctor_position, name, type) VALUES (?, ?, ?, ?, ?)"
  let saveStructureConstructor modName position ctorPos info := withTableName "structure_constructors" do
    saveStructureConstructorStmt.bind 1 modName
    saveStructureConstructorStmt.bind 2 position
    saveStructureConstructorStmt.bind 3 ctorPos
    saveStructureConstructorStmt.bind 4 info.name.toString
    saveStructureConstructorStmt.bind 5 info.type
    run saveStructureConstructorStmt
    match info.doc with
    | some (.inl md) => saveMarkdownDocstring modName ctorPos md
    | some (.inr v) => saveVersoDocstring modName ctorPos v
    | none => pure ()

  let saveStructureParentStmt ← sqlite.prepare "INSERT INTO structure_parents (module_name, position, sequence, projection_fn, type) VALUES (?, ?, ?, ?, ?)"
  let saveStructureParent modName position sequence projectionFn type := withTableName "structure_parents" do
    saveStructureParentStmt.bind 1 modName
    saveStructureParentStmt.bind 2 position
    saveStructureParentStmt.bind 3 sequence
    saveStructureParentStmt.bind 4 projectionFn
    saveStructureParentStmt.bind 5 type
    run saveStructureParentStmt
  let saveStructureFieldStmt ← sqlite.prepare "INSERT INTO structure_fields (module_name, position, sequence, name, type, render, is_direct) VALUES (?, ?, ?, ?, ?, ?, ?)"
  let saveStructureField modName position sequence name type render isDirect := withTableName "structure_fields" do
    saveStructureFieldStmt.bind 1 modName
    saveStructureFieldStmt.bind 2 position
    saveStructureFieldStmt.bind 3 sequence
    saveStructureFieldStmt.bind 4 name
    saveStructureFieldStmt.bind 5 type
    saveStructureFieldStmt.bind 6 render
    saveStructureFieldStmt.bind 7 isDirect
    run saveStructureFieldStmt
  pure {
    sqlite,
    deleteModule,
    saveModule,
    saveImport,
    saveMarkdownDocstring,
    saveVersoDocstring,
    saveDeclarationRange,
    saveInfo,
    saveAxiom,
    saveOpaque,
    saveDefinition,
    saveDefinitionEquation,
    saveInstance,
    saveInstanceArg,
    saveInductive,
    saveConstructor,
    saveClassInductive,
    saveStructure,
    saveStructureConstructor,
    saveStructureParent,
    saveStructureField
  }

end DB

open DB

def updateModuleDb (doc : Process.AnalyzerResult) (buildDir : System.FilePath) (dbFile : String) (sourceUrl? : Option String) : IO Unit := do
  let dbFile := buildDir / dbFile
  let db ← ensureDb dbFile
  let ms1 ← IO.monoMsNow
  db.sqlite.transaction do
    for (modName, modInfo) in doc.moduleInfo do
      let modName := modName.toString
      db.deleteModule modName
      db.saveModule modName sourceUrl?
      for imported in modInfo.imports do
        db.saveImport modName imported
      let mut i : Int64 := 0
      for mem in modInfo.members do
        let pos := i
        i := i + 1
        match mem with
        | .modDoc doc =>
          db.saveDeclarationRange modName pos doc.declarationRange
          db.saveMarkdownDocstring modName pos doc.doc
        | .docInfo info =>
          let baseInfo := info.toInfo
          db.saveInfo modName pos (infoKind info) baseInfo
          db.saveDeclarationRange modName pos baseInfo.declarationRange
          match info with
          | .axiomInfo info =>
            db.saveAxiom modName pos info.isUnsafe
          | .theoremInfo _info => -- No extra info here
            pure ()
          | .opaqueInfo info =>
            db.saveOpaque modName pos info.definitionSafety
          | .definitionInfo info =>
            db.saveDefinition modName pos info.isUnsafe info.hints info.isNonComputable
            if let some eqns := info.equations then
              for h : j in 0...eqns.size do
                db.saveDefinitionEquation modName pos eqns[j] j.toInt64
          | .instanceInfo info =>
            db.saveInstance modName pos info.className.toString
            for h : j in 0...info.typeNames.size do
              db.saveInstanceArg modName pos j.toInt64 info.typeNames[j].toString
          | .inductiveInfo info =>
            db.saveInductive modName pos info.isUnsafe
            for ctor in info.ctors do
              let cpos := i
              i := i + 1
              db.saveInfo modName cpos "constructor" ctor
              db.saveDeclarationRange modName cpos ctor.declarationRange
              db.saveConstructor modName cpos pos
          | .structureInfo info =>
            i := (← (saveStructureInfo false info db modName pos).run i).2
          | .classInfo info =>
            i := (← (saveStructureInfo true info db modName pos).run i).2
          | .classInductiveInfo info =>
            db.saveClassInductive modName pos info.isUnsafe
            for ctor in info.ctors do
              let cpos := i
              i := i + 1
              db.saveInfo modName cpos "constructor" ctor
              db.saveDeclarationRange modName cpos ctor.declarationRange
              db.saveConstructor modName cpos pos
          | .ctorInfo info =>
            -- Here we do nothing because they were inserted along with the inductive
            pure ()
  let ms2 ← IO.monoMsNow
  (← IO.FS.Handle.mk "db-timing" .append).write <| s!"{doc.moduleInfo.keysArray}\t{ms2 - ms1}ms\n".toUTF8
  pure ()

where
  saveStructureInfo (isClass : Bool) (info : Process.StructureInfo) (db : DB) (modName : String) (pos : Int64) : StateT Int64 IO Unit := do
    db.saveStructure modName pos isClass
    modify (· + 1)
    db.saveStructureConstructor modName pos (← get) info.ctor
    let mut seq : Int32 := 0
    for parent in info.parents do
      db.saveStructureParent modName pos seq parent.projFn.toString parent.type
      seq := seq + 1
    modify (· + 1)
    for field in info.fieldInfo do
      let fpos ← get
      modify (· + 1)
      db.saveStructureField modName pos fpos field.name.toString field.type field.render field.isDirect
      if let some doc := field.doc then db.saveDocstring modName fpos doc

  infoKind : Process.DocInfo → String
    | .axiomInfo _ => "axiom"
    | .theoremInfo info => "theorem"
    | .opaqueInfo info => "opaque"
    | .definitionInfo info => "definition"
    | .instanceInfo info => "instance"
    | .inductiveInfo info => "inductive"
    | .structureInfo info => "structure"
    | .classInfo info => "class"
    | .classInductiveInfo info => "class inductive"
    | .ctorInfo info => "constructor"
