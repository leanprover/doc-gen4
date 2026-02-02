import DocGen4.Process
import DocGen4.RenderedCode
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

def getDb (dbFile : System.FilePath) : IO SQLite := do
  -- SQLite atomically creates the DB file, and the schema and journal settings here are applied
  -- idempotently. This avoids DB creation race conditions.
  let db ← SQLite.openWith dbFile .readWriteCreate
  db.exec "PRAGMA busy_timeout = 60000"  -- 60 seconds for parallel builds
  db.exec "PRAGMA journal_mode = WAL"
  db.exec "PRAGMA foreign_keys = ON"
  try
    db.transaction (db.exec ddl)
  catch
  | e =>
    throw <| .userError s!"Exception while creating schema: {e}"
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

-- Internal names (like recursors) that aren't rendered but should link to a rendered declaration
CREATE TABLE IF NOT EXISTS internal_names (
  name TEXT NOT NULL PRIMARY KEY,
  target_module TEXT NOT NULL,
  target_position INTEGER NOT NULL,
  FOREIGN KEY (target_module, target_position) REFERENCES name_info(module_name, position) ON DELETE CASCADE
);

-- Index for CASCADE deletes: when name_info rows are deleted, find matching internal_names
CREATE INDEX IF NOT EXISTS idx_internal_names_target ON internal_names(target_module, target_position);

CREATE TABLE IF NOT EXISTS constructors (
  module_name TEXT NOT NULL,
  position INTEGER NOT NULL,
  type_position INTEGER NOT NULL,
  PRIMARY KEY (module_name, position),
  FOREIGN KEY (module_name, position) REFERENCES name_info(module_name, position) ON DELETE CASCADE
  FOREIGN KEY (module_name, type_position) REFERENCES name_info(module_name, position) ON DELETE CASCADE
);

-- Index for CASCADE deletes on the second FK (type_position)
CREATE INDEX IF NOT EXISTS idx_constructors_type_pos ON constructors(module_name, type_position);

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
  has_equations INTEGER NOT NULL,
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

-- Trigger to ensure has_equations is true when equations are inserted
CREATE TRIGGER IF NOT EXISTS ensure_has_equations_on_insert
AFTER INSERT ON definition_equations
BEGIN
  UPDATE definitions
  SET has_equations = 1
  WHERE module_name = NEW.module_name AND position = NEW.position AND has_equations = 0;
END;

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
  sequence INTEGER NOT NULL,
  proj_name TEXT NOT NULL,
  type BLOB NOT NULL,
  is_direct INTEGER NOT NULL,
  PRIMARY KEY (module_name, position, sequence),
  FOREIGN KEY (module_name, position) REFERENCES name_info(module_name, position) ON DELETE CASCADE
  -- Note: No FK on proj_name because the projection function may be in a different module
  -- (for inherited fields) that hasn't been processed yet. The JOIN at load time handles this.
);

CREATE TABLE IF NOT EXISTS structure_field_args (
  module_name TEXT NOT NULL,
  position INTEGER NOT NULL,
  field_sequence INTEGER NOT NULL,
  arg_sequence INTEGER NOT NULL,
  binder BLOB NOT NULL,
  is_implicit INTEGER NOT NULL,
  PRIMARY KEY (module_name, position, field_sequence, arg_sequence),
  FOREIGN KEY (module_name, position, field_sequence) REFERENCES structure_fields(module_name, position, sequence) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS declaration_args (
  module_name TEXT NOT NULL,
  position INTEGER NOT NULL,
  sequence INTEGER NOT NULL,
  binder BLOB NOT NULL,
  is_implicit INTEGER NOT NULL,
  PRIMARY KEY (module_name, position, sequence),
  FOREIGN KEY (module_name, position) REFERENCES name_info(module_name, position) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS declaration_attrs (
  module_name TEXT NOT NULL,
  position INTEGER NOT NULL,
  sequence INTEGER NOT NULL,
  attr TEXT NOT NULL,
  PRIMARY KEY (module_name, position, sequence),
  FOREIGN KEY (module_name, position) REFERENCES name_info(module_name, position) ON DELETE CASCADE
);
"#

def withDbContext (context : String) (act : IO α) : IO α := do
  let ms ← IO.monoMsNow
  try
    act
  catch
    | e =>
      let ms' ← IO.monoMsNow
      throw <| .userError s!"Exception in `{context}` after {ms' - ms}ms: {e.toString}"

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
  saveDefinition (modName : String) (position : Int64) (isUnsafe : Bool) (hints : Lean.ReducibilityHints) (isNonComputable : Bool) (hasEquations : Bool) : IO Unit
  saveDefinitionEquation (modName : String) (position : Int64) (code : RenderedCode) (sequence : Int64) : IO Unit
  saveInstance (modName : String) (position : Int64) (className : String) : IO Unit
  saveInstanceArg (modName : String) (position : Int64) (sequence : Int64) (typeName : String) : IO Unit
  saveInductive (modName : String) (position : Int64) (isUnsafe : Bool) : IO Unit
  saveConstructor (modName : String) (position : Int64) (typePosition : Int64) : IO Unit
  saveClassInductive (modName : String) (position : Int64) (isUnsafe : Bool) : IO Unit
  saveStructure (modName : String) (position : Int64) (isClass : Bool) : IO Unit
  saveStructureConstructor (modName : String) (position : Int64) (ctorPos : Int64) (info : Process.NameInfo) : IO Unit
  /-- Save minimal info to name_info for name lookups (not for rendering) -/
  saveNameOnly (modName : String) (position : Int64) (kind : String) (name : Lean.Name) (type : RenderedCode) (declRange : Lean.DeclarationRange) : IO Unit
  saveStructureParent (modName : String) (position : Int64) (sequence : Int32) (projectionFn : String) (type : RenderedCode) : IO Unit
  saveStructureField (modName : String) (position : Int64) (sequence : Int64) (projName : String) (type : RenderedCode) (isDirect : Bool) : IO Unit
  saveStructureFieldArg (modName : String) (position : Int64) (fieldSeq : Int64) (argSeq : Int64) (binder : RenderedCode) (isImplicit : Bool) : IO Unit
  saveArg (modName : String) (position : Int64) (sequence : Int64) (binder : RenderedCode) (isImplicit : Bool) : IO Unit
  saveAttr (modName : String) (position : Int64) (sequence : Int64) (attr : String) : IO Unit
  /-- Save an internal name (like a recursor) that should link to its target declaration -/
  saveInternalName (name : Lean.Name) (targetModule : String) (targetPosition : Int64) : IO Unit

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
instance : SQLite.QueryParam RenderedCode where
  bind stmt index code := Id.run do
    let str := ToBinary.serializer code .empty
    SQLite.QueryParam.bind stmt index str

def ensureDb (dbFile : System.FilePath) : IO DB := do
  let sqlite ← getDb dbFile
  let deleteModuleStmt ← sqlite.prepare "DELETE FROM modules WHERE name = ?"
  let deleteModule modName := withDbContext "write:delete:modules" do
    deleteModuleStmt.bind 1 modName
    run deleteModuleStmt
  let saveModuleStmt ← sqlite.prepare "INSERT INTO modules (name, source_url) VALUES (?, ?)"
  let saveModule modName sourceUrl? := withDbContext "write:insert:modules" do
    saveModuleStmt.bind 1 modName
    saveModuleStmt.bind 2 sourceUrl?
    run saveModuleStmt
  -- This is INSERT OR IGNORE because the module system often results in multiple imports of the same module (e.g. as meta)
  let saveImportStmt ← sqlite.prepare "INSERT OR IGNORE INTO module_imports (importer, imported) VALUES (?, ?)"
  let saveImport modName imported := withDbContext "write:insert:module_imports" do
    saveImportStmt.bind 1 modName
    saveImportStmt.bind 2 imported.toString
    run saveImportStmt
  let saveMarkdownDocstringStmt ← sqlite.prepare "INSERT INTO markdown_docstrings (module_name, position, text) VALUES (?, ?, ?)"
  let saveMarkdownDocstring modName position text := withDbContext "write:insert:markdown_docstrings" do
    saveMarkdownDocstringStmt.bind 1 modName
    saveMarkdownDocstringStmt.bind 2 position
    saveMarkdownDocstringStmt.bind 3 text
    run saveMarkdownDocstringStmt
  let saveVersoDocstringStmt ← sqlite.prepare "INSERT INTO verso_docstrings (module_name, position, content) VALUES (?, ?, ?)"
  let saveVersoDocstring modName position text := withDbContext "write:insert:verso_docstrings" do
    saveVersoDocstringStmt.bind 1 modName
    saveVersoDocstringStmt.bind 2 position
    saveVersoDocstringStmt.bind 3 text
    run saveVersoDocstringStmt
  let saveDeclarationRangeStmt ←
    sqlite.prepare
      "INSERT INTO declaration_ranges (module_name, position, start_line, start_column, start_utf16, end_line, end_column, end_utf16) VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
  let saveDeclarationRange modName position (declRange : Lean.DeclarationRange) := withDbContext "write:insert:declaration_ranges" do
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
  let saveArgStmt' ← sqlite.prepare "INSERT INTO declaration_args (module_name, position, sequence, binder, is_implicit) VALUES (?, ?, ?, ?, ?)"
  let saveAttrStmt' ← sqlite.prepare "INSERT INTO declaration_attrs (module_name, position, sequence, attr) VALUES (?, ?, ?, ?)"
  let saveInfo modName position kind (info : Process.Info) := withDbContext "write:insert:name_info" do
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
    -- Save args
    for h : j in 0...info.args.size do
      let arg := info.args[j]
      withDbContext "write:insert:declaration_args:info" do
        saveArgStmt'.bind 1 modName
        saveArgStmt'.bind 2 position
        saveArgStmt'.bind 3 j.toInt64
        saveArgStmt'.bind 4 arg.binder
        saveArgStmt'.bind 5 arg.implicit
        run saveArgStmt'
    -- Save attrs
    for h : j in 0...info.attrs.size do
      let attr := info.attrs[j]
      withDbContext "write:insert:declaration_attrs:info" do
        saveAttrStmt'.bind 1 modName
        saveAttrStmt'.bind 2 position
        saveAttrStmt'.bind 3 j.toInt64
        saveAttrStmt'.bind 4 attr
        run saveAttrStmt'
  let saveAxiomStmt ← sqlite.prepare "INSERT INTO axioms (module_name, position, is_unsafe) VALUES (?, ?, ?)"
  let saveAxiom modName position isUnsafe := withDbContext "write:insert:axioms" do
    saveAxiomStmt.bind 1 modName
    saveAxiomStmt.bind 2 position
    saveAxiomStmt.bind 3 isUnsafe
    run saveAxiomStmt
  let saveOpaqueStmt ← sqlite.prepare "INSERT INTO opaques (module_name, position, safety) VALUES (?, ?, ?)"
  let saveOpaque modName position safety := withDbContext "write:insert:opaques" do
    saveOpaqueStmt.bind 1 modName
    saveOpaqueStmt.bind 2 position
    saveOpaqueStmt.bind 3 safety
    run saveOpaqueStmt
  let saveDefinitionStmt ← sqlite.prepare "INSERT INTO definitions (module_name, position, is_unsafe, hints, is_noncomputable, has_equations) VALUES (?, ?, ?, ?, ?, ?)"
  let saveDefinition modName position isUnsafe hints isNonComputable hasEquations := withDbContext "write:insert:definitions" do
    saveDefinitionStmt.bind 1 modName
    saveDefinitionStmt.bind 2 position
    saveDefinitionStmt.bind 3 isUnsafe
    saveDefinitionStmt.bind 4 hints
    saveDefinitionStmt.bind 5 isNonComputable
    saveDefinitionStmt.bind 6 hasEquations
    run saveDefinitionStmt
  let saveDefinitionEquationStmt ← sqlite.prepare "INSERT INTO definition_equations (module_name, position, code, sequence) VALUES (?, ?, ?, ?)"
  let saveDefinitionEquation modName position (code : RenderedCode) sequence := withDbContext "write:insert:definition_equations" do
    saveDefinitionEquationStmt.bind 1 modName
    saveDefinitionEquationStmt.bind 2 position
    saveDefinitionEquationStmt.bind 3 code
    saveDefinitionEquationStmt.bind 4 sequence
    run saveDefinitionEquationStmt
  let saveInstanceStmt ← sqlite.prepare "INSERT INTO instances (module_name, position, class_name) VALUES (?, ?, ?)"
  let saveInstance modName position className := withDbContext "write:insert:instances" do
    saveInstanceStmt.bind 1 modName
    saveInstanceStmt.bind 2 position
    saveInstanceStmt.bind 3 className
    run saveInstanceStmt
  let saveInstanceArgStmt ← sqlite.prepare "INSERT INTO instance_args (module_name, position, sequence, type_name) VALUES (?, ?, ?, ?)"
  let saveInstanceArg modName position sequence typeName := withDbContext "write:insert:instance_args" do
    saveInstanceArgStmt.bind 1 modName
    saveInstanceArgStmt.bind 2 position
    saveInstanceArgStmt.bind 3 sequence
    saveInstanceArgStmt.bind 4 typeName
    run saveInstanceArgStmt
  let saveInductiveStmt ← sqlite.prepare "INSERT INTO inductives (module_name, position, is_unsafe) VALUES (?, ?, ?)"
  let saveInductive modName position isUnsafe := withDbContext "write:insert:inductives" do
    saveInductiveStmt.bind 1 modName
    saveInductiveStmt.bind 2 position
    saveInductiveStmt.bind 3 isUnsafe
    run saveInductiveStmt
  let saveConstructorStmt ← sqlite.prepare "INSERT INTO constructors (module_name, position, type_position) VALUES (?, ?, ?)"
  let saveConstructor modName position typePosition := withDbContext "write:insert:constructors" do
    saveConstructorStmt.bind 1 modName
    saveConstructorStmt.bind 2 position
    saveConstructorStmt.bind 3 typePosition
    run saveConstructorStmt
  let saveClassInductiveStmt ← sqlite.prepare "INSERT INTO class_inductives (module_name, position, is_unsafe) VALUES (?, ?, ?)"
  let saveClassInductive modName position isUnsafe := withDbContext "write:insert:class_inductives" do
    saveClassInductiveStmt.bind 1 modName
    saveClassInductiveStmt.bind 2 position
    saveClassInductiveStmt.bind 3 isUnsafe
    run saveClassInductiveStmt
  let saveStructureStmt ← sqlite.prepare "INSERT INTO structures (module_name, position, is_class) VALUES (?, ?, ?)"
  let saveStructure modName position isClass := withDbContext "write:insert:structures" do
    saveStructureStmt.bind 1 modName
    saveStructureStmt.bind 2 position
    saveStructureStmt.bind 3 isClass
    run saveStructureStmt
  let saveStructureConstructorStmt ← sqlite.prepare "INSERT INTO structure_constructors (module_name, position, ctor_position, name, type) VALUES (?, ?, ?, ?, ?)"
  let saveStructureConstructor modName position ctorPos info := withDbContext "write:insert:structure_constructors" do
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
  let saveStructureParent modName position sequence projectionFn (type : RenderedCode) := withDbContext "write:insert:structure_parents" do
    saveStructureParentStmt.bind 1 modName
    saveStructureParentStmt.bind 2 position
    saveStructureParentStmt.bind 3 sequence
    saveStructureParentStmt.bind 4 projectionFn
    saveStructureParentStmt.bind 5 type
    run saveStructureParentStmt
  -- Store projection function name directly; lookup happens at load time
  let saveStructureFieldStmt ← sqlite.prepare "INSERT INTO structure_fields (module_name, position, sequence, proj_name, type, is_direct) VALUES (?, ?, ?, ?, ?, ?)"
  let saveStructureField modName position sequence projName (type : RenderedCode) isDirect := withDbContext "write:insert:structure_fields" do
    saveStructureFieldStmt.bind 1 modName
    saveStructureFieldStmt.bind 2 position
    saveStructureFieldStmt.bind 3 sequence
    saveStructureFieldStmt.bind 4 projName
    saveStructureFieldStmt.bind 5 type
    saveStructureFieldStmt.bind 6 isDirect
    run saveStructureFieldStmt
  let saveStructureFieldArgStmt ← sqlite.prepare "INSERT INTO structure_field_args (module_name, position, field_sequence, arg_sequence, binder, is_implicit) VALUES (?, ?, ?, ?, ?, ?)"
  let saveStructureFieldArg modName position fieldSeq argSeq (binder : RenderedCode) isImplicit := withDbContext "write:insert:structure_field_args" do
    saveStructureFieldArgStmt.bind 1 modName
    saveStructureFieldArgStmt.bind 2 position
    saveStructureFieldArgStmt.bind 3 fieldSeq
    saveStructureFieldArgStmt.bind 4 argSeq
    saveStructureFieldArgStmt.bind 5 binder
    saveStructureFieldArgStmt.bind 6 isImplicit
    run saveStructureFieldArgStmt
  let saveArgStmt ← sqlite.prepare "INSERT INTO declaration_args (module_name, position, sequence, binder, is_implicit) VALUES (?, ?, ?, ?, ?)"
  let saveArg modName position sequence (binder : RenderedCode) isImplicit := withDbContext "write:insert:declaration_args" do
    saveArgStmt.bind 1 modName
    saveArgStmt.bind 2 position
    saveArgStmt.bind 3 sequence
    saveArgStmt.bind 4 binder
    saveArgStmt.bind 5 isImplicit
    run saveArgStmt
  let saveAttrStmt ← sqlite.prepare "INSERT INTO declaration_attrs (module_name, position, sequence, attr) VALUES (?, ?, ?, ?)"
  let saveAttr modName position sequence attr := withDbContext "write:insert:declaration_attrs" do
    saveAttrStmt.bind 1 modName
    saveAttrStmt.bind 2 position
    saveAttrStmt.bind 3 sequence
    saveAttrStmt.bind 4 attr
    run saveAttrStmt
  -- For saving minimal info to name_info for name lookups only (not rendering)
  let saveNameOnlyStmt ← sqlite.prepare "INSERT INTO name_info (module_name, position, kind, name, type, sorried, render) VALUES (?, ?, ?, ?, ?, 0, 0)"
  let saveNameOnly modName position kind (name : Lean.Name) (type : RenderedCode) (declRange : Lean.DeclarationRange) := withDbContext "write:insert:name_info:nameonly" do
    saveNameOnlyStmt.bind 1 modName
    saveNameOnlyStmt.bind 2 position
    saveNameOnlyStmt.bind 3 kind
    saveNameOnlyStmt.bind 4 name.toString
    saveNameOnlyStmt.bind 5 type
    run saveNameOnlyStmt
    -- Also save declaration range
    saveDeclarationRange modName position declRange
  -- For saving internal names (like recursors) that link to their target declaration
  let saveInternalNameStmt ← sqlite.prepare "INSERT OR IGNORE INTO internal_names (name, target_module, target_position) VALUES (?, ?, ?)"
  let saveInternalName (name : Lean.Name) (targetModule : String) (targetPosition : Int64) := withDbContext "write:insert:internal_names" do
    saveInternalNameStmt.bind 1 name.toString
    saveInternalNameStmt.bind 2 targetModule
    saveInternalNameStmt.bind 3 targetPosition
    run saveInternalNameStmt
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
    saveStructureField,
    saveStructureFieldArg,
    saveArg,
    saveAttr,
    saveNameOnly,
    saveInternalName
  }

end DB

open DB

def updateModuleDb (doc : Process.AnalyzerResult) (buildDir : System.FilePath) (dbFile : String)
    (sourceUrl? : Option String) : IO Unit := do
  let dbFile := buildDir / dbFile
  let db ← ensureDb dbFile
  for (modName, modInfo) in doc.moduleInfo do
    let modNameStr := modName.toString
    -- Each module gets its own transaction to reduce lock contention
    let _ ← withDbContext s!"transaction:immediate:{modNameStr}" <| db.sqlite.transaction (mode := .immediate) do
      -- Collect structure field info to save in second pass (after all declarations are in name_info)
      let mut pendingStructureFields : Array (Int64 × Process.StructureInfo) := #[]
      db.deleteModule modNameStr
      db.saveModule modNameStr sourceUrl?
      for imported in modInfo.imports do
        db.saveImport modNameStr imported
      let mut i : Int64 := 0
      for mem in modInfo.members do
        let pos := i
        i := i + 1
        match mem with
        | .modDoc doc =>
          db.saveDeclarationRange modNameStr pos doc.declarationRange
          db.saveMarkdownDocstring modNameStr pos doc.doc
        | .docInfo info =>
          let baseInfo := info.toInfo
          -- Skip saving ctorInfo here - they're saved along with their parent inductive
          if !info.isCtorInfo then
            db.saveInfo modNameStr pos (infoKind info) baseInfo
            db.saveDeclarationRange modNameStr pos baseInfo.declarationRange
          match info with
          | .axiomInfo info =>
            db.saveAxiom modNameStr pos info.isUnsafe
          | .theoremInfo _info => -- No extra info here
            pure ()
          | .opaqueInfo info =>
            db.saveOpaque modNameStr pos info.definitionSafety
          | .definitionInfo info =>
            db.saveDefinition modNameStr pos info.isUnsafe info.hints info.isNonComputable info.equations.isSome
            if let some eqns := info.equations then
              for h : j in 0...eqns.size do
                db.saveDefinitionEquation modNameStr pos eqns[j] j.toInt64
          | .instanceInfo info =>
            -- Save definition data (InstanceInfo extends DefinitionInfo)
            db.saveDefinition modNameStr pos info.isUnsafe info.hints info.isNonComputable info.equations.isSome
            if let some eqns := info.equations then
              for h : j in 0...eqns.size do
                db.saveDefinitionEquation modNameStr pos eqns[j] j.toInt64
            -- Save instance-specific data
            db.saveInstance modNameStr pos info.className.toString
            for h : j in 0...info.typeNames.size do
              db.saveInstanceArg modNameStr pos j.toInt64 info.typeNames[j].toString
          | .inductiveInfo info =>
            db.saveInductive modNameStr pos info.isUnsafe
            -- Save recursors (main + aux) as internal names linking to this inductive
            saveRecursors doc.name2ModIdx db modNameStr pos info.name
            for ctor in info.ctors do
              let cpos := i
              i := i + 1
              db.saveInfo modNameStr cpos "constructor" ctor
              db.saveDeclarationRange modNameStr cpos ctor.declarationRange
              db.saveConstructor modNameStr cpos pos
          | .structureInfo info =>
            -- First pass: save structure metadata (not fields)
            i := (← (saveStructureMetadata false info db modNameStr pos doc.name2ModIdx).run i).2
            pendingStructureFields := pendingStructureFields.push (pos, info)
          | .classInfo info =>
            -- First pass: save structure metadata (not fields)
            i := (← (saveStructureMetadata true info db modNameStr pos doc.name2ModIdx).run i).2
            pendingStructureFields := pendingStructureFields.push (pos, info)
          | .classInductiveInfo info =>
            db.saveClassInductive modNameStr pos info.isUnsafe
            -- Save recursors (main + aux) as internal names linking to this class inductive
            saveRecursors doc.name2ModIdx db modNameStr pos info.name
            for ctor in info.ctors do
              let cpos := i
              i := i + 1
              db.saveInfo modNameStr cpos "constructor" ctor
              db.saveDeclarationRange modNameStr cpos ctor.declarationRange
              db.saveConstructor modNameStr cpos pos
          | .ctorInfo info =>
            -- Here we do nothing because they were inserted along with the inductive
            pure ()
      -- Second pass: save structure fields (now that all projection functions are in name_info)
      for (pos, info) in pendingStructureFields do
        saveStructureFields info db modNameStr pos
      pure ()
  pure ()

where
  -- Save all recursors (main + aux) for an inductive type
  -- Uses name2ModIdx (from env.const2ModIdx) to check if names exist
  saveRecursors (name2ModIdx : Std.HashMap Lean.Name Lean.ModuleIdx) (db : DB) (modName : String) (pos : Int64) (indName : Lean.Name) : IO Unit := do
    -- Save the main recursor
    db.saveInternalName (Lean.mkRecName indName) modName pos
    -- Save aux recursors if they exist in the environment
    for auxName in [Lean.mkCasesOnName indName, Lean.mkRecOnName indName, Lean.mkBRecOnName indName] do
      if name2ModIdx.contains auxName then
        db.saveInternalName auxName modName pos
    -- Special case: Eq and HEq also have ndrec and ndrecOn (non-dependent versions)
    if indName == `Eq then
      db.saveInternalName `Eq.ndrec modName pos
      db.saveInternalName `Eq.ndrecOn modName pos
    if indName == `HEq then
      db.saveInternalName `HEq.ndrec modName pos
      db.saveInternalName `HEq.ndrecOn modName pos

  -- First pass: save structure metadata (not fields)
  saveStructureMetadata (isClass : Bool) (info : Process.StructureInfo) (db : DB) (modName : String) (pos : Int64) (name2ModIdx : Std.HashMap Lean.Name Lean.ModuleIdx) : StateT Int64 IO Unit := do
    db.saveStructure modName pos isClass
    -- Save recursors for this structure
    saveRecursors name2ModIdx db modName pos info.name
    modify (· + 1)
    let ctorPos := ← get
    db.saveStructureConstructor modName pos ctorPos info.ctor
    -- Also save to name_info for name lookups (constructor is not rendered separately)
    -- Use the structure's declaration range for the constructor
    db.saveNameOnly modName ctorPos "constructor" info.ctor.name info.ctor.type info.declarationRange
    let mut seq : Int32 := 0
    for parent in info.parents do
      db.saveStructureParent modName pos seq parent.projFn.toString parent.type
      seq := seq + 1
    modify (· + 1)
    -- Skip field count to maintain position consistency
    -- Note: We don't save projection functions here because:
    -- - Direct fields (isDirect = true): projection function is saved when the definition is processed
    -- - Inherited fields (isDirect = false): projection function is in a different module and will
    --   be saved when that module is processed. The lookup at load time handles missing projections.
    modify (· + info.fieldInfo.size.toInt64)

  -- Second pass: save structure fields (after all projection functions are in name_info)
  -- The INSERT...SELECT in saveStructureField handles the projection function lookup
  saveStructureFields (info : Process.StructureInfo) (db : DB) (modName : String) (pos : Int64) : IO Unit := do
    let mut fieldSeq : Int64 := 0
    for field in info.fieldInfo do
      let projName := field.name.toString
      db.saveStructureField modName pos fieldSeq projName field.type field.isDirect
      -- Save projection function name to internal_names so it can be linked
      -- (projection functions like _private.*.field link to their parent structure)
      db.saveInternalName field.name modName pos
      -- Save field args to structure_field_args
      for h : j in 0...field.args.size do
        let arg := field.args[j]
        db.saveStructureFieldArg modName pos fieldSeq j.toInt64 arg.binder arg.implicit
      fieldSeq := fieldSeq + 1

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

/-! ## DB Reading -/

section Reading
open Lean SQLite.Blob

/-- Open a database for reading. -/
def openDbForReading (dbFile : System.FilePath) : IO SQLite := do
  let db ← SQLite.openWith dbFile .readonly
  db.exec "PRAGMA busy_timeout = 50000"
  return db

/-- Read RenderedCode from a blob. -/
def readRenderedCode (blob : ByteArray) : IO RenderedCode := do
  match fromBinary blob with
  | .ok code => return code
  | .error e => throw <| IO.userError s!"Failed to deserialize RenderedCode: {e}"

/-- Read VersoDocString from a blob. -/
def readVersoDocString (blob : ByteArray) : IO VersoDocString := do
  match fromBinary blob with
  | .ok doc => return doc
  | .error e => throw <| IO.userError s!"Failed to deserialize VersoDocString: {e}"

/-- Get all module names from the database. -/
def getModuleNames (db : SQLite) : IO (Array Name) := withDbContext "read:modules:names" do
  let stmt ← db.prepare "SELECT name FROM modules ORDER BY name"
  let mut names := #[]
  while (← stmt.step) do
    let name := (← stmt.columnText 0).toName
    names := names.push name
  return names

/-- Get all module source URLs from the database. -/
def getModuleSourceUrls (db : SQLite) : IO (Std.HashMap Name String) := withDbContext "read:modules:source_urls" do
  let stmt ← db.prepare "SELECT name, source_url FROM modules WHERE source_url IS NOT NULL"
  let mut urls : Std.HashMap Name String := {}
  while (← stmt.step) do
    let name := (← stmt.columnText 0).toName
    let url ← stmt.columnText 1
    urls := urls.insert name url
  return urls

/-- Get all module imports from the database. -/
def getModuleImports (db : SQLite) (moduleName : Name) : IO (Array Name) := withDbContext "read:module_imports" do
  let stmt ← db.prepare "SELECT imported FROM module_imports WHERE importer = ?"
  stmt.bind 1 moduleName.toString
  let mut imports := #[]
  while (← stmt.step) do
    let name := (← stmt.columnText 0).toName
    imports := imports.push name
  return imports

/-- Build the name-to-module index needed for cross-linking. -/
def buildName2ModIdx (db : SQLite) (moduleNames : Array Name) : IO (Std.HashMap Name ModuleIdx) := do
  -- First build a map from module name string to index
  let modNameToIdx : Std.HashMap Name ModuleIdx :=
    moduleNames.foldl (init := {}) fun acc modName =>
      acc.insert modName acc.size
  -- Now query all names and their modules
  let stmt ← db.prepare "SELECT name, module_name FROM name_info"
  let mut result : Std.HashMap Name ModuleIdx := {}
  while (← stmt.step) do
    let name := (← stmt.columnText 0).toName
    let moduleName := (← stmt.columnText 1).toName
    if let some idx := modNameToIdx[moduleName]? then
      result := result.insert name idx
  -- Also add internal names (like recursors) that map to their target's module.
  -- Only add if not already in result (name_info entries take precedence).
  let internalStmt ← db.prepare "SELECT name, target_module FROM internal_names"
  while (← internalStmt.step) do
    let name := (← internalStmt.columnText 0).toName
    if !result.contains name then
      let targetModule := (← internalStmt.columnText 1).toName
      if let some idx := modNameToIdx[targetModule]? then
        result := result.insert name idx
  return result

/-- Load declaration arguments from the database. -/
def loadArgs (db : SQLite) (moduleName : String) (position : Int64) : IO (Array Process.Arg) := withDbContext "read:declaration_args" do
  let stmt ← db.prepare "SELECT binder, is_implicit FROM declaration_args WHERE module_name = ? AND position = ? ORDER BY sequence"
  stmt.bind 1 moduleName
  stmt.bind 2 position
  let mut args := #[]
  while (← stmt.step) do
    let binderBlob ← stmt.columnBlob 0
    let binder ← readRenderedCode binderBlob
    let isImplicit := (← stmt.columnInt64 1) != 0
    args := args.push { binder, implicit := isImplicit }
  return args

/-- Load declaration attributes from the database. -/
def loadAttrs (db : SQLite) (moduleName : String) (position : Int64) : IO (Array String) := withDbContext "read:declaration_attrs" do
  let stmt ← db.prepare "SELECT attr FROM declaration_attrs WHERE module_name = ? AND position = ? ORDER BY sequence"
  stmt.bind 1 moduleName
  stmt.bind 2 position
  let mut attrs := #[]
  while (← stmt.step) do
    let attr ← stmt.columnText 0
    attrs := attrs.push attr
  return attrs

/-- Load a docstring from the database. -/
def loadDocstring (db : SQLite) (moduleName : String) (position : Int64) : IO (Option (String ⊕ VersoDocString)) := withDbContext "read:docstrings" do
  -- Try markdown first
  let mdStmt ← db.prepare "SELECT text FROM markdown_docstrings WHERE module_name = ? AND position = ?"
  mdStmt.bind 1 moduleName
  mdStmt.bind 2 position
  if (← mdStmt.step) then
    let text ← mdStmt.columnText 0
    return some (.inl text)
  -- Try verso
  let versoStmt ← db.prepare "SELECT content FROM verso_docstrings WHERE module_name = ? AND position = ?"
  versoStmt.bind 1 moduleName
  versoStmt.bind 2 position
  if (← versoStmt.step) then
    let blob ← versoStmt.columnBlob 0
    let doc ← readVersoDocString blob
    return some (.inr doc)
  return none

/-- Load a declaration range from the database. -/
def loadDeclarationRange (db : SQLite) (moduleName : String) (position : Int64) : IO (Option DeclarationRange) := withDbContext "read:declaration_ranges" do
  let stmt ← db.prepare "SELECT start_line, start_column, start_utf16, end_line, end_column, end_utf16 FROM declaration_ranges WHERE module_name = ? AND position = ?"
  stmt.bind 1 moduleName
  stmt.bind 2 position
  if (← stmt.step) then
    let startLine := (← stmt.columnInt64 0).toNatClampNeg
    let startCol := (← stmt.columnInt64 1).toNatClampNeg
    let startUtf16 := (← stmt.columnInt64 2).toNatClampNeg
    let endLine := (← stmt.columnInt64 3).toNatClampNeg
    let endCol := (← stmt.columnInt64 4).toNatClampNeg
    let endUtf16 := (← stmt.columnInt64 5).toNatClampNeg
    return some {
      pos := ⟨startLine, startCol⟩
      charUtf16 := startUtf16
      endPos := ⟨endLine, endCol⟩
      endCharUtf16 := endUtf16
    }
  return none

/-- Load base Info from the database row. -/
def loadInfo (db : SQLite) (moduleName : String) (position : Int64) (name : Name) (typeBlob : ByteArray) (sorried : Bool) (render : Bool) : IO Process.Info := do
  let type ← readRenderedCode typeBlob
  let doc ← loadDocstring db moduleName position
  let args ← loadArgs db moduleName position
  let attrs ← loadAttrs db moduleName position
  let some declRange ← loadDeclarationRange db moduleName position
    | throw <| IO.userError s!"Missing declaration range for {name}"
  return {
    name
    type
    doc
    args
    declarationRange := declRange
    attrs
    sorried
    render
  }

/-- Load definition equations from the database.
    Takes hasEquations flag to distinguish `none` from `some #[]`. -/
def loadEquations (db : SQLite) (moduleName : String) (position : Int64) (hasEquations : Bool) : IO (Option (Array RenderedCode)) := withDbContext "read:definition_equations" do
  if !hasEquations then return none
  let stmt ← db.prepare "SELECT code FROM definition_equations WHERE module_name = ? AND position = ? ORDER BY sequence"
  stmt.bind 1 moduleName
  stmt.bind 2 position
  let mut eqns := #[]
  while (← stmt.step) do
    let blob ← stmt.columnBlob 0
    let code ← readRenderedCode blob
    eqns := eqns.push code
  return some eqns

/-- Load instance type names from the database. -/
def loadInstanceArgs (db : SQLite) (moduleName : String) (position : Int64) : IO (Array Name) := do
  let stmt ← db.prepare "SELECT type_name FROM instance_args WHERE module_name = ? AND position = ? ORDER BY sequence"
  stmt.bind 1 moduleName
  stmt.bind 2 position
  let mut typeNames := #[]
  while (← stmt.step) do
    let typeName := (← stmt.columnText 0).toName
    typeNames := typeNames.push typeName
  return typeNames

/-- Load structure parents from the database. -/
def loadStructureParents (db : SQLite) (moduleName : String) (position : Int64) : IO (Array Process.StructureParentInfo) := do
  let stmt ← db.prepare "SELECT projection_fn, type FROM structure_parents WHERE module_name = ? AND position = ? ORDER BY sequence"
  stmt.bind 1 moduleName
  stmt.bind 2 position
  let mut parents := #[]
  while (← stmt.step) do
    let projFn := (← stmt.columnText 0).toName
    let typeBlob ← stmt.columnBlob 1
    let type ← readRenderedCode typeBlob
    parents := parents.push { projFn, type }
  return parents

/-- Load structure field args from the database. -/
def loadStructureFieldArgs (db : SQLite) (moduleName : String) (position : Int64) (fieldSeq : Int64) : IO (Array Process.Arg) := do
  let stmt ← db.prepare "SELECT binder, is_implicit FROM structure_field_args WHERE module_name = ? AND position = ? AND field_sequence = ? ORDER BY arg_sequence"
  stmt.bind 1 moduleName
  stmt.bind 2 position
  stmt.bind 3 fieldSeq
  let mut args := #[]
  while (← stmt.step) do
    let binderBlob ← stmt.columnBlob 0
    let binder ← readRenderedCode binderBlob
    let isImplicit := (← stmt.columnInt64 1) != 0
    args := args.push { binder, implicit := isImplicit }
  return args

/-- Load structure fields from the database. -/
def loadStructureFields (db : SQLite) (moduleName : String) (position : Int64) : IO (Array Process.FieldInfo) := do
  -- Get structure fields and look up projection function info by name
  let stmt ← db.prepare "SELECT sequence, proj_name, type, is_direct FROM structure_fields WHERE module_name = ? AND position = ? ORDER BY sequence"
  stmt.bind 1 moduleName
  stmt.bind 2 position
  let mut fields := #[]
  while (← stmt.step) do
    let fieldSeq := ← stmt.columnInt64 0
    let name := (← stmt.columnText 1).toName
    let typeBlob ← stmt.columnBlob 2
    let type ← readRenderedCode typeBlob
    let isDirect := (← stmt.columnInt64 3) != 0
    -- Look up projection function by name to get its module and position
    let projStmt ← db.prepare "SELECT module_name, position FROM name_info WHERE name = ? LIMIT 1"
    projStmt.bind 1 name.toString
    let (doc, attrs, declRange, render) ← if (← projStmt.step) then do
      let projModName ← projStmt.columnText 0
      let projPos ← projStmt.columnInt64 1
      -- Load projection function's docstring, attrs, and declaration range
      let doc ← loadDocstring db projModName projPos
      let attrs ← loadAttrs db projModName projPos
      let declRange ← loadDeclarationRange db projModName projPos
      -- Get render flag from projection function's name_info
      let render ← do
        let renderStmt ← db.prepare "SELECT render FROM name_info WHERE module_name = ? AND position = ?"
        renderStmt.bind 1 projModName
        renderStmt.bind 2 projPos
        if (← renderStmt.step) then
          pure ((← renderStmt.columnInt64 0) != 0)
        else
          pure true
      pure (doc, attrs, declRange, render)
    else
      -- Projection function not found in name_info - use defaults
      -- This can happen for inherited fields whose parent module wasn't processed
      pure (none, #[], none, true)
    -- Load field-specific args from structure_field_args
    let args ← loadStructureFieldArgs db moduleName position fieldSeq
    fields := fields.push {
      name
      type
      doc
      args
      declarationRange := declRange.getD default
      attrs
      render
      isDirect
    }
  return fields

/-- Load structure constructor from the database. -/
def loadStructureConstructor (db : SQLite) (moduleName : String) (position : Int64) : IO (Option Process.NameInfo) := do
  let stmt ← db.prepare "SELECT name, type, ctor_position FROM structure_constructors WHERE module_name = ? AND position = ?"
  stmt.bind 1 moduleName
  stmt.bind 2 position
  if (← stmt.step) then
    let name := (← stmt.columnText 0).toName
    let typeBlob ← stmt.columnBlob 1
    let ctorPos ← stmt.columnInt64 2
    let type ← readRenderedCode typeBlob
    let doc ← loadDocstring db moduleName ctorPos
    return some { name, type, doc }
  return none

/-- Load constructors for an inductive type. -/
def loadConstructors (db : SQLite) (moduleName : String) (position : Int64) : IO (List Process.ConstructorInfo) := do
  let stmt ← db.prepare "SELECT c.position FROM constructors c WHERE c.module_name = ? AND c.type_position = ? ORDER BY c.position"
  stmt.bind 1 moduleName
  stmt.bind 2 position
  let mut ctors := []
  while (← stmt.step) do
    let ctorPos ← stmt.columnInt64 0
    -- Now load the full info for this constructor
    let infoStmt ← db.prepare "SELECT name, type, sorried, render FROM name_info WHERE module_name = ? AND position = ?"
    infoStmt.bind 1 moduleName
    infoStmt.bind 2 ctorPos
    if (← infoStmt.step) then
      let name := (← infoStmt.columnText 0).toName
      let typeBlob ← infoStmt.columnBlob 1
      let sorried := (← infoStmt.columnInt64 2) != 0
      let render := (← infoStmt.columnInt64 3) != 0
      let info ← loadInfo db moduleName ctorPos name typeBlob sorried render
      ctors := ctors ++ [info]
  return ctors

/-- Load a DocInfo from the database based on its kind. -/
def loadDocInfo (db : SQLite) (moduleName : String) (position : Int64) (kind : String)
    (name : Name) (typeBlob : ByteArray) (sorried : Bool) (render : Bool) : IO (Option Process.DocInfo) := do
  let info ← loadInfo db moduleName position name typeBlob sorried render
  match kind with
  | "axiom" =>
    let stmt ← db.prepare "SELECT is_unsafe FROM axioms WHERE module_name = ? AND position = ?"
    stmt.bind 1 moduleName
    stmt.bind 2 position
    if (← stmt.step) then
      let isUnsafe := (← stmt.columnInt64 0) != 0
      return some <| .axiomInfo { toInfo := info, isUnsafe }
    return none
  | "theorem" =>
    return some <| .theoremInfo { toInfo := info }
  | "opaque" =>
    let stmt ← db.prepare "SELECT safety FROM opaques WHERE module_name = ? AND position = ?"
    stmt.bind 1 moduleName
    stmt.bind 2 position
    if (← stmt.step) then
      let safetyStr ← stmt.columnText 0
      let safety := match safetyStr with
        | "unsafe" => .unsafe
        | "partial" => .partial
        | _ => .safe
      return some <| .opaqueInfo { toInfo := info, definitionSafety := safety }
    return none
  | "definition" =>
    let stmt ← db.prepare "SELECT is_unsafe, hints, is_noncomputable, has_equations FROM definitions WHERE module_name = ? AND position = ?"
    stmt.bind 1 moduleName
    stmt.bind 2 position
    if (← stmt.step) then
      let isUnsafe := (← stmt.columnInt64 0) != 0
      let hintsStr ← stmt.columnText 1
      let isNonComputable := (← stmt.columnInt64 2) != 0
      let hasEquations := (← stmt.columnInt64 3) != 0
      let hints : ReducibilityHints := match hintsStr with
        | "opaque" => .opaque
        | "abbrev" => .abbrev
        | s => .regular (s.toNat?.getD 0 |>.toUInt32)
      let equations ← loadEquations db moduleName position hasEquations
      return some <| .definitionInfo { toInfo := info, isUnsafe, hints, equations, isNonComputable }
    return none
  | "instance" =>
    let instStmt ← db.prepare "SELECT class_name FROM instances WHERE module_name = ? AND position = ?"
    instStmt.bind 1 moduleName
    instStmt.bind 2 position
    if (← instStmt.step) then
      let className := (← instStmt.columnText 0).toName
      let defStmt ← db.prepare "SELECT is_unsafe, hints, is_noncomputable, has_equations FROM definitions WHERE module_name = ? AND position = ?"
      defStmt.bind 1 moduleName
      defStmt.bind 2 position
      if (← defStmt.step) then
        let isUnsafe := (← defStmt.columnInt64 0) != 0
        let hintsStr ← defStmt.columnText 1
        let isNonComputable := (← defStmt.columnInt64 2) != 0
        let hasEquations := (← defStmt.columnInt64 3) != 0
        let hints : ReducibilityHints := match hintsStr with
          | "opaque" => .opaque
          | "abbrev" => .abbrev
          | s => .regular (s.toNat?.getD 0 |>.toUInt32)
        let equations ← loadEquations db moduleName position hasEquations
        let typeNames ← loadInstanceArgs db moduleName position
        return some <| .instanceInfo { toInfo := info, isUnsafe, hints, equations, isNonComputable, className, typeNames }
    return none
  | "inductive" =>
    let stmt ← db.prepare "SELECT is_unsafe FROM inductives WHERE module_name = ? AND position = ?"
    stmt.bind 1 moduleName
    stmt.bind 2 position
    if (← stmt.step) then
      let isUnsafe := (← stmt.columnInt64 0) != 0
      let ctors ← loadConstructors db moduleName position
      return some <| .inductiveInfo { toInfo := info, isUnsafe, ctors }
    return none
  | "structure" =>
    let stmt ← db.prepare "SELECT is_class FROM structures WHERE module_name = ? AND position = ?"
    stmt.bind 1 moduleName
    stmt.bind 2 position
    if (← stmt.step) then
      let parents ← loadStructureParents db moduleName position
      let fieldInfo ← loadStructureFields db moduleName position
      let some ctor ← loadStructureConstructor db moduleName position
        | return none
      return some <| .structureInfo { toInfo := info, fieldInfo, parents, ctor }
    return none
  | "class" =>
    let stmt ← db.prepare "SELECT is_class FROM structures WHERE module_name = ? AND position = ?"
    stmt.bind 1 moduleName
    stmt.bind 2 position
    if (← stmt.step) then
      let parents ← loadStructureParents db moduleName position
      let fieldInfo ← loadStructureFields db moduleName position
      let some ctor ← loadStructureConstructor db moduleName position
        | return none
      return some <| .classInfo { toInfo := info, fieldInfo, parents, ctor }
    return none
  | "class inductive" =>
    let stmt ← db.prepare "SELECT is_unsafe FROM class_inductives WHERE module_name = ? AND position = ?"
    stmt.bind 1 moduleName
    stmt.bind 2 position
    if (← stmt.step) then
      let isUnsafe := (← stmt.columnInt64 0) != 0
      let ctors ← loadConstructors db moduleName position
      return some <| .classInductiveInfo { toInfo := info, isUnsafe, ctors }
    return none
  | "constructor" =>
    -- Constructors are handled as part of their parent inductive
    return some <| .ctorInfo info
  | _ =>
    return none

/-- Load a module from the database. -/
def loadModule (db : SQLite) (moduleName : Name) : IO Process.Module := do
  let modNameStr := moduleName.toString
  let imports ← getModuleImports db moduleName
  -- Load all members (declarations and module docs) with their positions.
  -- We'll sort by (declaration range, position) to maintain deterministic ordering
  -- even when multiple declarations have the same position (which happens for
  -- auto-generated declarations like instance defaults).
  let stmt ← db.prepare "
    SELECT n.position, n.kind, n.name, n.type, n.sorried, n.render
    FROM name_info n
    WHERE n.module_name = ?"
  stmt.bind 1 modNameStr
  let mut members : Array (Int64 × Process.ModuleMember) := #[]
  while (← stmt.step) do
    let position ← stmt.columnInt64 0
    let kind ← stmt.columnText 1
    let name := (← stmt.columnText 2).toName
    let typeBlob ← stmt.columnBlob 3
    let sorried := (← stmt.columnInt64 4) != 0
    let render := (← stmt.columnInt64 5) != 0
    if let some docInfo ← loadDocInfo db modNameStr position kind name typeBlob sorried render then
      members := members.push (position, .docInfo docInfo)
  -- Load module docs
  let mdStmt ← db.prepare "
    SELECT m.position, m.text
    FROM markdown_docstrings m
    WHERE m.module_name = ?
      AND m.position NOT IN (SELECT position FROM name_info WHERE module_name = ?)"
  mdStmt.bind 1 modNameStr
  mdStmt.bind 2 modNameStr
  while (← mdStmt.step) do
    let position ← mdStmt.columnInt64 0
    let doc ← mdStmt.columnText 1
    if let some declRange ← loadDeclarationRange db modNameStr position then
      members := members.push (position, .modDoc { doc, declarationRange := declRange })
  -- Sort by (declaration range, position) to maintain deterministic ordering.
  -- Primary key: declaration range position (line, column) using Position.lt
  -- Secondary key: DB position (to break ties when ranges are equal)
  let sortedMembers := members.qsort fun (pos1, m1) (pos2, m2) =>
    let r1 := m1.getDeclarationRange.pos
    let r2 := m2.getDeclarationRange.pos
    if Position.lt r1 r2 then true
    else if Position.lt r2 r1 then false
    else pos1 < pos2  -- Tiebreaker: use DB position
  return { name := moduleName, members := sortedMembers.map (·.2), imports }

/-- Shared index data needed for cross-module linking, without loading full module contents. -/
structure SharedIndex where
  moduleNames : Array Name
  sourceUrls : Std.HashMap Name String
  name2ModIdx : Std.HashMap Name ModuleIdx

/-- Load just the shared index (fast) - only what's needed for cross-module linking. -/
def loadSharedIndex (db : SQLite) : IO SharedIndex := do
  let moduleNames ← getModuleNames db
  let sourceUrls ← getModuleSourceUrls db
  let name2ModIdx ← buildName2ModIdx db moduleNames
  return { moduleNames, sourceUrls, name2ModIdx }

end Reading
