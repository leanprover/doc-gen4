
import DocGen4.RenderedCode
import SQLite

namespace DocGen4.DB

open Lean in
/--
Extracts a deterministic string representation of an inductive type, which is used to invalidate
database schemas in which blobs implicitly depend on serializations of datatypes. Includes
constructor names and their types.
-/
private def inductiveRepr (env : Environment) (name : Name) : String := Id.run do
  let some (.inductInfo info) := env.find? name | return s!"not found: {name}"
  let mut s := s!"inductive {name} : {info.type}\n"
  for ctor in info.ctors do
    let some (.ctorInfo ctorInfo) := env.find? ctor | continue
    let ctorName := ctor.replacePrefix name .anonymous
    s := s ++ s!"  | {ctorName} : {ctorInfo.type}\n"
  return s

namespace Internals
open Lean Elab Term in
/--
Gets a string representation of inductive type definitions, computed at compile time.
-/
scoped elab "inductiveRepr![" types:ident,* "]" : term => do
  let env ← getEnv
  let mut reprs : Array String := #[]
  for type in types.getElems do
    let name ← resolveGlobalConstNoOverload type
    reprs := reprs.push (inductiveRepr env name)
  return .lit (.strVal (String.intercalate "\n" reprs.toList))
end Internals

open Internals in
open Lean.Widget in
/--
The datatypes that are serialized to the database. If they change, then the database should be
rebuilt.
-/
def serializedCodeTypeDefs : String :=
  inductiveRepr![
    SortFormer,
    RenderedCode.Tag,
    TaggedText
  ]

def getDb (dbFile : System.FilePath) : IO SQLite := do
  -- SQLite atomically creates the DB file, and the schema and journal settings here are applied
  -- idempotently. This avoids DB creation race conditions.
  let db ← SQLite.openWith dbFile .readWriteCreate
  db.exec "PRAGMA busy_timeout = 86400000"  -- 24 hours - effectively no timeout for parallel builds
  db.exec "PRAGMA journal_mode = WAL"
  db.exec "PRAGMA synchronous = OFF"
  db.exec "PRAGMA foreign_keys = ON"
  try
    db.transaction (db.exec ddl)
  catch
  | e =>
    throw <| .userError s!"Exception while creating schema: {e}"
  -- Check schema version via DDL hash and type definition hash
  let ddlHash := toString ddl.hash
  let typeHash := toString serializedCodeTypeDefs.hash
  let stmt ← db.prepare "SELECT key, value FROM schema_meta"
  let mut storedDdlHash : Option String := none
  let mut storedTypeHash : Option String := none
  while ← stmt.step do
    let key ← stmt.columnText 0
    let value ← stmt.columnText 1
    if key == "ddl_hash" then storedDdlHash := some value
    if key == "type_hash" then storedTypeHash := some value
  match storedDdlHash, storedTypeHash with
  | none, none =>
    -- New database, store the hashes
    db.exec s!"INSERT INTO schema_meta (key, value) VALUES ('ddl_hash', '{ddlHash}')"
    db.exec s!"INSERT INTO schema_meta (key, value) VALUES ('type_hash', '{typeHash}')"
  | some stored, _ =>
    if stored != ddlHash then
      throw <| .userError s!"Database schema is outdated (DDL hash mismatch). Run `lake clean` or delete '{dbFile}' and rebuild."
    match storedTypeHash with
    | none =>
      -- Older DB without type hash, add it
      db.exec s!"INSERT INTO schema_meta (key, value) VALUES ('type_hash', '{typeHash}')"
    | some storedType =>
      if storedType != typeHash then
        throw <| .userError s!"Database schema is outdated (serialized type definitions changed). Run `lake clean` or delete '{dbFile}' and rebuild."
  | none, some _ => -- Shouldn't happen, but handle gracefully
    db.exec s!"INSERT INTO schema_meta (key, value) VALUES ('ddl_hash', '{ddlHash}')"
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
  type BLOB NOT NULL,
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
  FOREIGN KEY (module_name, position) REFERENCES name_info(module_name, position) ON DELETE CASCADE,
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
  PRIMARY KEY (module_name, position),
  FOREIGN KEY (module_name, position) REFERENCES name_info(module_name, position) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS definition_equations (
  module_name TEXT NOT NULL,
  position INTEGER NOT NULL,
  code BLOB,
  text_length INTEGER NOT NULL,
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
  type BLOB NOT NULL,
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

CREATE TABLE IF NOT EXISTS tactics (
  module_name TEXT NOT NULL,
  internal_name TEXT NOT NULL,
  user_name TEXT NOT NULL,
  doc_string TEXT NOT NULL,
  PRIMARY KEY (module_name, internal_name),
  FOREIGN KEY (module_name) REFERENCES modules(name) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS tactic_tags (
  module_name TEXT NOT NULL,
  internal_name TEXT NOT NULL,
  tag TEXT NOT NULL,
  PRIMARY KEY (module_name, internal_name, tag),
  FOREIGN KEY (module_name, internal_name) REFERENCES tactics(module_name, internal_name) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS schema_meta (
  key TEXT PRIMARY KEY,
  value TEXT NOT NULL
);
"#

end DocGen4.DB
