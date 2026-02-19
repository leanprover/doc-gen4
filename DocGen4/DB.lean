/-
Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: David Thrane Christiansen
-/
import DocGen4.RenderedCode
import SQLite
import DocGen4.Helpers
import DocGen4.DB.VersoDocString
import DocGen4.DB.Schema
import DocGen4.DB.Read

/-!
# Database Write Interface

This file defines `WriteDB`, the interface for populating the database. Lake runs one `single`
command per module (and `genCore` for Init/Std/Lake/Lean), each of which writes to the shared
SQLite database through `WriteDB`. Later, the `fromDb` command reads everything back via `ReadDB`
(defined in `DocGen4.DB.Read`) and generates HTML.

`WriteDB` and `ReadDB` are separate types because writers and readers have different needs.
The database schema lives in `DocGen4.DB.Schema`.

## Module Item Positions

Within a module, each item (declaration, module doc, constructor) is assigned a sequential `Int64`
position starting from 0. This position serves as the item's identity within the module: the
composite key `(module_name, position)` is the primary key for most tables. Positions are assigned
in the order items appear in the module's `members` array, with constructors and structure fields
interleaved between their parent declarations. For example, a module containing a module doc,
a definition, and an inductive with two constructors might have positions:

  0: module doc
  1: definition
  2: inductive
  3: constructor 1 (of the inductive at position 2)
  4: constructor 2 (of the inductive at position 2)

The position counter is a mutable `Int64` in `updateModuleDb`. Structures consume extra positions
for their constructor and fields. The read side reconstructs module members by querying
`name_info` and `module_docs_markdown` ordered by position.

## Changing the Schema

When adding a new column or table:
  1. Add the DDL in `DocGen4.DB.Schema` (the `ddl` string in `getDb`).
  2. Add a prepared statement to `WriteStmts` and its `prepare` method below.
  3. Add a `WriteStmts.saveXxx` method with positional `bind` calls matching the SQL.
  4. Expose it through `WriteDB` (the structure at the top of this file) and the mutex wrapper
     in `ensureWriteDb`.
  5. Add a prepared statement and loader to `ReadStmts` in `DocGen4.DB.Read`.
  6. If the change affects serialized blob types, update `serializedCodeTypeDefs` in
     `DocGen4.DB.Schema` so the type hash invalidates stale databases.
-/

namespace DocGen4.DB

/-- A write-only database handle. Used during the analysis phase to populate the database. -/
structure WriteDB where
  sqlite : SQLite
  deleteModule (modName : String) : IO Unit
  saveModule (modName : String) (sourceUrl? : Option String) : IO Unit
  saveImport (modName : String) (imported : Lean.Name) : IO Unit
  saveMarkdownDocstring (modName : String) (position : Int64) (text : String) : IO Unit
  saveModuleDoc (modName : String) (position : Int64) (text : String) : IO Unit
  saveVersoDocstring (modName : String) (position : Int64) (text : Lean.VersoDocString) : IO Unit
  saveDeclarationRange (modName : String) (position : Int64) (declRange : Lean.DeclarationRange) : IO Unit
  saveInfo (modName : String) (position : Int64) (kind : String) (info : Process.Info) : IO Unit
  saveAxiom (modName : String) (position : Int64) (isUnsafe : Bool) : IO Unit
  saveOpaque (modName : String) (position : Int64) (safety : Lean.DefinitionSafety) : IO Unit
  saveDefinition (modName : String) (position : Int64) (isUnsafe : Bool) (hints : Lean.ReducibilityHints) (isNonComputable : Bool) : IO Unit
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
  /-- Save an internal name (like a recursor) that should link to its target declaration -/
  saveInternalName (name : Lean.Name) (targetModule : String) (targetPosition : Int64) : IO Unit
  /-- Save a tactic defined in this module -/
  saveTactic (modName : String) (tactic : Process.TacticInfo Process.MarkdownDocstring) : IO Unit

def WriteDB.saveDocstring (db : WriteDB) (modName : String) (position : Int64) (text : String ⊕ Lean.VersoDocString) : IO Unit :=
  match text with
  | .inl md => db.saveMarkdownDocstring modName position md
  | .inr v => db.saveVersoDocstring modName position v

instance : Coe WriteDB SQLite where
  coe := WriteDB.sqlite

private def run (stmt : SQLite.Stmt) : IO Unit := do
  stmt.exec
  stmt.reset
  stmt.clearBindings

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

-- Each `WriteStmts` field is a prepared SQLite statement. The corresponding `WriteStmts.funName`
-- function binds parameters by position (1-indexed, matching the `?` placeholders in the SQL).
-- The SQLite bindings don't check that bind positions match the SQL at compile time, so be
-- careful when adding or reordering columns.
private structure WriteStmts where
  values : DocstringValues
  deleteModuleStmt : SQLite.Stmt
  saveModuleStmt : SQLite.Stmt
  saveImportStmt : SQLite.Stmt
  saveMarkdownDocstringStmt : SQLite.Stmt
  saveModuleDocStmt : SQLite.Stmt
  saveVersoDocstringStmt : SQLite.Stmt
  saveDeclarationRangeStmt : SQLite.Stmt
  saveInfoStmt : SQLite.Stmt
  saveArgStmt : SQLite.Stmt
  saveAttrStmt : SQLite.Stmt
  saveAxiomStmt : SQLite.Stmt
  saveOpaqueStmt : SQLite.Stmt
  saveDefinitionStmt : SQLite.Stmt
  saveDefinitionEquationStmt : SQLite.Stmt
  saveInstanceStmt : SQLite.Stmt
  saveInstanceArgStmt : SQLite.Stmt
  saveInductiveStmt : SQLite.Stmt
  saveConstructorStmt : SQLite.Stmt
  saveClassInductiveStmt : SQLite.Stmt
  saveStructureStmt : SQLite.Stmt
  saveStructureConstructorStmt : SQLite.Stmt
  saveStructureParentStmt : SQLite.Stmt
  saveStructureFieldStmt : SQLite.Stmt
  saveStructureFieldArgStmt : SQLite.Stmt
  saveNameOnlyStmt : SQLite.Stmt
  saveInternalNameStmt : SQLite.Stmt
  saveTacticStmt : SQLite.Stmt
  saveTacticTagStmt : SQLite.Stmt

private def WriteStmts.prepare (sqlite : SQLite) (values : DocstringValues) : IO WriteStmts := do
  pure {
    values
    deleteModuleStmt := ← sqlite.prepare "DELETE FROM modules WHERE name = ?"
    saveModuleStmt := ← sqlite.prepare "INSERT INTO modules (name, source_url) VALUES (?, ?)"
    -- INSERT OR IGNORE because the module system often results in multiple imports of the same module
    saveImportStmt := ← sqlite.prepare "INSERT OR IGNORE INTO module_imports (importer, imported) VALUES (?, ?)"
    saveMarkdownDocstringStmt := ← sqlite.prepare "INSERT INTO declaration_markdown_docstrings (module_name, position, text) VALUES (?, ?, ?)"
    saveModuleDocStmt := ← sqlite.prepare "INSERT INTO module_docs_markdown (module_name, position, text) VALUES (?, ?, ?)"
    saveVersoDocstringStmt := ← sqlite.prepare "INSERT INTO declaration_verso_docstrings (module_name, position, content) VALUES (?, ?, ?)"
    saveDeclarationRangeStmt := ← sqlite.prepare
      "INSERT INTO declaration_ranges (module_name, position, start_line, start_column, start_utf16, end_line, end_column, end_utf16) VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
    saveInfoStmt := ← sqlite.prepare "INSERT INTO name_info (module_name, position, kind, name, type, sorried, render) VALUES (?, ?, ?, ?, ?, ?, ?)"
    saveArgStmt := ← sqlite.prepare "INSERT INTO declaration_args (module_name, position, sequence, binder, is_implicit) VALUES (?, ?, ?, ?, ?)"
    saveAttrStmt := ← sqlite.prepare "INSERT INTO declaration_attrs (module_name, position, sequence, attr) VALUES (?, ?, ?, ?)"
    saveAxiomStmt := ← sqlite.prepare "INSERT INTO axioms (module_name, position, is_unsafe) VALUES (?, ?, ?)"
    saveOpaqueStmt := ← sqlite.prepare "INSERT INTO opaques (module_name, position, safety) VALUES (?, ?, ?)"
    saveDefinitionStmt := ← sqlite.prepare "INSERT INTO definitions (module_name, position, is_unsafe, hints, is_noncomputable) VALUES (?, ?, ?, ?, ?)"
    saveDefinitionEquationStmt := ← sqlite.prepare "INSERT INTO definition_equations (module_name, position, code, text_length, sequence) VALUES (?, ?, ?, ?, ?)"
    saveInstanceStmt := ← sqlite.prepare "INSERT INTO instances (module_name, position, class_name) VALUES (?, ?, ?)"
    saveInstanceArgStmt := ← sqlite.prepare "INSERT INTO instance_args (module_name, position, sequence, type_name) VALUES (?, ?, ?, ?)"
    saveInductiveStmt := ← sqlite.prepare "INSERT INTO inductives (module_name, position, is_unsafe) VALUES (?, ?, ?)"
    saveConstructorStmt := ← sqlite.prepare "INSERT INTO constructors (module_name, position, type_position) VALUES (?, ?, ?)"
    saveClassInductiveStmt := ← sqlite.prepare "INSERT INTO class_inductives (module_name, position, is_unsafe) VALUES (?, ?, ?)"
    saveStructureStmt := ← sqlite.prepare "INSERT INTO structures (module_name, position, is_class) VALUES (?, ?, ?)"
    saveStructureConstructorStmt := ← sqlite.prepare "INSERT INTO structure_constructors (module_name, position, ctor_position, name, type) VALUES (?, ?, ?, ?, ?)"
    saveStructureParentStmt := ← sqlite.prepare "INSERT INTO structure_parents (module_name, position, sequence, projection_fn, type) VALUES (?, ?, ?, ?, ?)"
    saveStructureFieldStmt := ← sqlite.prepare "INSERT INTO structure_fields (module_name, position, sequence, proj_name, type, is_direct) VALUES (?, ?, ?, ?, ?, ?)"
    saveStructureFieldArgStmt := ← sqlite.prepare "INSERT INTO structure_field_args (module_name, position, field_sequence, arg_sequence, binder, is_implicit) VALUES (?, ?, ?, ?, ?, ?)"
    saveNameOnlyStmt := ← sqlite.prepare "INSERT INTO name_info (module_name, position, kind, name, type, sorried, render) VALUES (?, ?, ?, ?, ?, 0, 0)"
    saveInternalNameStmt := ← sqlite.prepare "INSERT OR IGNORE INTO internal_names (name, target_module, target_position) VALUES (?, ?, ?)"
    saveTacticStmt := ← sqlite.prepare "INSERT INTO tactics (module_name, internal_name, user_name, doc_string) VALUES (?, ?, ?, ?)"
    saveTacticTagStmt := ← sqlite.prepare "INSERT INTO tactic_tags (module_name, internal_name, tag) VALUES (?, ?, ?)"
  }

private def WriteStmts.deleteModule (s : WriteStmts) (modName : String) : IO Unit := withDbContext "write:delete:modules" do
  s.deleteModuleStmt.bind 1 modName
  run s.deleteModuleStmt

private def WriteStmts.saveModule (s : WriteStmts) (modName : String) (sourceUrl? : Option String) : IO Unit := withDbContext "write:insert:modules" do
  s.saveModuleStmt.bind 1 modName
  s.saveModuleStmt.bind 2 sourceUrl?
  run s.saveModuleStmt

private def WriteStmts.saveImport (s : WriteStmts) (modName : String) (imported : Lean.Name) : IO Unit := withDbContext "write:insert:module_imports" do
  s.saveImportStmt.bind 1 modName
  s.saveImportStmt.bind 2 imported.toString
  run s.saveImportStmt

private def WriteStmts.saveMarkdownDocstring (s : WriteStmts) (modName : String) (position : Int64) (text : String) : IO Unit := withDbContext "write:insert:declaration_markdown_docstrings" do
  s.saveMarkdownDocstringStmt.bind 1 modName
  s.saveMarkdownDocstringStmt.bind 2 position
  s.saveMarkdownDocstringStmt.bind 3 text
  run s.saveMarkdownDocstringStmt

private def WriteStmts.saveModuleDoc (s : WriteStmts) (modName : String) (position : Int64) (text : String) : IO Unit := withDbContext "write:insert:module_docs_markdown" do
  s.saveModuleDocStmt.bind 1 modName
  s.saveModuleDocStmt.bind 2 position
  s.saveModuleDocStmt.bind 3 text
  run s.saveModuleDocStmt

private def WriteStmts.saveVersoDocstring (s : WriteStmts) (modName : String) (position : Int64) (text : Lean.VersoDocString) : IO Unit := do
  have := versoDocStringQueryParam s.values
  withDbContext "write:insert:declaration_verso_docstrings" do
    s.saveVersoDocstringStmt.bind 1 modName
    s.saveVersoDocstringStmt.bind 2 position
    s.saveVersoDocstringStmt.bind 3 text
    run s.saveVersoDocstringStmt

private def WriteStmts.saveDeclarationRange (s : WriteStmts) (modName : String) (position : Int64) (declRange : Lean.DeclarationRange) : IO Unit := withDbContext "write:insert:declaration_ranges" do
  s.saveDeclarationRangeStmt.bind 1 modName
  s.saveDeclarationRangeStmt.bind 2 position
  s.saveDeclarationRangeStmt.bind 3 declRange.pos.line.toInt64
  s.saveDeclarationRangeStmt.bind 4 declRange.pos.column.toInt64
  s.saveDeclarationRangeStmt.bind 5 declRange.charUtf16.toInt64
  s.saveDeclarationRangeStmt.bind 6 declRange.endPos.line.toInt64
  s.saveDeclarationRangeStmt.bind 7 declRange.endPos.column.toInt64
  s.saveDeclarationRangeStmt.bind 8 declRange.endCharUtf16.toInt64
  run s.saveDeclarationRangeStmt

private def WriteStmts.saveInfo (s : WriteStmts) (modName : String) (position : Int64) (kind : String) (info : Process.Info) : IO Unit := withDbContext "write:insert:name_info" do
  s.saveInfoStmt.bind 1 modName
  s.saveInfoStmt.bind 2 position
  s.saveInfoStmt.bind 3 kind
  s.saveInfoStmt.bind 4 info.name.toString
  s.saveInfoStmt.bind 5 info.type
  s.saveInfoStmt.bind 6 info.sorried
  s.saveInfoStmt.bind 7 info.render
  run s.saveInfoStmt
  match info.doc with
  | some (.inl md) => s.saveMarkdownDocstring modName position md
  | some (.inr v) => s.saveVersoDocstring modName position v
  | none => pure ()
  for h : j in 0...info.args.size do
    let arg := info.args[j]
    withDbContext "write:insert:declaration_args" do
      s.saveArgStmt.bind 1 modName
      s.saveArgStmt.bind 2 position
      s.saveArgStmt.bind 3 j.toInt64
      s.saveArgStmt.bind 4 arg.binder
      s.saveArgStmt.bind 5 arg.implicit
      run s.saveArgStmt
  for h : j in 0...info.attrs.size do
    let attr := info.attrs[j]
    withDbContext "write:insert:declaration_attrs" do
      s.saveAttrStmt.bind 1 modName
      s.saveAttrStmt.bind 2 position
      s.saveAttrStmt.bind 3 j.toInt64
      s.saveAttrStmt.bind 4 attr
      run s.saveAttrStmt

private def WriteStmts.saveAxiom (s : WriteStmts) (modName : String) (position : Int64) (isUnsafe : Bool) : IO Unit := withDbContext "write:insert:axioms" do
  s.saveAxiomStmt.bind 1 modName
  s.saveAxiomStmt.bind 2 position
  s.saveAxiomStmt.bind 3 isUnsafe
  run s.saveAxiomStmt

private def WriteStmts.saveOpaque (s : WriteStmts) (modName : String) (position : Int64) (safety : Lean.DefinitionSafety) : IO Unit := withDbContext "write:insert:opaques" do
  s.saveOpaqueStmt.bind 1 modName
  s.saveOpaqueStmt.bind 2 position
  s.saveOpaqueStmt.bind 3 safety
  run s.saveOpaqueStmt

private def WriteStmts.saveDefinition (s : WriteStmts) (modName : String) (position : Int64) (isUnsafe : Bool) (hints : Lean.ReducibilityHints) (isNonComputable : Bool) : IO Unit := withDbContext "write:insert:definitions" do
  s.saveDefinitionStmt.bind 1 modName
  s.saveDefinitionStmt.bind 2 position
  s.saveDefinitionStmt.bind 3 isUnsafe
  s.saveDefinitionStmt.bind 4 hints
  s.saveDefinitionStmt.bind 5 isNonComputable
  run s.saveDefinitionStmt

private def WriteStmts.saveDefinitionEquation (s : WriteStmts) (modName : String) (position : Int64) (code : RenderedCode) (sequence : Int64) : IO Unit := withDbContext "write:insert:definition_equations" do
  let textLength := RenderedCode.textLength code
  s.saveDefinitionEquationStmt.bind 1 modName
  s.saveDefinitionEquationStmt.bind 2 position
  s.saveDefinitionEquationStmt.bind 3 <|
    if textLength < Process.equationLimit then some code
    else none
  s.saveDefinitionEquationStmt.bind 4 textLength.toInt64
  s.saveDefinitionEquationStmt.bind 5 sequence
  run s.saveDefinitionEquationStmt

private def WriteStmts.saveInstance (s : WriteStmts) (modName : String) (position : Int64) (className : String) : IO Unit := withDbContext "write:insert:instances" do
  s.saveInstanceStmt.bind 1 modName
  s.saveInstanceStmt.bind 2 position
  s.saveInstanceStmt.bind 3 className
  run s.saveInstanceStmt

private def WriteStmts.saveInstanceArg (s : WriteStmts) (modName : String) (position : Int64) (sequence : Int64) (typeName : String) : IO Unit := withDbContext "write:insert:instance_args" do
  s.saveInstanceArgStmt.bind 1 modName
  s.saveInstanceArgStmt.bind 2 position
  s.saveInstanceArgStmt.bind 3 sequence
  s.saveInstanceArgStmt.bind 4 typeName
  run s.saveInstanceArgStmt

private def WriteStmts.saveInductive (s : WriteStmts) (modName : String) (position : Int64) (isUnsafe : Bool) : IO Unit := withDbContext "write:insert:inductives" do
  s.saveInductiveStmt.bind 1 modName
  s.saveInductiveStmt.bind 2 position
  s.saveInductiveStmt.bind 3 isUnsafe
  run s.saveInductiveStmt

private def WriteStmts.saveConstructor (s : WriteStmts) (modName : String) (position : Int64) (typePosition : Int64) : IO Unit := withDbContext "write:insert:constructors" do
  s.saveConstructorStmt.bind 1 modName
  s.saveConstructorStmt.bind 2 position
  s.saveConstructorStmt.bind 3 typePosition
  run s.saveConstructorStmt

private def WriteStmts.saveClassInductive (s : WriteStmts) (modName : String) (position : Int64) (isUnsafe : Bool) : IO Unit := withDbContext "write:insert:class_inductives" do
  s.saveClassInductiveStmt.bind 1 modName
  s.saveClassInductiveStmt.bind 2 position
  s.saveClassInductiveStmt.bind 3 isUnsafe
  run s.saveClassInductiveStmt

private def WriteStmts.saveStructure (s : WriteStmts) (modName : String) (position : Int64) (isClass : Bool) : IO Unit := withDbContext "write:insert:structures" do
  s.saveStructureStmt.bind 1 modName
  s.saveStructureStmt.bind 2 position
  s.saveStructureStmt.bind 3 isClass
  run s.saveStructureStmt

private def WriteStmts.saveStructureConstructor (s : WriteStmts) (modName : String) (position : Int64) (ctorPos : Int64) (info : Process.NameInfo) : IO Unit := withDbContext "write:insert:structure_constructors" do
  s.saveStructureConstructorStmt.bind 1 modName
  s.saveStructureConstructorStmt.bind 2 position
  s.saveStructureConstructorStmt.bind 3 ctorPos
  s.saveStructureConstructorStmt.bind 4 info.name.toString
  s.saveStructureConstructorStmt.bind 5 info.type
  run s.saveStructureConstructorStmt
  match info.doc with
  | some (.inl md) => s.saveMarkdownDocstring modName ctorPos md
  | some (.inr v) => s.saveVersoDocstring modName ctorPos v
  | none => pure ()

private def WriteStmts.saveStructureParent (s : WriteStmts) (modName : String) (position : Int64) (sequence : Int32) (projectionFn : String) (type : RenderedCode) : IO Unit := withDbContext "write:insert:structure_parents" do
  s.saveStructureParentStmt.bind 1 modName
  s.saveStructureParentStmt.bind 2 position
  s.saveStructureParentStmt.bind 3 sequence
  s.saveStructureParentStmt.bind 4 projectionFn
  s.saveStructureParentStmt.bind 5 type
  run s.saveStructureParentStmt

-- Store projection function name directly; lookup happens at load time
private def WriteStmts.saveStructureField (s : WriteStmts) (modName : String) (position : Int64) (sequence : Int64) (projName : String) (type : RenderedCode) (isDirect : Bool) : IO Unit := withDbContext "write:insert:structure_fields" do
  s.saveStructureFieldStmt.bind 1 modName
  s.saveStructureFieldStmt.bind 2 position
  s.saveStructureFieldStmt.bind 3 sequence
  s.saveStructureFieldStmt.bind 4 projName
  s.saveStructureFieldStmt.bind 5 type
  s.saveStructureFieldStmt.bind 6 isDirect
  run s.saveStructureFieldStmt

private def WriteStmts.saveStructureFieldArg (s : WriteStmts) (modName : String) (position : Int64) (fieldSeq : Int64) (argSeq : Int64) (binder : RenderedCode) (isImplicit : Bool) : IO Unit := withDbContext "write:insert:structure_field_args" do
  s.saveStructureFieldArgStmt.bind 1 modName
  s.saveStructureFieldArgStmt.bind 2 position
  s.saveStructureFieldArgStmt.bind 3 fieldSeq
  s.saveStructureFieldArgStmt.bind 4 argSeq
  s.saveStructureFieldArgStmt.bind 5 binder
  s.saveStructureFieldArgStmt.bind 6 isImplicit
  run s.saveStructureFieldArgStmt

private def WriteStmts.saveNameOnly (s : WriteStmts) (modName : String) (position : Int64) (kind : String) (name : Lean.Name) (type : RenderedCode) (declRange : Lean.DeclarationRange) : IO Unit := withDbContext "write:insert:name_info:nameonly" do
  s.saveNameOnlyStmt.bind 1 modName
  s.saveNameOnlyStmt.bind 2 position
  s.saveNameOnlyStmt.bind 3 kind
  s.saveNameOnlyStmt.bind 4 name.toString
  s.saveNameOnlyStmt.bind 5 type
  run s.saveNameOnlyStmt
  s.saveDeclarationRange modName position declRange

private def WriteStmts.saveInternalName (s : WriteStmts) (name : Lean.Name) (targetModule : String) (targetPosition : Int64) : IO Unit := withDbContext "write:insert:internal_names" do
  s.saveInternalNameStmt.bind 1 name.toString
  s.saveInternalNameStmt.bind 2 targetModule
  s.saveInternalNameStmt.bind 3 targetPosition
  run s.saveInternalNameStmt

private def WriteStmts.saveTactic (s : WriteStmts) (modName : String) (tactic : Process.TacticInfo Process.MarkdownDocstring) : IO Unit := withDbContext "write:insert:tactics" do
  s.saveTacticStmt.bind 1 modName
  s.saveTacticStmt.bind 2 tactic.internalName.toString
  s.saveTacticStmt.bind 3 tactic.userName
  s.saveTacticStmt.bind 4 tactic.docString
  run s.saveTacticStmt
  for tag in tactic.tags do
    s.saveTacticTagStmt.bind 1 modName
    s.saveTacticTagStmt.bind 2 tactic.internalName.toString
    s.saveTacticTagStmt.bind 3 tag.toString
    run s.saveTacticTagStmt

def ensureWriteDb (values : DocstringValues) (dbFile : System.FilePath) : IO WriteDB := do
  let sqlite ← getDb dbFile
  let ws ← WriteStmts.prepare sqlite values
  let writeMutex ← Std.Mutex.new ws
  pure {
    sqlite,
    deleteModule modName := writeMutex.atomically do (← get).deleteModule modName
    saveModule modName sourceUrl? := writeMutex.atomically do (← get).saveModule modName sourceUrl?
    saveImport modName imported := writeMutex.atomically do (← get).saveImport modName imported
    saveMarkdownDocstring modName position text := writeMutex.atomically do (← get).saveMarkdownDocstring modName position text
    saveModuleDoc modName position text := writeMutex.atomically do (← get).saveModuleDoc modName position text
    saveVersoDocstring modName position text := writeMutex.atomically do (← get).saveVersoDocstring modName position text
    saveDeclarationRange modName position declRange := writeMutex.atomically do (← get).saveDeclarationRange modName position declRange
    saveInfo modName position kind info := writeMutex.atomically do (← get).saveInfo modName position kind info
    saveAxiom modName position isUnsafe := writeMutex.atomically do (← get).saveAxiom modName position isUnsafe
    saveOpaque modName position safety := writeMutex.atomically do (← get).saveOpaque modName position safety
    saveDefinition modName position isUnsafe hints isNonComputable := writeMutex.atomically do (← get).saveDefinition modName position isUnsafe hints isNonComputable
    saveDefinitionEquation modName position code sequence := writeMutex.atomically do (← get).saveDefinitionEquation modName position code sequence
    saveInstance modName position className := writeMutex.atomically do (← get).saveInstance modName position className
    saveInstanceArg modName position sequence typeName := writeMutex.atomically do (← get).saveInstanceArg modName position sequence typeName
    saveInductive modName position isUnsafe := writeMutex.atomically do (← get).saveInductive modName position isUnsafe
    saveConstructor modName position typePosition := writeMutex.atomically do (← get).saveConstructor modName position typePosition
    saveClassInductive modName position isUnsafe := writeMutex.atomically do (← get).saveClassInductive modName position isUnsafe
    saveStructure modName position isClass := writeMutex.atomically do (← get).saveStructure modName position isClass
    saveStructureConstructor modName position ctorPos info := writeMutex.atomically do (← get).saveStructureConstructor modName position ctorPos info
    saveStructureParent modName position sequence projectionFn type := writeMutex.atomically do (← get).saveStructureParent modName position sequence projectionFn type
    saveStructureField modName position sequence projName type isDirect := writeMutex.atomically do (← get).saveStructureField modName position sequence projName type isDirect
    saveStructureFieldArg modName position fieldSeq argSeq binder isImplicit := writeMutex.atomically do (← get).saveStructureFieldArg modName position fieldSeq argSeq binder isImplicit
    saveNameOnly modName position kind name type declRange := writeMutex.atomically do (← get).saveNameOnly modName position kind name type declRange
    saveInternalName name targetModule targetPosition := writeMutex.atomically do (← get).saveInternalName name targetModule targetPosition
    saveTactic modName tactic := writeMutex.atomically do (← get).saveTactic modName tactic
  }

structure DBM.Context where
  values : DocstringValues
  db : WriteDB

abbrev DBM α := ReaderT DBM.Context IO α

def DBM.run (values : DocstringValues) (dbFile : System.FilePath) (act : DBM α) : IO α := do
  let db ← ensureWriteDb values dbFile
  ReaderT.run act { values, db }

def withDB (f : WriteDB → DBM α) : DBM α := do f (← read).db

def withSQLite (f : SQLite → DBM α) : DBM α := do f (← read).db.sqlite

/--
Open a database for reading only.

Read operations are protected by a `Std.Mutex` internally (see `mkReadDB`), so a single `ReadDB`
can be shared across tasks without corrupting state. However, sharing serializes all reads through
one SQLite connection. For parallel workloads, each task should call `openForReading` to get its own
connection.
-/
def openForReading (dbFile : System.FilePath) (values : DocstringValues) : IO ReadDB := do
  let sqlite ← SQLite.openWith dbFile .readonly
  sqlite.exec "PRAGMA busy_timeout = 1800000"  -- 30 minutes
  mkReadDB sqlite values

/-! ## DB Reading -/

section Reading
open Lean

/-- Context needed for cross-module linking, without loading full module contents. -/
structure LinkingContext where
  moduleNames : Array Name
  sourceUrls : Std.HashMap Name String
  name2ModIdx : Std.HashMap Name ModuleIdx

/-- Load the linking context from the database. -/
def ReadDB.loadLinkingContext (db : ReadDB) : IO LinkingContext := do
  let moduleNames ← db.getModuleNames
  let sourceUrls ← db.getModuleSourceUrls
  let name2ModIdx ← db.buildName2ModIdx moduleNames
  return { moduleNames, sourceUrls, name2ModIdx }

end Reading

end DB

open DB


def updateModuleDb (values : DocstringValues)
    (doc : Process.AnalyzerResult)
    (buildDir : System.FilePath) (dbFile : String)
    (sourceUrl? : Option String) : IO Unit := do
  let dbFile := buildDir / dbFile
  DBM.run values dbFile <| withDB fun db => do
    for batch in chunked doc.moduleInfo.toArray 100 do
      -- Each module gets its own transaction to reduce lock contention
      let ctxStr :=
        if h : batch.size = 1 then batch[0].1.toString
        else if h : batch.size = 0 then "none"
        else s!"{batch[0].1}-{batch[batch.size-1].1} ({batch.size} modules)"

      let _ ← withDbContext s!"transaction:immediate:{ctxStr}" <| db.sqlite.transaction (mode := .immediate) do
        for (modName, modInfo) in batch do
          let modNameStr := modName.toString
          -- Collect structure field info to save in second pass (after all declarations are in name_info)
          let mut pendingStructureFields : Array (Int64 × Process.StructureInfo) := #[]
          db.deleteModule modNameStr
          db.saveModule modNameStr sourceUrl?
          for imported in modInfo.imports do
            db.saveImport modNameStr imported
          -- Position counter: each item gets a unique sequential position within the module.
          -- Constructors and structure metadata consume positions between their parent and the
          -- next top-level member. See "Module Item Positions" in the module docstring.
          let mut i : Int64 := 0
          for mem in modInfo.members do
            let pos := i
            i := i + 1
            match mem with
            | .modDoc doc =>
              db.saveDeclarationRange modNameStr pos doc.declarationRange
              db.saveModuleDoc modNameStr pos doc.doc
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
                db.saveDefinition modNameStr pos info.isUnsafe info.hints info.isNonComputable
                if let some eqns := info.equations then
                  for h : j in 0...eqns.size do
                    db.saveDefinitionEquation modNameStr pos eqns[j] j.toInt64
              | .instanceInfo info =>
                -- Save definition data (InstanceInfo extends DefinitionInfo)
                db.saveDefinition modNameStr pos info.isUnsafe info.hints info.isNonComputable
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
          -- Save tactics defined in this module
          for tactic in modInfo.tactics do
            db.saveTactic modNameStr tactic
          pure ()
  pure ()

where
  -- Save all recursors (main + aux) for an inductive type
  -- Uses name2ModIdx (from env.const2ModIdx) to check if names exist
  saveRecursors (name2ModIdx : Std.HashMap Lean.Name Lean.ModuleIdx) (db : WriteDB) (modName : String) (pos : Int64) (indName : Lean.Name) : IO Unit := do
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
  saveStructureMetadata (isClass : Bool) (info : Process.StructureInfo) (db : WriteDB) (modName : String) (pos : Int64) (name2ModIdx : Std.HashMap Lean.Name Lean.ModuleIdx) : StateT Int64 IO Unit := do
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
  saveStructureFields (info : Process.StructureInfo) (db : WriteDB) (modName : String) (pos : Int64) : IO Unit := do
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
    | .theoremInfo _ => "theorem"
    | .opaqueInfo _ => "opaque"
    | .definitionInfo _ => "definition"
    | .instanceInfo _ => "instance"
    | .inductiveInfo _ => "inductive"
    | .structureInfo _ => "structure"
    | .classInfo _ => "class"
    | .classInductiveInfo _ => "class inductive"
    | .ctorInfo _ => "constructor"
