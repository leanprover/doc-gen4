
import DocGen4.RenderedCode
import SQLite
import DocGen4.Helpers
import DocGen4.DB.VersoDocString
import DocGen4.DB.Schema
import DocGen4.DB.Read

namespace DocGen4.DB

structure DB extends ReadOps where
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

def ensureDb (values : DocstringValues) (dbFile : System.FilePath) : IO DB := do
  have := versoDocStringQueryParam values
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
  let saveMarkdownDocstringStmt ← sqlite.prepare "INSERT INTO declaration_markdown_docstrings (module_name, position, text) VALUES (?, ?, ?)"
  let saveMarkdownDocstring modName position text := withDbContext "write:insert:declaration_markdown_docstrings" do
    saveMarkdownDocstringStmt.bind 1 modName
    saveMarkdownDocstringStmt.bind 2 position
    saveMarkdownDocstringStmt.bind 3 text
    run saveMarkdownDocstringStmt
  let saveModuleDocStmt ← sqlite.prepare "INSERT INTO module_docs_markdown (module_name, position, text) VALUES (?, ?, ?)"
  let saveModuleDoc modName position text := withDbContext "write:insert:module_docs_markdown" do
    saveModuleDocStmt.bind 1 modName
    saveModuleDocStmt.bind 2 position
    saveModuleDocStmt.bind 3 text
    run saveModuleDocStmt
  let saveVersoDocstringStmt ← sqlite.prepare "INSERT INTO declaration_verso_docstrings (module_name, position, content) VALUES (?, ?, ?)"
  let saveVersoDocstring modName position text := withDbContext "write:insert:declaration_verso_docstrings" do
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
    saveDeclarationRangeStmt.bind 3 declRange.pos.line.toInt64
    saveDeclarationRangeStmt.bind 4 declRange.pos.column.toInt64
    saveDeclarationRangeStmt.bind 5 declRange.charUtf16.toInt64
    saveDeclarationRangeStmt.bind 6 declRange.endPos.line.toInt64
    saveDeclarationRangeStmt.bind 7 declRange.endPos.column.toInt64
    saveDeclarationRangeStmt.bind 8 declRange.endCharUtf16.toInt64
    run saveDeclarationRangeStmt
  let saveInfoStmt ← sqlite.prepare "INSERT INTO name_info (module_name, position, kind, name, type, sorried, render) VALUES (?, ?, ?, ?, ?, ?, ?)"
  let saveArgStmt ← sqlite.prepare "INSERT INTO declaration_args (module_name, position, sequence, binder, is_implicit) VALUES (?, ?, ?, ?, ?)"
  let saveAttrStmt ← sqlite.prepare "INSERT INTO declaration_attrs (module_name, position, sequence, attr) VALUES (?, ?, ?, ?)"
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
      withDbContext "write:insert:declaration_args" do
        saveArgStmt.bind 1 modName
        saveArgStmt.bind 2 position
        saveArgStmt.bind 3 j.toInt64
        saveArgStmt.bind 4 arg.binder
        saveArgStmt.bind 5 arg.implicit
        run saveArgStmt
    -- Save attrs
    for h : j in 0...info.attrs.size do
      let attr := info.attrs[j]
      withDbContext "write:insert:declaration_attrs" do
        saveAttrStmt.bind 1 modName
        saveAttrStmt.bind 2 position
        saveAttrStmt.bind 3 j.toInt64
        saveAttrStmt.bind 4 attr
        run saveAttrStmt
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
  let saveDefinitionStmt ← sqlite.prepare "INSERT INTO definitions (module_name, position, is_unsafe, hints, is_noncomputable) VALUES (?, ?, ?, ?, ?)"
  let saveDefinition modName position isUnsafe hints isNonComputable := withDbContext "write:insert:definitions" do
    saveDefinitionStmt.bind 1 modName
    saveDefinitionStmt.bind 2 position
    saveDefinitionStmt.bind 3 isUnsafe
    saveDefinitionStmt.bind 4 hints
    saveDefinitionStmt.bind 5 isNonComputable
    run saveDefinitionStmt
  let saveDefinitionEquationStmt ← sqlite.prepare "INSERT INTO definition_equations (module_name, position, code, text_length, sequence) VALUES (?, ?, ?, ?, ?)"
  let saveDefinitionEquation modName position (code : RenderedCode) sequence := withDbContext "write:insert:definition_equations" do
    let textLength := RenderedCode.textLength code
    saveDefinitionEquationStmt.bind 1 modName
    saveDefinitionEquationStmt.bind 2 position
    saveDefinitionEquationStmt.bind 3 <|
      if textLength < Process.equationLimit then some code
      else none
    saveDefinitionEquationStmt.bind 4 textLength.toInt64
    saveDefinitionEquationStmt.bind 5 sequence
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
  let saveTacticStmt ← sqlite.prepare "INSERT INTO tactics (module_name, internal_name, user_name, doc_string) VALUES (?, ?, ?, ?)"
  let saveTacticTagStmt ← sqlite.prepare "INSERT INTO tactic_tags (module_name, internal_name, tag) VALUES (?, ?, ?)"
  let saveTactic modName (tactic : Process.TacticInfo Process.MarkdownDocstring) := withDbContext "write:insert:tactics" do
    saveTacticStmt.bind 1 modName
    saveTacticStmt.bind 2 tactic.internalName.toString
    saveTacticStmt.bind 3 tactic.userName
    saveTacticStmt.bind 4 tactic.docString
    run saveTacticStmt
    for tag in tactic.tags do
      saveTacticTagStmt.bind 1 modName
      saveTacticTagStmt.bind 2 tactic.internalName.toString
      saveTacticTagStmt.bind 3 tag.toString
      run saveTacticTagStmt
  let readOps ← mkReadOps sqlite values
  pure { readOps with
    sqlite,
    deleteModule,
    saveModule,
    saveImport,
    saveMarkdownDocstring,
    saveModuleDoc,
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
    saveNameOnly,
    saveInternalName,
    saveTactic
  }

structure DBM.Context where
  values : DocstringValues
  db : DB

abbrev DBM α := ReaderT DBM.Context IO α

def DBM.run (values : DocstringValues) (dbFile : System.FilePath) (act : DBM α) : IO α := do
  let db ← ensureDb values dbFile
  ReaderT.run act { values, db }

def withDB (f : DB → DBM α) : DBM α := do f (← read).db

def withSQLite (f : SQLite → DBM α) : DBM α := do f (← read).db

private def readonlyError : IO α := throw (IO.userError "DB opened for reading only")

def openForReading (dbFile : System.FilePath) (values : DocstringValues) : IO DB := do
  let sqlite ← SQLite.openWith dbFile .readonly
  sqlite.exec "PRAGMA busy_timeout = 86400000"
  let readOps ← mkReadOps sqlite values
  pure {
    sqlite,
    deleteModule := fun _ => readonlyError,
    saveModule := fun _ _ => readonlyError,
    saveImport := fun _ _ => readonlyError,
    saveMarkdownDocstring := fun _ _ _ => readonlyError,
    saveModuleDoc := fun _ _ _ => readonlyError,
    saveVersoDocstring := fun _ _ _ => readonlyError,
    saveDeclarationRange := fun _ _ _ => readonlyError,
    saveInfo := fun _ _ _ _ => readonlyError,
    saveAxiom := fun _ _ _ => readonlyError,
    saveOpaque := fun _ _ _ => readonlyError,
    saveDefinition := fun _ _ _ _ _ => readonlyError,
    saveDefinitionEquation := fun _ _ _ _ => readonlyError,
    saveInstance := fun _ _ _ => readonlyError,
    saveInstanceArg := fun _ _ _ _ => readonlyError,
    saveInductive := fun _ _ _ => readonlyError,
    saveConstructor := fun _ _ _ => readonlyError,
    saveClassInductive := fun _ _ _ => readonlyError,
    saveStructure := fun _ _ _ => readonlyError,
    saveStructureConstructor := fun _ _ _ _ => readonlyError,
    saveNameOnly := fun _ _ _ _ _ _ => readonlyError,
    saveStructureParent := fun _ _ _ _ _ => readonlyError,
    saveStructureField := fun _ _ _ _ _ _ => readonlyError,
    saveStructureFieldArg := fun _ _ _ _ _ _ => readonlyError,
    saveInternalName := fun _ _ _ => readonlyError,
    saveTactic := fun _ _ => readonlyError,
    getModuleNames := readOps.getModuleNames,
    getModuleSourceUrls := readOps.getModuleSourceUrls,
    getModuleImports := readOps.getModuleImports,
    buildName2ModIdx := readOps.buildName2ModIdx,
    buildRenderedNames := readOps.buildRenderedNames,
    loadModule := readOps.loadModule,
    loadAllTactics := readOps.loadAllTactics,
  }

/-! ## DB Reading -/

section Reading
open Lean

/-- Context needed for cross-module linking, without loading full module contents. -/
structure LinkingContext where
  moduleNames : Array Name
  sourceUrls : Std.HashMap Name String
  name2ModIdx : Std.HashMap Name ModuleIdx
  renderedNames : Std.HashSet Name

/-- Load the linking context from the database. -/
def DB.loadLinkingContext (db : DB) : IO LinkingContext := do
  let moduleNames ← db.getModuleNames
  let sourceUrls ← db.getModuleSourceUrls
  let name2ModIdx ← db.buildName2ModIdx moduleNames
  let renderedNames ← db.buildRenderedNames
  return { moduleNames, sourceUrls, name2ModIdx, renderedNames }

/--
Get transitive closure of imports for given modules using recursive CTE.
Uses dynamic SQL (variable number of placeholders) so cannot be pre-prepared.
-/
def DB.getTransitiveImports (db : DB) (modules : Array Name) : IO (Array Name) := withDbContext "read:transitive_imports" do
  if modules.isEmpty then return #[]
  let placeholders := ", ".intercalate (modules.toList.map fun _ => "(?)")
  let sql := s!"
    WITH RECURSIVE transitive_imports(name) AS (
      VALUES {placeholders}
      UNION
      SELECT mi.imported FROM module_imports mi
      JOIN transitive_imports ti ON mi.importer = ti.name
    )
    SELECT DISTINCT name FROM transitive_imports"
  let stmt ← db.sqlite.prepare sql
  for h : i in [0:modules.size] do
    stmt.bind (i.toInt32 + 1) modules[i].toString
  let mut result := #[]
  while (← stmt.step) do
    let name := (← stmt.columnText 0).toName
    result := result.push name
  return result

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
