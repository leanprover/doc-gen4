
import DocGen4.RenderedCode
import SQLite
import DocGen4.DB.VersoDocString

namespace DocGen4.DB

def withDbContext [MonadLiftT BaseIO m] [MonadControlT IO m] [Monad m] (context : String) (act : m α) : m α :=
  controlAt IO fun runInBase => do
  let ms ← IO.monoMsNow
  try
    runInBase act
  catch
    | e =>
      let ms' ← IO.monoMsNow
      throw <| .userError s!"Exception in `{context}` after {ms' - ms}ms: {e.toString}"

structure ReadOps where
  getModuleNames : IO (Array Lean.Name)
  getModuleSourceUrls : IO (Std.HashMap Lean.Name String)
  getModuleImports : Lean.Name → IO (Array Lean.Name)
  buildName2ModIdx : Array Lean.Name → IO (Std.HashMap Lean.Name Lean.ModuleIdx)
  loadModule : Lean.Name → IO Process.Module

private def done (stmt : SQLite.Stmt) : IO Unit := do
  stmt.reset
  stmt.clearBindings

def _root_.SQLite.Stmt.bind [SQLite.NullableQueryParam α] (stmt : SQLite.Stmt) (index : Int32) (param : α) : IO Unit := do
  SQLite.NullableQueryParam.bind stmt index param

open SQLite.Blob in
/-- Read RenderedCode from a blob. -/
def readRenderedCode (blob : ByteArray) : IO RenderedCode := do
  match fromBinary blob with
  | .ok code => return code
  | .error e => throw <| IO.userError s!"Failed to deserialize RenderedCode: {e}"


open Lean SQLite.Blob in
private structure ReadStmts where
  values : DocstringValues
  loadArgsStmt : SQLite.Stmt
  loadAttrsStmt : SQLite.Stmt
  readMdDocstringStmt : SQLite.Stmt
  readVersoDocstringStmt : SQLite.Stmt
  loadDeclRangeStmt : SQLite.Stmt
  loadEqnsStmt : SQLite.Stmt
  loadInstanceArgsStmt : SQLite.Stmt
  loadStructureParentsStmt : SQLite.Stmt
  loadFieldArgsStmt : SQLite.Stmt
  loadFieldsStmt : SQLite.Stmt
  lookupProjStmt : SQLite.Stmt
  lookupRenderStmt : SQLite.Stmt
  loadStructCtorStmt : SQLite.Stmt
  loadCtorPosStmt : SQLite.Stmt
  loadCtorInfoStmt : SQLite.Stmt
  readAxiomStmt : SQLite.Stmt
  readOpaqueStmt : SQLite.Stmt
  readDefinitionStmt : SQLite.Stmt
  readInstanceStmt : SQLite.Stmt
  readInductiveStmt : SQLite.Stmt
  readStructureStmt : SQLite.Stmt
  readClassInductiveStmt : SQLite.Stmt
  getModuleNamesStmt : SQLite.Stmt
  getModuleSourceUrlsStmt : SQLite.Stmt
  getModuleImportsStmt : SQLite.Stmt
  buildNameInfoStmt : SQLite.Stmt
  buildInternalNamesStmt : SQLite.Stmt
  loadModuleStmt : SQLite.Stmt
  loadModuleDocsStmt : SQLite.Stmt
  loadTacticsStmt : SQLite.Stmt
  loadTacticTagsStmt : SQLite.Stmt

open Lean SQLite.Blob in
private def ReadStmts.prepare (sqlite : SQLite) (values : DocstringValues) : IO ReadStmts := do
  let loadArgsStmt ← sqlite.prepare "SELECT binder, is_implicit FROM declaration_args WHERE module_name = ? AND position = ? ORDER BY sequence"
  let loadAttrsStmt ← sqlite.prepare "SELECT attr FROM declaration_attrs WHERE module_name = ? AND position = ? ORDER BY sequence"
  let readMdDocstringStmt ← sqlite.prepare "SELECT text FROM markdown_docstrings WHERE module_name = ? AND position = ?"
  let readVersoDocstringStmt ← sqlite.prepare "SELECT content FROM verso_docstrings WHERE module_name = ? AND position = ?"
  let loadDeclRangeStmt ← sqlite.prepare "SELECT start_line, start_column, start_utf16, end_line, end_column, end_utf16 FROM declaration_ranges WHERE module_name = ? AND position = ?"
  let loadEqnsStmt ← sqlite.prepare "SELECT code FROM definition_equations WHERE module_name = ? AND position = ? ORDER BY sequence"
  let loadInstanceArgsStmt ← sqlite.prepare "SELECT type_name FROM instance_args WHERE module_name = ? AND position = ? ORDER BY sequence"
  let loadStructureParentsStmt ← sqlite.prepare "SELECT projection_fn, type FROM structure_parents WHERE module_name = ? AND position = ? ORDER BY sequence"
  let loadFieldArgsStmt ← sqlite.prepare "SELECT binder, is_implicit FROM structure_field_args WHERE module_name = ? AND position = ? AND field_sequence = ? ORDER BY arg_sequence"
  let loadFieldsStmt ← sqlite.prepare "SELECT sequence, proj_name, type, is_direct FROM structure_fields WHERE module_name = ? AND position = ? ORDER BY sequence"
  let lookupProjStmt ← sqlite.prepare "SELECT module_name, position FROM name_info WHERE name = ? LIMIT 1"
  let lookupRenderStmt ← sqlite.prepare "SELECT render FROM name_info WHERE module_name = ? AND position = ?"
  let loadStructCtorStmt ← sqlite.prepare "SELECT name, type, ctor_position FROM structure_constructors WHERE module_name = ? AND position = ?"
  let loadCtorPosStmt ← sqlite.prepare "SELECT c.position FROM constructors c WHERE c.module_name = ? AND c.type_position = ? ORDER BY c.position"
  let loadCtorInfoStmt ← sqlite.prepare "SELECT name, type, sorried, render FROM name_info WHERE module_name = ? AND position = ?"
  let readAxiomStmt ← sqlite.prepare "SELECT is_unsafe FROM axioms WHERE module_name = ? AND position = ?"
  let readOpaqueStmt ← sqlite.prepare "SELECT safety FROM opaques WHERE module_name = ? AND position = ?"
  let readDefinitionStmt ← sqlite.prepare "SELECT is_unsafe, hints, is_noncomputable FROM definitions WHERE module_name = ? AND position = ?"
  let readInstanceStmt ← sqlite.prepare "SELECT class_name FROM instances WHERE module_name = ? AND position = ?"
  let readInductiveStmt ← sqlite.prepare "SELECT is_unsafe FROM inductives WHERE module_name = ? AND position = ?"
  let readStructureStmt ← sqlite.prepare "SELECT is_class FROM structures WHERE module_name = ? AND position = ?"
  let readClassInductiveStmt ← sqlite.prepare "SELECT is_unsafe FROM class_inductives WHERE module_name = ? AND position = ?"
  let getModuleNamesStmt ← sqlite.prepare "SELECT name FROM modules ORDER BY name"
  let getModuleSourceUrlsStmt ← sqlite.prepare "SELECT name, source_url FROM modules WHERE source_url IS NOT NULL"
  let getModuleImportsStmt ← sqlite.prepare "SELECT imported FROM module_imports WHERE importer = ?"
  let buildNameInfoStmt ← sqlite.prepare "SELECT name, module_name FROM name_info"
  let buildInternalNamesStmt ← sqlite.prepare "SELECT name, target_module FROM internal_names"
  let loadModuleStmt ← sqlite.prepare "SELECT n.position, n.kind, n.name, n.type, n.sorried, n.render FROM name_info n WHERE n.module_name = ?"
  let loadModuleDocsStmt ← sqlite.prepare "SELECT m.position, m.text FROM markdown_docstrings m WHERE m.module_name = ? AND m.position NOT IN (SELECT position FROM name_info WHERE module_name = ?)"
  let loadTacticsStmt ← sqlite.prepare "SELECT internal_name, user_name, doc_string FROM tactics WHERE module_name = ?"
  let loadTacticTagsStmt ← sqlite.prepare "SELECT tag FROM tactic_tags WHERE module_name = ? AND internal_name = ?"
  pure {
    values, loadArgsStmt, loadAttrsStmt, readMdDocstringStmt, readVersoDocstringStmt,
    loadDeclRangeStmt, loadEqnsStmt, loadInstanceArgsStmt, loadStructureParentsStmt,
    loadFieldArgsStmt, loadFieldsStmt, lookupProjStmt, lookupRenderStmt,
    loadStructCtorStmt, loadCtorPosStmt, loadCtorInfoStmt,
    readAxiomStmt, readOpaqueStmt, readDefinitionStmt, readInstanceStmt,
    readInductiveStmt, readStructureStmt, readClassInductiveStmt,
    getModuleNamesStmt, getModuleSourceUrlsStmt, getModuleImportsStmt,
    buildNameInfoStmt, buildInternalNamesStmt,
    loadModuleStmt, loadModuleDocsStmt, loadTacticsStmt, loadTacticTagsStmt
  }

open Lean SQLite.Blob in
private def ReadStmts.loadArgs (s : ReadStmts) (moduleName : String) (position : Int64) : IO (Array Process.Arg) := withDbContext "read:declaration_args" do
  s.loadArgsStmt.bind 1 moduleName
  s.loadArgsStmt.bind 2 position
  let mut args := #[]
  while (← s.loadArgsStmt.step) do
    let binderBlob ← s.loadArgsStmt.columnBlob 0
    let binder ← readRenderedCode binderBlob
    let isImplicit := (← s.loadArgsStmt.columnInt64 1) != 0
    args := args.push { binder, implicit := isImplicit }
  done s.loadArgsStmt
  return args

open Lean SQLite.Blob in
private def ReadStmts.loadAttrs (s : ReadStmts) (moduleName : String) (position : Int64) : IO (Array String) := withDbContext "read:declaration_attrs" do
  s.loadAttrsStmt.bind 1 moduleName
  s.loadAttrsStmt.bind 2 position
  let mut attrs := #[]
  while (← s.loadAttrsStmt.step) do
    let attr ← s.loadAttrsStmt.columnText 0
    attrs := attrs.push attr
  done s.loadAttrsStmt
  return attrs

open Lean SQLite.Blob in
private def ReadStmts.loadDocstring (s : ReadStmts) (moduleName : String) (position : Int64) : IO (Option (String ⊕ VersoDocString)) := withDbContext "read:docstrings" do
  s.readMdDocstringStmt.bind 1 moduleName
  s.readMdDocstringStmt.bind 2 position
  if (← s.readMdDocstringStmt.step) then
    let text ← s.readMdDocstringStmt.columnText 0
    done s.readMdDocstringStmt
    return some (.inl text)
  done s.readMdDocstringStmt
  s.readVersoDocstringStmt.bind 1 moduleName
  s.readVersoDocstringStmt.bind 2 position
  if (← s.readVersoDocstringStmt.step) then
    let blob ← s.readVersoDocstringStmt.columnBlob 0
    done s.readVersoDocstringStmt
    have := versoDocStringFromBinary s.values
    match fromBinary blob with
    | .ok doc => return some (.inr doc)
    | .error e => throw <| IO.userError s!"Failed to deserialize VersoDocString: {e}"
  done s.readVersoDocstringStmt
  return none

open Lean SQLite.Blob in
private def ReadStmts.loadDeclarationRange (s : ReadStmts) (moduleName : String) (position : Int64) : IO (Option DeclarationRange) := withDbContext "read:declaration_ranges" do
  s.loadDeclRangeStmt.bind 1 moduleName
  s.loadDeclRangeStmt.bind 2 position
  if (← s.loadDeclRangeStmt.step) then
    let startLine := (← s.loadDeclRangeStmt.columnInt64 0).toNatClampNeg
    let startCol := (← s.loadDeclRangeStmt.columnInt64 1).toNatClampNeg
    let startUtf16 := (← s.loadDeclRangeStmt.columnInt64 2).toNatClampNeg
    let endLine := (← s.loadDeclRangeStmt.columnInt64 3).toNatClampNeg
    let endCol := (← s.loadDeclRangeStmt.columnInt64 4).toNatClampNeg
    let endUtf16 := (← s.loadDeclRangeStmt.columnInt64 5).toNatClampNeg
    done s.loadDeclRangeStmt
    return some {
      pos := ⟨startLine, startCol⟩
      charUtf16 := startUtf16
      endPos := ⟨endLine, endCol⟩
      endCharUtf16 := endUtf16
    }
  done s.loadDeclRangeStmt
  return none

open Lean SQLite.Blob in
private def ReadStmts.loadInfo (s : ReadStmts) (moduleName : String) (position : Int64) (name : Name) (typeBlob : ByteArray) (sorried : Bool) (render : Bool) : IO Process.Info := do
  let type ← readRenderedCode typeBlob
  let doc ← s.loadDocstring moduleName position
  let args ← s.loadArgs moduleName position
  let attrs ← s.loadAttrs moduleName position
  let some declRange ← s.loadDeclarationRange moduleName position
    | throw <| IO.userError s!"Missing declaration range for {name}"
  return { name, type, doc, args, declarationRange := declRange, attrs, sorried, render }

open Lean SQLite.Blob in
private def ReadStmts.loadEquations (s : ReadStmts) (moduleName : String) (position : Int64) : IO (Option (Array RenderedCode) × Bool) := withDbContext "read:definition_equations" do
  s.loadEqnsStmt.bind 1 moduleName
  s.loadEqnsStmt.bind 2 position
  if !(← s.loadEqnsStmt.step) then
    done s.loadEqnsStmt
    return (none, false)
  let mut eqns := #[]
  let mut wereOmitted := false
  let processRow : IO (Option RenderedCode) := do
    let colType ← s.loadEqnsStmt.columnType 0
    if colType == .null then
      return none
    else
      let blob ← s.loadEqnsStmt.columnBlob 0
      return some (← readRenderedCode blob)
  match (← processRow) with
  | some code => eqns := eqns.push code
  | none => wereOmitted := true
  while (← s.loadEqnsStmt.step) do
    match (← processRow) with
    | some code => eqns := eqns.push code
    | none => wereOmitted := true
  done s.loadEqnsStmt
  if eqns.isEmpty && !wereOmitted then
    return (none, false)
  return (some eqns, wereOmitted)

open Lean SQLite.Blob in
private def ReadStmts.loadInstanceArgs (s : ReadStmts) (moduleName : String) (position : Int64) : IO (Array Name) := do
  s.loadInstanceArgsStmt.bind 1 moduleName
  s.loadInstanceArgsStmt.bind 2 position
  let mut typeNames := #[]
  while (← s.loadInstanceArgsStmt.step) do
    let typeName := (← s.loadInstanceArgsStmt.columnText 0).toName
    typeNames := typeNames.push typeName
  done s.loadInstanceArgsStmt
  return typeNames

open Lean SQLite.Blob in
private def ReadStmts.loadStructureParents (s : ReadStmts) (moduleName : String) (position : Int64) : IO (Array Process.StructureParentInfo) := do
  s.loadStructureParentsStmt.bind 1 moduleName
  s.loadStructureParentsStmt.bind 2 position
  let mut parents := #[]
  while (← s.loadStructureParentsStmt.step) do
    let projFn := (← s.loadStructureParentsStmt.columnText 0).toName
    let typeBlob ← s.loadStructureParentsStmt.columnBlob 1
    let type ← readRenderedCode typeBlob
    parents := parents.push { projFn, type }
  done s.loadStructureParentsStmt
  return parents

open Lean SQLite.Blob in
private def ReadStmts.loadStructureFieldArgs (s : ReadStmts) (moduleName : String) (position : Int64) (fieldSeq : Int64) : IO (Array Process.Arg) := do
  s.loadFieldArgsStmt.bind 1 moduleName
  s.loadFieldArgsStmt.bind 2 position
  s.loadFieldArgsStmt.bind 3 fieldSeq
  let mut args := #[]
  while (← s.loadFieldArgsStmt.step) do
    let binderBlob ← s.loadFieldArgsStmt.columnBlob 0
    let binder ← readRenderedCode binderBlob
    let isImplicit := (← s.loadFieldArgsStmt.columnInt64 1) != 0
    args := args.push { binder, implicit := isImplicit }
  done s.loadFieldArgsStmt
  return args

open Lean SQLite.Blob in
private def ReadStmts.loadStructureFields (s : ReadStmts) (moduleName : String) (position : Int64) : IO (Array Process.FieldInfo) := do
  s.loadFieldsStmt.bind 1 moduleName
  s.loadFieldsStmt.bind 2 position
  let mut fields := #[]
  while (← s.loadFieldsStmt.step) do
    let fieldSeq ← s.loadFieldsStmt.columnInt64 0
    let name := (← s.loadFieldsStmt.columnText 1).toName
    let typeBlob ← s.loadFieldsStmt.columnBlob 2
    let type ← readRenderedCode typeBlob
    let isDirect := (← s.loadFieldsStmt.columnInt64 3) != 0
    s.lookupProjStmt.bind 1 name.toString
    let (doc, attrs, declRange, render) ← if (← s.lookupProjStmt.step) then do
      let projModName ← s.lookupProjStmt.columnText 0
      let projPos ← s.lookupProjStmt.columnInt64 1
      done s.lookupProjStmt
      let doc ← s.loadDocstring projModName projPos
      let attrs ← s.loadAttrs projModName projPos
      let declRange ← s.loadDeclarationRange projModName projPos
      let render ← do
        s.lookupRenderStmt.bind 1 projModName
        s.lookupRenderStmt.bind 2 projPos
        let r ← if (← s.lookupRenderStmt.step) then
          pure ((← s.lookupRenderStmt.columnInt64 0) != 0)
        else
          pure true
        done s.lookupRenderStmt
        pure r
      pure (doc, attrs, declRange, render)
    else do
      done s.lookupProjStmt
      pure (none, #[], none, true)
    let args ← s.loadStructureFieldArgs moduleName position fieldSeq
    fields := fields.push {
      name, type, doc, args, declarationRange := declRange.getD default,
      attrs, render, isDirect
    }
  done s.loadFieldsStmt
  return fields

open Lean SQLite.Blob in
private def ReadStmts.loadStructureConstructor (s : ReadStmts) (moduleName : String) (position : Int64) : IO (Option Process.NameInfo) := do
  s.loadStructCtorStmt.bind 1 moduleName
  s.loadStructCtorStmt.bind 2 position
  if (← s.loadStructCtorStmt.step) then
    let name := (← s.loadStructCtorStmt.columnText 0).toName
    let typeBlob ← s.loadStructCtorStmt.columnBlob 1
    let ctorPos ← s.loadStructCtorStmt.columnInt64 2
    done s.loadStructCtorStmt
    let type ← readRenderedCode typeBlob
    let doc ← s.loadDocstring moduleName ctorPos
    return some { name, type, doc }
  done s.loadStructCtorStmt
  return none

open Lean SQLite.Blob in
private def ReadStmts.loadConstructors (s : ReadStmts) (moduleName : String) (position : Int64) : IO (List Process.ConstructorInfo) := do
  s.loadCtorPosStmt.bind 1 moduleName
  s.loadCtorPosStmt.bind 2 position
  let mut ctorPositions := #[]
  while (← s.loadCtorPosStmt.step) do
    ctorPositions := ctorPositions.push (← s.loadCtorPosStmt.columnInt64 0)
  done s.loadCtorPosStmt
  let mut ctors := []
  for ctorPos in ctorPositions do
    s.loadCtorInfoStmt.bind 1 moduleName
    s.loadCtorInfoStmt.bind 2 ctorPos
    if (← s.loadCtorInfoStmt.step) then
      let name := (← s.loadCtorInfoStmt.columnText 0).toName
      let typeBlob ← s.loadCtorInfoStmt.columnBlob 1
      let sorried := (← s.loadCtorInfoStmt.columnInt64 2) != 0
      let render := (← s.loadCtorInfoStmt.columnInt64 3) != 0
      done s.loadCtorInfoStmt
      let info ← s.loadInfo moduleName ctorPos name typeBlob sorried render
      ctors := ctors ++ [info]
    else
      done s.loadCtorInfoStmt
  return ctors

open Lean SQLite.Blob in
private def ReadStmts.loadDocInfo (s : ReadStmts) (moduleName : String) (position : Int64) (kind : String)
    (name : Name) (typeBlob : ByteArray) (sorried : Bool) (render : Bool) : IO (Option Process.DocInfo) := do
  let info ← s.loadInfo moduleName position name typeBlob sorried render
  match kind with
  | "axiom" => readAxiom info
  | "theorem" => return some <| .theoremInfo { toInfo := info }
  | "opaque" => readOpaque info
  | "definition" => readDefinition info
  | "instance" => readInstance info
  | "inductive" => readInductive info
  | "structure" => readStructure .structureInfo info
  | "class" => readStructure .classInfo info
  | "class inductive" => readClassInductive info
  | "constructor" => return some <| .ctorInfo info
  | other =>
    IO.eprintln s!"warning: unknown declaration kind '{other}' for '{name}' in module '{moduleName}'; skipping"
    return none
where
  readAxiom (info : Process.Info) : IO (Option Process.DocInfo) := do
    s.readAxiomStmt.bind 1 moduleName
    s.readAxiomStmt.bind 2 position
    if (← s.readAxiomStmt.step) then
      let isUnsafe := (← s.readAxiomStmt.columnInt64 0) != 0
      done s.readAxiomStmt
      return some <| .axiomInfo { toInfo := info, isUnsafe }
    done s.readAxiomStmt
    return none
  readOpaque (info : Process.Info) : IO (Option Process.DocInfo) := do
    s.readOpaqueStmt.bind 1 moduleName
    s.readOpaqueStmt.bind 2 position
    if (← s.readOpaqueStmt.step) then
      let safetyStr ← s.readOpaqueStmt.columnText 0
      done s.readOpaqueStmt
      let safety := match safetyStr with
        | "unsafe" => .unsafe
        | "partial" => .partial
        | _ => .safe
      return some <| .opaqueInfo { toInfo := info, definitionSafety := safety }
    done s.readOpaqueStmt
    return none
  readDefinitionData : IO (Option (Bool × ReducibilityHints × Bool × Option (Array RenderedCode) × Bool)) := do
    s.readDefinitionStmt.bind 1 moduleName
    s.readDefinitionStmt.bind 2 position
    if (← s.readDefinitionStmt.step) then
      let isUnsafe := (← s.readDefinitionStmt.columnInt64 0) != 0
      let hintsStr ← s.readDefinitionStmt.columnText 1
      let isNonComputable := (← s.readDefinitionStmt.columnInt64 2) != 0
      done s.readDefinitionStmt
      let hints : ReducibilityHints := match hintsStr with
        | "opaque" => .opaque
        | "abbrev" => .abbrev
        | s => .regular (s.toNat?.getD 0 |>.toUInt32)
      let (equations, equationsWereOmitted) ← s.loadEquations moduleName position
      return some (isUnsafe, hints, isNonComputable, equations, equationsWereOmitted)
    done s.readDefinitionStmt
    return none
  readDefinition (info : Process.Info) : IO (Option Process.DocInfo) := do
    let some (isUnsafe, hints, isNonComputable, equations, equationsWereOmitted) ← readDefinitionData
      | return none
    return some <| .definitionInfo { toInfo := info, isUnsafe, hints, equations, equationsWereOmitted, isNonComputable }
  readInstance (info : Process.Info) : IO (Option Process.DocInfo) := do
    s.readInstanceStmt.bind 1 moduleName
    s.readInstanceStmt.bind 2 position
    if (← s.readInstanceStmt.step) then
      let className := (← s.readInstanceStmt.columnText 0).toName
      done s.readInstanceStmt
      let some (isUnsafe, hints, isNonComputable, equations, equationsWereOmitted) ← readDefinitionData
        | return none
      let typeNames ← s.loadInstanceArgs moduleName position
      return some <| .instanceInfo { toInfo := info, isUnsafe, hints, equations, equationsWereOmitted, isNonComputable, className, typeNames }
    else
      done s.readInstanceStmt
    return none
  readInductive (info : Process.Info) : IO (Option Process.DocInfo) := do
    s.readInductiveStmt.bind 1 moduleName
    s.readInductiveStmt.bind 2 position
    if (← s.readInductiveStmt.step) then
      let isUnsafe := (← s.readInductiveStmt.columnInt64 0) != 0
      done s.readInductiveStmt
      let ctors ← s.loadConstructors moduleName position
      return some <| .inductiveInfo { toInfo := info, isUnsafe, ctors }
    done s.readInductiveStmt
    return none
  readStructure (mk : Process.StructureInfo → Process.DocInfo) (info : Process.Info) : IO (Option Process.DocInfo) := do
    s.readStructureStmt.bind 1 moduleName
    s.readStructureStmt.bind 2 position
    if (← s.readStructureStmt.step) then
      done s.readStructureStmt
      let parents ← s.loadStructureParents moduleName position
      let fieldInfo ← s.loadStructureFields moduleName position
      let some ctor ← s.loadStructureConstructor moduleName position
        | return none
      return some <| mk { toInfo := info, fieldInfo, parents, ctor }
    done s.readStructureStmt
    return none
  readClassInductive (info : Process.Info) : IO (Option Process.DocInfo) := do
    s.readClassInductiveStmt.bind 1 moduleName
    s.readClassInductiveStmt.bind 2 position
    if (← s.readClassInductiveStmt.step) then
      let isUnsafe := (← s.readClassInductiveStmt.columnInt64 0) != 0
      done s.readClassInductiveStmt
      let ctors ← s.loadConstructors moduleName position
      return some <| .classInductiveInfo { toInfo := info, isUnsafe, ctors }
    done s.readClassInductiveStmt
    return none

open Lean SQLite.Blob in
private def ReadStmts.getModuleNames (s : ReadStmts) : IO (Array Name) := withDbContext "read:modules:names" do
  let mut names := #[]
  while (← s.getModuleNamesStmt.step) do
    let name := (← s.getModuleNamesStmt.columnText 0).toName
    names := names.push name
  done s.getModuleNamesStmt
  return names

open Lean SQLite.Blob in
private def ReadStmts.getModuleSourceUrls (s : ReadStmts) : IO (Std.HashMap Name String) := withDbContext "read:modules:source_urls" do
  let mut urls : Std.HashMap Name String := {}
  while (← s.getModuleSourceUrlsStmt.step) do
    let name := (← s.getModuleSourceUrlsStmt.columnText 0).toName
    let url ← s.getModuleSourceUrlsStmt.columnText 1
    urls := urls.insert name url
  done s.getModuleSourceUrlsStmt
  return urls

open Lean SQLite.Blob in
private def ReadStmts.getModuleImports (s : ReadStmts) (moduleName : Name) : IO (Array Name) := withDbContext "read:module_imports" do
  s.getModuleImportsStmt.bind 1 moduleName.toString
  let mut imports := #[]
  while (← s.getModuleImportsStmt.step) do
    let name := (← s.getModuleImportsStmt.columnText 0).toName
    imports := imports.push name
  done s.getModuleImportsStmt
  return imports

open Lean SQLite.Blob in
private def ReadStmts.buildName2ModIdx (s : ReadStmts) (moduleNames : Array Name) : IO (Std.HashMap Name ModuleIdx) := do
  let modNameToIdx : Std.HashMap Name ModuleIdx :=
    moduleNames.foldl (init := {}) fun acc modName =>
      acc.insert modName acc.size
  let mut result : Std.HashMap Name ModuleIdx := {}
  while (← s.buildNameInfoStmt.step) do
    let name := (← s.buildNameInfoStmt.columnText 0).toName
    let moduleName := (← s.buildNameInfoStmt.columnText 1).toName
    if let some idx := modNameToIdx[moduleName]? then
      result := result.insert name idx
  done s.buildNameInfoStmt
  while (← s.buildInternalNamesStmt.step) do
    let name := (← s.buildInternalNamesStmt.columnText 0).toName
    if !result.contains name then
      let targetModule := (← s.buildInternalNamesStmt.columnText 1).toName
      if let some idx := modNameToIdx[targetModule]? then
        result := result.insert name idx
  done s.buildInternalNamesStmt
  return result

open Lean SQLite.Blob in
private def ReadStmts.loadModule (s : ReadStmts) (moduleName : Name) : IO Process.Module := do
  let modNameStr := moduleName.toString
  let imports ← s.getModuleImports moduleName
  s.loadModuleStmt.bind 1 modNameStr
  let mut members : Array (Int64 × Process.ModuleMember) := #[]
  while (← s.loadModuleStmt.step) do
    let position ← s.loadModuleStmt.columnInt64 0
    let kind ← s.loadModuleStmt.columnText 1
    let name := (← s.loadModuleStmt.columnText 2).toName
    let typeBlob ← s.loadModuleStmt.columnBlob 3
    let sorried := (← s.loadModuleStmt.columnInt64 4) != 0
    let render := (← s.loadModuleStmt.columnInt64 5) != 0
    match (← s.loadDocInfo modNameStr position kind name typeBlob sorried render) with
    | some docInfo => members := members.push (position, .docInfo docInfo)
    | none => IO.eprintln s!"warning: failed to load declaration '{name}' (kind '{kind}') at position {position} in module '{modNameStr}'; skipping"
  done s.loadModuleStmt
  s.loadModuleDocsStmt.bind 1 modNameStr
  s.loadModuleDocsStmt.bind 2 modNameStr
  while (← s.loadModuleDocsStmt.step) do
    let position ← s.loadModuleDocsStmt.columnInt64 0
    let doc ← s.loadModuleDocsStmt.columnText 1
    match (← s.loadDeclarationRange modNameStr position) with
    | some declRange => members := members.push (position, .modDoc { doc, declarationRange := declRange })
    | none => IO.eprintln s!"warning: missing declaration range for module docstring at position {position} in module '{modNameStr}'; skipping"
  done s.loadModuleDocsStmt
  let sortedMembers := members.qsort fun (pos1, m1) (pos2, m2) =>
    let r1 := m1.getDeclarationRange.pos
    let r2 := m2.getDeclarationRange.pos
    if Position.lt r1 r2 then true
    else if Position.lt r2 r1 then false
    else pos1 < pos2
  s.loadTacticsStmt.bind 1 modNameStr
  let mut tactics : Array (Process.TacticInfo Process.MarkdownDocstring) := #[]
  while (← s.loadTacticsStmt.step) do
    let internalName := (← s.loadTacticsStmt.columnText 0).toName
    let userName ← s.loadTacticsStmt.columnText 1
    let docString ← s.loadTacticsStmt.columnText 2
    s.loadTacticTagsStmt.bind 1 modNameStr
    s.loadTacticTagsStmt.bind 2 internalName.toString
    let mut tags : Array Name := #[]
    while (← s.loadTacticTagsStmt.step) do
      tags := tags.push (← s.loadTacticTagsStmt.columnText 0).toName
    done s.loadTacticTagsStmt
    tactics := tactics.push { internalName, userName, tags, docString, definingModule := moduleName }
  done s.loadTacticsStmt
  return { name := moduleName, members := sortedMembers.map (·.2), imports, tactics }

def mkReadOps (sqlite : SQLite) (values : DocstringValues) : IO ReadOps := do
  let s ← ReadStmts.prepare sqlite values
  pure {
    getModuleNames := s.getModuleNames
    getModuleSourceUrls := s.getModuleSourceUrls
    getModuleImports := s.getModuleImports
    buildName2ModIdx := s.buildName2ModIdx
    loadModule := s.loadModule
  }

end DocGen4.DB
