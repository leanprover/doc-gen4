/-
Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Kim Morrison
-/
import DocGen4.DB
import DocGen4.Load

/-!
# External Modules

With interproject linking enabled (`DOCGEN_LOCAL_MODULE_ROOTS`), pages are generated only for the
project's own modules, and references to declarations in other modules link to the dependency
documentation site through its `/find` resolver. Such a link needs to know only that the name is an
external declaration, so running the full per-module analysis (`single`) on every external module is
wasted work, and for a project built on Mathlib it dominates the build.

Instead, Lake skips the analysis of external modules, and `recordExternals` loads the environment of
the root modules once and records every external module, its imports, and a name-only `name_info`
row for each of its documentable declarations and for the recursors of its inductive types. Projection
functions also get their docstring, because a local structure extending an external one shows the
docstrings of its inherited fields, and the module's tactics are recorded in full, because the
tactics page lists every tactic available to the project.
-/

namespace DocGen4

open Lean Meta DB

/-- The information recorded for one external declaration. -/
structure ExternalDecl where
  name : Name
  declarationRange : DeclarationRange
  /-- The docstring, recorded only for projection functions. -/
  doc? : Option String

/--
Reads the local module roots from `DOCGEN_LOCAL_MODULE_ROOTS` (comma-separated). Empty when the
variable is unset, which means that every module is local.
-/
def getLocalModuleRoots : IO (Array Name) := do
  match ← IO.getEnv "DOCGEN_LOCAL_MODULE_ROOTS" with
  | some s =>
    pure <| s.splitOn "," |>.map (·.trimAscii.copy) |>.filter (! ·.isEmpty) |>.map String.toName |>.toArray
  | none => pure #[]

/-- What is recorded about the external modules, indexed like `env.header.moduleNames`. -/
structure ExternalInfo where
  decls : Array (Array ExternalDecl)
  tactics : Array (Array (Process.TacticInfo Process.MarkdownDocstring))

/--
The recursors that a full analysis links to an inductive type (see `saveRecursors` in
`updateModuleDb`), so that references to them keep linking to themselves.
-/
def recursorNames (env : Environment) (indName : Name) : Array Name := Id.run do
  let mut names := #[mkRecName indName]
  for auxName in [mkCasesOnName indName, mkRecOnName indName, mkBRecOnName indName] do
    if env.contains auxName then
      names := names.push auxName
  if indName == `Eq || indName == `HEq then
    names := names ++ #[indName ++ `ndrec, indName ++ `ndrecOn]
  return names

/-- Collects the declarations and tactics of the modules satisfying `isExternal`. -/
def collectExternals (isExternal : Name → Bool) : MetaM ExternalInfo := do
  let env ← getEnv
  -- Arrays indexed by module rather than a map to arrays: `Array.modify` updates the inner array in
  -- place, whereas updating a map's value copies it, which is quadratic for a large module.
  let mut decls : Array (Array ExternalDecl) := .replicate env.header.moduleNames.size #[]
  for (name, cinfo) in env.constants do
    let some modIdx := env.getModuleIdxFor? name | continue
    let mod := env.header.moduleNames[modIdx]!
    if !isExternal mod || isPrivateName name then continue
    if ← Process.DocInfo.isBlackListed name then continue
    let some ranges ← findDeclarationRanges? name | continue
    let doc? ← if ← Process.DocInfo.isProjFn name then findDocString? env name else pure none
    let mut new := #[{ name, declarationRange := ranges.range, doc? : ExternalDecl }]
    if cinfo matches .inductInfo _ then
      new := new ++ (recursorNames env name).map ({ name := ·, declarationRange := ranges.range, doc? := none })
    decls := decls.modify modIdx (· ++ new)
  let mut tactics := .replicate env.header.moduleNames.size #[]
  for doc in ← Elab.Tactic.Doc.allTacticDocs do
    let some modIdx := env.getModuleIdxFor? doc.internalName | continue
    let mod := env.header.moduleNames[modIdx]!
    if !isExternal mod then continue
    -- `findDocString?` already renders a tactic's extensions into `docString`, so, unlike
    -- `collectTactics`, do not append `extensionDocs` again. Since the environment is the whole
    -- project's, these are all the extensions the project sees, which can be more than the
    -- defining module had in scope.
    let info : Process.TacticInfo Process.MarkdownDocstring := {
      doc with
      docString := doc.docString.getD "This tactic has no documentation."
      tags := doc.tags.toArray
      definingModule := mod
    }
    tactics := tactics.modify modIdx (·.push info)
  return { decls, tactics }

/--
Records the external modules in the environment of `roots`, and their declarations, in the database.
Each external module is replaced as a whole, so rerunning this after a dependency changes leaves no
stale entries for the modules that remain.
-/
def recordExternals (values : DocstringValues) (roots localRoots : Array Name)
    (buildDir : System.FilePath) (dbFile : String) : IO Unit := do
  initSearchPath (← findSysroot)
  let env ← envOfImports roots
  let isExternal (mod : Name) := !localRoots.contains mod.getRoot
  let config := {
    maxHeartbeats := 0,
    options := Options.empty,
    fileName := default,
    fileMap := default,
  }
  let info ← Prod.fst <$> (collectExternals isExternal).toIO config { env := env } {} {}
  let db ← ensureWriteDb values (buildDir / dbFile)
  db.sqlite.transaction (mode := .immediate) do
    for h : i in 0...env.header.moduleNames.size do
      let mod := env.header.moduleNames[i]
      if !isExternal mod then continue
      let modStr := mod.toString
      db.deleteModule modStr
      db.saveModule modStr none
      for imported in env.header.moduleData[i]!.imports do
        db.saveImport modStr imported.module
      let mut pos : Int64 := 0
      for decl in info.decls[i]! do
        db.saveNameOnly modStr pos "external" decl.name RenderedCode.empty decl.declarationRange
        if let some doc := decl.doc? then
          db.saveMarkdownDocstring modStr pos doc
        pos := pos + 1
      for tactic in info.tactics[i]! do
        db.saveTactic modStr tactic

end DocGen4
