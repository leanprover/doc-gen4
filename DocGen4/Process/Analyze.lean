/-
Copyright (c) 2022 Henrik Böving. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Henrik Böving
-/

import Lean.Meta.Basic
import Std.Data.HashMap
import Std.Data.HashSet

import DocGen4.Process.Base
import DocGen4.Process.Hierarchy
import DocGen4.Process.DocInfo

namespace DocGen4.Process

open Lean Meta

/--
Member of a module, either a declaration or some module doc string.
-/
inductive ModuleMember where
| docInfo (info : DocInfo) : ModuleMember
| modDoc (doc : ModuleDoc) : ModuleMember
deriving Inhabited

/--
A Lean module.
-/
structure Module where
  /--
  Name of the module.
  -/
  name : Name
  /--
  All members of the module, sorted according to their line numbers.
  -/
  members : Array ModuleMember
  imports : Array Name
  deriving Inhabited

/--
The result of running a full doc-gen analysis on a project.
-/
structure AnalyzerResult where
  /--
  The map from module names to indices of the `moduleNames` array.
  -/
  name2ModIdx : Std.HashMap Name ModuleIdx
  /--
  The list of all modules, accessible nicely via `name2ModIdx`.
  -/
  moduleNames : Array Name
  /--
  A map from module names to information about these modules.
  -/
  moduleInfo : Std.HashMap Name Module
  deriving Inhabited

namespace ModuleMember

def getDeclarationRange : ModuleMember → DeclarationRange
| docInfo i => i.getDeclarationRange
| modDoc i => i.declarationRange

/--
An order for module members, based on their declaration range.
-/
def order (l r : ModuleMember) : Bool :=
  Position.lt l.getDeclarationRange.pos r.getDeclarationRange.pos

def getName : ModuleMember → Name
| docInfo i => i.getName
| modDoc _ => Name.anonymous

def getDocString : ModuleMember → Option String
| docInfo i => i.getDocString
| modDoc i => i.doc

def shouldRender : ModuleMember → Bool
| docInfo i => i.shouldRender
| modDoc _ => true

end ModuleMember

inductive AnalyzeTask where
| analyzePrefixModules (topLevel : Name) : AnalyzeTask
| analyzeConcreteModules (modules : Array Name) : AnalyzeTask

def AnalyzeTask.getLoad (task : AnalyzeTask) : Array Name :=
  match task with
  | .analyzePrefixModules topLevel => #[topLevel]
  | .analyzeConcreteModules modules => modules

def getAllModuleDocs (relevantModules : Array Name) : MetaM (Std.HashMap Name Module) := do
  let env ← getEnv
  let mut res := Std.HashMap.emptyWithCapacity relevantModules.size
  for module in relevantModules do
    let modDocs := getModuleDoc? env module |>.getD #[] |>.map .modDoc
    let some modIdx := env.getModuleIdx? module | unreachable!
    let moduleData := env.header.moduleData[modIdx]!
    let imports := moduleData.imports.map Import.module
    res := res.insert module <| Module.mk module modDocs imports
  return res

def mkOptions : IO DocGenOptions := do
  match ← IO.getEnv "DISABLE_EQUATIONS" with
  | some "1" => return ⟨false⟩
  | _ => return {}

/--
Run the doc-gen analysis on all modules that are loaded into the `Environment`
of this `MetaM` run and mentioned by the `AnalyzeTask`.
-/
def process (task : AnalyzeTask) : MetaM (AnalyzerResult × Hierarchy) := do
  let env ← getEnv
  let relevantModules :=
    match task with
    | .analyzePrefixModules topLevel =>
      let modules := env.header.moduleNames.filter (topLevel.isPrefixOf ·)
      Std.HashSet.insertMany (Std.HashSet.emptyWithCapacity modules.size) modules
    | .analyzeConcreteModules modules =>
      Std.HashSet.insertMany (Std.HashSet.emptyWithCapacity modules.size) modules
  let allModules := env.header.moduleNames

  let mut res ← getAllModuleDocs relevantModules.toArray

  let options ← liftM mkOptions

  for (name, cinfo) in env.constants do
    let some modidx := env.getModuleIdxFor? name | unreachable!
    let moduleName := env.allImportedModuleNames[modidx]!
    if !relevantModules.contains moduleName then
      continue

    res ← tryCatchRuntimeEx
      (do
        let config := {
          maxHeartbeats := 5000000,
          options := ← getOptions,
          fileName := ← getFileName,
          fileMap := ← getFileMap,
        }
        let analysis ← Prod.fst <$> ((DocInfo.ofConstant (name, cinfo)).run options).toIO config { env := env } {} {}
        if let some dinfo := analysis then
          let moduleName := env.allImportedModuleNames[modidx]!
          let module := res[moduleName]!
          return res.insert moduleName {module with members := module.members.push (ModuleMember.docInfo dinfo)}
        else
          return res
      )
      (fun e => do
        if let some pos := e.getRef.getPos? then
          let pos := (← getFileMap).toPosition pos
          IO.println s!"WARNING: Failed to obtain information in file: {pos}, for: {name}, {← e.toMessageData.toString}"
        else
          IO.println s!"WARNING: Failed to obtain information for: {name}: {← e.toMessageData.toString}"
        return res
      )

  -- TODO: This could probably be faster if we did sorted insert above instead
  for (moduleName, module) in res.toArray do
    res := res.insert moduleName {module with members := module.members.qsort ModuleMember.order}

  let hierarchy := Hierarchy.fromArray allModules
  let analysis := {
    name2ModIdx := env.const2ModIdx,
    moduleNames := allModules,
    moduleInfo := res,
  }
  return (analysis, hierarchy)

def filterDocInfo (ms : Array ModuleMember) : Array DocInfo :=
  ms.filterMap filter
  where
    filter : ModuleMember → Option DocInfo
    | ModuleMember.docInfo i => some i
    | _ => none

end DocGen4.Process
