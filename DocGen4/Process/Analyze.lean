/-
Copyright (c) 2022 Henrik Böving. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Henrik Böving
-/

import Lean
import Std.Data.HashMap

import DocGen4.Process.Base
import DocGen4.Process.Hierarchy
import DocGen4.Process.DocInfo

namespace DocGen4.Process

open Lean Meta Std

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
  deriving Inhabited

/--
The result of running a full doc-gen analysis on a project.
-/
structure AnalyzerResult where
  /--
  The map from module names to indices of the `moduleNames` array.
  -/
  name2ModIdx : HashMap Name ModuleIdx
  /--
  The list of all modules, accessible nicely via `name2ModIdx`.
  -/
  moduleNames : Array Name
  /--
  A map from module names to information about these modules.
  -/
  moduleInfo : HashMap Name Module
  /--
  The module hierarchy as a tree structure.
  -/
  hierarchy : Hierarchy
  /--
  An adjacency matrix for the import relation between modules, indexed
  my the values in `name2ModIdx`.
  -/
  importAdj : Array (Array Bool)
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

end ModuleMember

/--
Run the doc-gen analysis on all modules that are loaded into the `Environment`
of this `MetaM` run.
-/
def process : MetaM AnalyzerResult := do
  let env ← getEnv
  let mut res := mkHashMap env.header.moduleNames.size
  for module in env.header.moduleNames do
    let modDocs := match getModuleDoc? env module with
    | none => #[]
    | some ds => ds
    |>.map (λ doc => ModuleMember.modDoc doc)
    res := res.insert module (Module.mk module modDocs)

  for cinfo in env.constants.toList do
    try
      let config := {
        maxHeartbeats := 5000000,
        options := ←getOptions,
        fileName := ←getFileName,
        fileMap := ←getFileMap
      }
      let analysis := Prod.fst <$> Meta.MetaM.toIO (DocInfo.ofConstant cinfo) config { env := env} {} {}
      if let some dinfo ← analysis then
        let some modidx := env.getModuleIdxFor? dinfo.getName | unreachable!
        let moduleName := env.allImportedModuleNames.get! modidx
        let module := res.find! moduleName
        res := res.insert moduleName {module with members := module.members.push (ModuleMember.docInfo dinfo)}
    catch e =>
      IO.println s!"WARNING: Failed to obtain information for: {cinfo.fst}: {←e.toMessageData.toString}"

  -- TODO: This is definitely not the most efficient way to store this data
  let mut adj := Array.mkArray res.size (Array.mkArray res.size false)
  -- TODO: This could probably be faster if we did an insertion sort above instead
  for (moduleName, module) in res.toArray do
    res := res.insert moduleName {module with members := module.members.qsort ModuleMember.order}
    let some modIdx := env.getModuleIdx? moduleName | unreachable!
    let moduleData := env.header.moduleData.get! modIdx
    for imp in moduleData.imports do
      let some importIdx := env.getModuleIdx? imp.module | unreachable!
      adj := adj.set! modIdx (adj.get! modIdx |>.set! importIdx true)

  pure {
    name2ModIdx := env.const2ModIdx,
    moduleNames := env.header.moduleNames,
    moduleInfo := res,
    hierarchy := Hierarchy.fromArray env.header.moduleNames,
    importAdj := adj
  }

def filterMapDocInfo (ms : Array ModuleMember) : Array DocInfo :=
  ms.filterMap filter
  where
    filter : ModuleMember → Option DocInfo
    | ModuleMember.docInfo i => some i
    | _ => none

end DocGen4.Process
