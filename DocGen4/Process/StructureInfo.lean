/-
Copyright (c) 2022 Henrik Böving. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Henrik Böving
-/
import Lean

import DocGen4.Process.Base
import DocGen4.Process.NameInfo

namespace DocGen4.Process

open Lean Meta

/--
  Execute `k` with an array containing pairs `(fieldName, fieldType)`.
  `k` is executed in an updated local context which contains local declarations for the `structName` parameters.
-/
def withFields (info : InductiveVal) (k : Array (Name × Expr) → MetaM α) (includeSubobjectFields : Bool := false) : MetaM α := do
  let structName := info.name
  let us := info.levelParams.map mkLevelParam
  forallTelescopeReducing info.type fun params _ =>
  withLocalDeclD `self (mkAppN (mkConst structName us) params) fun s => do
    let mut info := #[]
    for fieldName in getStructureFieldsFlattened (← getEnv) structName includeSubobjectFields do
      let proj ← mkProjection s fieldName
      info := info.push (fieldName, (← inferType proj))
    k info

def getFieldTypes (v : InductiveVal) : MetaM (Array NameInfo) := do
  withFields v fun fields =>
    fields.foldlM (init := #[]) (fun acc (name, type) => do return acc.push (← NameInfo.ofTypedName (v.name.append name) type))

def StructureInfo.ofInductiveVal (v : InductiveVal) : MetaM StructureInfo := do
  let info ← Info.ofConstantVal v.toConstantVal
  let env ← getEnv
  let parents ← getAllParentStructures v.name
  let ctorVal := getStructureCtor env v.name
  let ctor ← NameInfo.ofTypedName ctorVal.name ctorVal.type
  match getStructureInfo? env v.name with
  | some i =>
    if i.fieldNames.size - parents.size > 0 then
      return {
        toInfo := info,
        fieldInfo := ← getFieldTypes v,
        parents,
        ctor
      }
    else
      return {
        toInfo := info,
        fieldInfo := #[],
        parents,
        ctor
      }
  | none => panic! s!"{v.name} is not a structure"

end DocGen4.Process
