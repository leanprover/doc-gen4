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
  Execute `k` with an array containing pairs `(parentName, projFn, parentType)`
  and an array containing pairs `(fieldName, fieldType)`.
  `k` is executed in an updated local context which contains local declarations for the `structName` parameters.
-/
def withFields (info : InductiveVal) (k : Array (Name × Name × Expr) → Array (Name × Expr) → MetaM α) : MetaM α := do
  let structName := info.name
  let us := info.levelParams.map mkLevelParam
  forallTelescopeReducing info.type fun params _ =>
  withLocalDeclD `self (mkAppN (mkConst structName us) params) fun s => do
    let mut parents := #[]
    for parent in getStructureParentInfo (← getEnv) structName do
      let proj := mkApp (mkAppN (mkConst parent.projFn us) params) s
      parents := parents.push (parent.structName, parent.projFn, ← inferType proj)
    let mut fields := #[]
    for fieldName in getStructureFieldsFlattened (← getEnv) structName (includeSubobjectFields := false) do
      let proj ← mkProjection s fieldName
      fields := fields.push (fieldName, (← inferType proj))
    k parents fields

def getFieldTypes (v : InductiveVal) : MetaM (Array StructureParentInfo × Array FieldInfo) := do
  let env ← getEnv
  withFields v fun parents fields => do
    let mut parentInfo : Array StructureParentInfo := #[]
    let mut fieldInfo : Array FieldInfo := #[]
    for (_, projFn, type) in parents do
      parentInfo := parentInfo.push { projFn, type := ← prettyPrintTerm type }
    for (name, type) in fields do
      let some structForField := findField? env v.name name | unreachable!
      -- We can't simply do `structForField == v.name` since the field might be from a parent that overlapped with another.
      let isDirect := structForField == v.name && !parents.any fun (parent, _) => (getFieldInfo? env parent name).isSome
      let some fi := getFieldInfo? env structForField name | unreachable!
      fieldInfo := fieldInfo.push { ← NameInfo.ofTypedName fi.projFn type with isDirect }
    return (parentInfo, fieldInfo)

def StructureInfo.ofInductiveVal (v : InductiveVal) : MetaM StructureInfo := do
  let info ← Info.ofConstantVal v.toConstantVal
  let env ← getEnv
  let ctorVal := getStructureCtor env v.name
  let ctor ← NameInfo.ofTypedName ctorVal.name ctorVal.type
  let (parents, fieldInfo) ← getFieldTypes v
  return {
    toInfo := info,
    fieldInfo,
    parents,
    ctor
  }

end DocGen4.Process
