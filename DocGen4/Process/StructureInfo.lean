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
def withFields (info : InductiveVal) (k : Array (Name × Expr) → MetaM α) : MetaM α := do
  let structName := info.name
  let us := info.levelParams.map mkLevelParam
  forallTelescopeReducing info.type fun params _ =>
  withLocalDeclD `self (mkAppN (mkConst structName us) params) fun s => do
    let mut info := #[]
    for fieldName in getStructureFieldsFlattened (← getEnv) structName (includeSubobjectFields := false) do
      let proj ← mkProjection s fieldName
      info := info.push (fieldName, (← inferType proj))
    k info

def getFieldTypes (parents : Array Name) (v : InductiveVal) : MetaM (Array FieldInfo) := do
  let env ← getEnv
  withFields v fun fields =>
    fields.foldlM (init := #[]) (fun acc (name, type) => do
      let some structForField := findField? env v.name name | unreachable!
      -- We can't simply do `structForField == v.name` since the field might be from a parent that overlapped with another.
      let isDirect := structForField == v.name && !parents.any fun parent => (getFieldInfo? env parent name).isSome
      let some fi := getFieldInfo? env structForField name | unreachable!
      return acc.push { ← NameInfo.ofTypedName fi.projFn type with isDirect })

def StructureInfo.ofInductiveVal (v : InductiveVal) : MetaM StructureInfo := do
  let info ← Info.ofConstantVal v.toConstantVal
  let env ← getEnv
  let parents := (getStructureParentInfo env v.name).map fun parent => parent.structName
  let ctorVal := getStructureCtor env v.name
  let ctor ← NameInfo.ofTypedName ctorVal.name ctorVal.type
  return {
    toInfo := info,
    fieldInfo := ← getFieldTypes parents v,
    parents,
    ctor
  }

end DocGen4.Process
