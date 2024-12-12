/-
Copyright (c) 2022 Henrik Böving. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Henrik Böving
-/
import Lean

import DocGen4.Process.Base
import DocGen4.Process.NameInfo
import DocGen4.Process.DefinitionInfo

namespace DocGen4.Process

open Lean Meta

def getInstanceTypes (typ : Expr) : MetaM (Array Name) := do
  let (_, _, tail) ← forallMetaTelescopeReducing typ
  let args := tail.getAppArgs
  let (_, bis, _) ← forallMetaTelescopeReducing (← inferType tail.getAppFn)
  let (_, names) ← (bis.zip args).mapM findName |>.run .empty
  return names
where
  findName : BinderInfo × Expr → StateRefT (Array Name) MetaM Unit
    | (.default, .sort .zero) => modify (·.push `_builtin_prop)
    | (.default, .sort (.succ _)) => modify (·.push `_builtin_typeu)
    | (.default, .sort _) => modify (·.push `_builtin_sortu)
    | (.default, e) =>
      match e.getAppFn with
      | .const name .. => modify (·.push name)
      | _ => return ()
    | _ => return ()

def getInstPriority (name : Name) : MetaM (Option Nat) := do
  let instances := instanceExtension.getState (← getEnv)
  let some instEntry := instances.instanceNames.find? name
    | panic! s!"instance not in instance extension"
  let priority := instEntry.priority
  if priority == 1000 then
    return none
  else
    return some priority

private def InstanceInfo.ofDefinitionInfo (info : DefinitionInfo) (type : Expr) :
    MetaM InstanceInfo  := do
  let mut info := info

  if let some priority ← getInstPriority info.name then
    info := { info with attrs := info.attrs.push s!"instance {priority}" }

  let some className ← isClass? type | panic! s!"isClass? on {info.name} returned none"
  if let some instAttr ← getDefaultInstance info.name className then
    info := { info with attrs := info.attrs.push instAttr }

  let typeNames ← getInstanceTypes type

  return {
    toDefinitionInfo := info,
    className,
    typeNames,
  }

def InstanceInfo.ofDefinitionVal (v : DefinitionVal) : MetaM InstanceInfo := do
  let info ← DefinitionInfo.ofDefinitionVal v
  InstanceInfo.ofDefinitionInfo info v.type

def InstanceInfo.ofTheoremVal (v : TheoremVal) : MetaM InstanceInfo := do
  let info ← Info.ofConstantVal v.toConstantVal
  /-
  This is a bit of a shortcut but it avoids having to duplicate the instance infrastructure for
  `Prop` and non-`Prop` valued instances for now. If we run into issues with this later on we
  can still easily get the hack out as it is limited to this function.
  -/
  let info : DefinitionInfo := {
    toInfo := info,
    -- theorems can't be unsafe, only definitions
    isUnsafe := false,
    -- theorems can't have reducibility hints so just put default
    hints := .regular 0,
    -- theorems don't have equations of interest
    equations := none,
    -- theorems can't be noncomputable, only definitions
    isNonComputable := false
  }

  InstanceInfo.ofDefinitionInfo info v.type

end DocGen4.Process
