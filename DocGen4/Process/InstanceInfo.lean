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
  let (_, names) ← args.mapM (Expr.forEach · findName) |>.run .empty
  return names
where
  findName : Expr → StateRefT (Array Name) MetaM Unit
    | .const name _ => modify (·.push name)
    | .sort .zero => modify (·.push "_builtin_prop")
    | .sort (.succ _) => modify (·.push "_builtin_typeu")
    | .sort _ => modify (·.push "_builtin_sortu")
    | _ => return ()

def InstanceInfo.ofDefinitionVal (v : DefinitionVal) : MetaM InstanceInfo := do
  let mut info ← DefinitionInfo.ofDefinitionVal v
  let some className ← isClass? v.type | unreachable!
  if let some instAttr ← getDefaultInstance v.name className then
    info := { info with attrs := info.attrs.push instAttr }
  let typeNames ← getInstanceTypes v.type
  return {
    toDefinitionInfo := info,
    className,
    typeNames
  }

end DocGen4.Process
