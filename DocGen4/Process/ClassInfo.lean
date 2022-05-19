
/-
Copyright (c) 2022 Henrik Böving. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Henrik Böving
-/
import Lean

import DocGen4.Process.Base
import DocGen4.Process.NameInfo
import DocGen4.Process.StructureInfo
import DocGen4.Process.InductiveInfo

namespace DocGen4.Process

open Lean Meta 

def getInstances (className : Name) : MetaM (Array Name) := do
  let fn ← mkConstWithFreshMVarLevels className
  let (xs, _, _) ← forallMetaTelescopeReducing (← inferType fn)
  let insts ← SynthInstance.getInstances (mkAppN fn xs)
  pure $ insts.map Expr.constName!

def ClassInfo.ofInductiveVal (v : InductiveVal) : MetaM ClassInfo := do
  let sinfo ← StructureInfo.ofInductiveVal v
  pure $ ClassInfo.mk sinfo (←getInstances v.name)

def ClassInductiveInfo.ofInductiveVal (v : InductiveVal) : MetaM ClassInductiveInfo := do
  let info ← InductiveInfo.ofInductiveVal v
  pure $ ClassInductiveInfo.mk info (←getInstances v.name)

end DocGen4.Process
