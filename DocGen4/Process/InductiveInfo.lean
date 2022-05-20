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

def getConstructorType (ctor : Name) : MetaM Expr := do
  let env ← getEnv
  match env.find? ctor with
  | some (ConstantInfo.ctorInfo i) => pure i.type
  | _ => panic! s!"Constructor {ctor} was requested but does not exist"

def InductiveInfo.ofInductiveVal (v : InductiveVal) : MetaM InductiveInfo := do
  let info ← Info.ofConstantVal v.toConstantVal
  let env ← getEnv
  let ctors ← v.ctors.mapM (λ name => do NameInfo.ofTypedName name (←getConstructorType name))
  pure $ InductiveInfo.mk info ctors v.isUnsafe

end DocGen4.Process
