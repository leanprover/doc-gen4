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

def InductiveInfo.ofInductiveVal (scope : Array Name) (v : InductiveVal) : MetaM InductiveInfo := do
  let info ← Info.ofConstantVal scope v.toConstantVal
  let ctors ← v.ctors.mapM (fun name => do Info.ofConstantVal scope (← getConstInfoCtor name).toConstantVal)
  return {
    toInfo := info,
    ctors,
    isUnsafe := v.isUnsafe
  }

end DocGen4.Process
