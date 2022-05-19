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

def InstanceInfo.ofDefinitionVal (v : DefinitionVal) : MetaM InstanceInfo := do
  let info ← DefinitionInfo.ofDefinitionVal v
  let some className := getClassName (←getEnv) v.type | unreachable!
  if let some instAttr ← getDefaultInstance v.name className then
    pure { info with attrs := info.attrs.push instAttr }
  else
    pure info

end DocGen4.Process
