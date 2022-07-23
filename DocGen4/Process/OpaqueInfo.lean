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

def OpaqueInfo.ofOpaqueVal (v : OpaqueVal) : MetaM OpaqueInfo := do
  let info ← Info.ofConstantVal v.toConstantVal
  let value ← prettyPrintTerm v.value
  let env ← getEnv
  let isPartial := env.find? (Compiler.mkUnsafeRecName v.name) |>.isSome
  let definitionSafety :=
    if isPartial then
      DefinitionSafety.partial
    else if v.isUnsafe then
      DefinitionSafety.unsafe
    else
      DefinitionSafety.safe
  pure {
    toInfo := info,
    value,
    definitionSafety
  }

end DocGen4.Process
