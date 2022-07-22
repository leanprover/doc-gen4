
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

def ClassInfo.ofInductiveVal (v : InductiveVal) : MetaM ClassInfo := do
  StructureInfo.ofInductiveVal v

def ClassInductiveInfo.ofInductiveVal (v : InductiveVal) : MetaM ClassInductiveInfo := do
  InductiveInfo.ofInductiveVal v

end DocGen4.Process
