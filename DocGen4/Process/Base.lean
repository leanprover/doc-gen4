/-
Copyright (c) 2022 Henrik Böving. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Henrik Böving
-/

import Lean

namespace DocGen4.Process
open Lean Widget Meta

structure NameInfo where
  name  : Name
  type : CodeWithInfos
  doc : Option String
  deriving Inhabited

structure Arg where
  name : Name
  type : CodeWithInfos
  binderInfo : BinderInfo

structure Info extends NameInfo where
  args : Array Arg
  declarationRange : DeclarationRange
  attrs : Array String
  deriving Inhabited

structure AxiomInfo extends Info where
  isUnsafe : Bool
  deriving Inhabited

structure TheoremInfo extends Info
  deriving Inhabited

structure OpaqueInfo extends Info where
  value : CodeWithInfos
  -- A value of partial is interpreted as this constant being part of a partial def
  -- since the actual definition for a partial def is hidden behind an inaccessible value
  unsafeInformation : DefinitionSafety
  deriving Inhabited

structure DefinitionInfo extends Info where
  -- partial defs are handled by OpaqueInfo
  isUnsafe : Bool
  hints : ReducibilityHints
  equations : Option (Array CodeWithInfos)
  isNonComputable : Bool
  deriving Inhabited

abbrev InstanceInfo := DefinitionInfo

structure InductiveInfo extends Info where
  ctors : List NameInfo   -- List of all constructors and their type for this inductive datatype
  isUnsafe : Bool
  deriving Inhabited

structure StructureInfo extends Info where
  fieldInfo : Array NameInfo
  parents : Array Name
  ctor : NameInfo
  deriving Inhabited

structure ClassInfo extends StructureInfo where
  instances : Array Name
  deriving Inhabited

structure ClassInductiveInfo extends InductiveInfo where
  instances : Array Name
  deriving Inhabited

inductive DocInfo where
| axiomInfo (info : AxiomInfo) : DocInfo
| theoremInfo (info : TheoremInfo) : DocInfo
| opaqueInfo (info : OpaqueInfo) : DocInfo
| definitionInfo (info : DefinitionInfo) : DocInfo
| instanceInfo (info : InstanceInfo) : DocInfo
| inductiveInfo (info : InductiveInfo) : DocInfo
| structureInfo (info : StructureInfo) : DocInfo
| classInfo (info : ClassInfo) : DocInfo
| classInductiveInfo (info : ClassInductiveInfo) : DocInfo
  deriving Inhabited

deriving instance Inhabited for ModuleDoc

def prettyPrintTerm (expr : Expr) : MetaM CodeWithInfos := do
  let (fmt, infos) ← formatInfos expr
  let tt := TaggedText.prettyTagged fmt
  let ctx := {
    env := ← getEnv
    mctx := ← getMCtx
    options := ← getOptions
    currNamespace := ← getCurrNamespace
    openDecls := ← getOpenDecls
    fileMap := default,
    ngen := ← getNGen
  }
  pure $ tagExprInfos ctx infos tt

def isInstance (declName : Name) : MetaM Bool := do
  pure $ (instanceExtension.getState (←getEnv)).instanceNames.contains declName

end DocGen4.Process
