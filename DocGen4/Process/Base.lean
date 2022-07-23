/-
Copyright (c) 2022 Henrik Böving. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Henrik Böving
-/

import Lean

namespace DocGen4.Process
open Lean Widget Meta

/--
Stores information about a typed name.
-/
structure NameInfo where
  /--
  The name that has this info attached.
  -/
  name  : Name
  /--
  The pretty printed type of this name.
  -/
  type : CodeWithInfos
  /--
  The doc string of the name if it exists.
  -/
  doc : Option String
  deriving Inhabited

/--
An argument to a declaration, e.g. the `(x : Nat)` in `def foo (x : Nat) := x`.
-/
structure Arg where
  /--
  The name of the argument.
  -/
  name : Name
  /--
  The pretty printed type of the argument.
  -/
  type : CodeWithInfos
  /--
  What kind of binder was used for the argument.
  -/
  binderInfo : BinderInfo

/--
A base structure for information about a declaration.
-/
structure Info extends NameInfo where
  /--
  The list of arguments to the declaration.
  -/
  args : Array Arg
  /--
  In which lines the declaration was created.
  -/
  declarationRange : DeclarationRange
  /--
  A list of (known) attributes that are attached to the declaration.
  -/
  attrs : Array String
  deriving Inhabited

/--
Information about an `axiom` declaration.
-/
structure AxiomInfo extends Info where
  isUnsafe : Bool
  deriving Inhabited

/--
Information about a `theorem` declaration.
-/
structure TheoremInfo extends Info
  deriving Inhabited

/--
Information about an `opaque` declaration.
-/
structure OpaqueInfo extends Info where
  /--
  The pretty printed value of the declaration.
  -/
  value : CodeWithInfos
  /--
  A value of partial is interpreted as this opaque being part of a partial def
  since the actual definition for a partial def is hidden behind an inaccessible value.
  -/
  definitionSafety : DefinitionSafety
  deriving Inhabited

/--
Information about a `def` declaration, note that partial defs are handled by `OpaqueInfo`.
-/
structure DefinitionInfo extends Info where
  isUnsafe : Bool
  hints : ReducibilityHints
  equations : Option (Array CodeWithInfos)
  isNonComputable : Bool
  deriving Inhabited

/--
Information about an `instance` declaration.
-/
structure InstanceInfo extends DefinitionInfo where
  className : Name
  typeNames : Array Name
  deriving Inhabited

/--
Information about an `inductive` declaration
-/
structure InductiveInfo extends Info where
  /--
  List of all constructors of this inductive type.
  -/
  ctors : List NameInfo
  isUnsafe : Bool
  deriving Inhabited

/--
Information about a `structure` declaration.
-/
structure StructureInfo extends Info where
  /--
  Information about all the fields of the structure.
  -/
  fieldInfo : Array NameInfo
  /--
  All the structures this one inherited from.
  -/
  parents : Array Name
  /--
  The constructor of the structure.
  -/
  ctor : NameInfo
  deriving Inhabited

/--
Information about a `class` declaration.
-/
abbrev ClassInfo := StructureInfo

/--
Information about a `class inductive` declaration.
-/
abbrev ClassInductiveInfo := InductiveInfo

/--
A general type for informations about declarations.
-/
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

/--
Turns an `Expr` into a pretty printed `CodeWithInfos`.
-/
def prettyPrintTerm (expr : Expr) : MetaM CodeWithInfos := do
  let (fmt, infos) ←  PrettyPrinter.ppExprWithInfos expr
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
  pure <| tagExprInfos ctx infos tt

def isInstance (declName : Name) : MetaM Bool := do
  pure <| (instanceExtension.getState (←getEnv)).instanceNames.contains declName

end DocGen4.Process
