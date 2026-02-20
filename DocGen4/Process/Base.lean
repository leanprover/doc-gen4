/-
Copyright (c) 2022 Henrik Böving. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Henrik Böving
-/

import Lean
import DocGen4.RenderedCode

namespace DocGen4.Process
open Lean Widget Meta

structure DocGenOptions where
  genEquations : Bool := true

abbrev AnalyzeM : Type → Type := ReaderT DocGenOptions MetaM

def versoDocToMarkdown (v : VersoDocString) : String :=
  let { text, subsections } := v
  Doc.MarkdownM.run' do
    for b in text do
      Doc.ToMarkdown.toMarkdown b
    for p in subsections do
      Doc.ToMarkdown.toMarkdown p


/--
Stores information about a typed name.
-/
structure NameInfo where
  /--
  The name that has this info attached.
  -/
  name : Name
  /--
  The pretty printed type of this name.
  -/
  type : RenderedCode
  /--
  The doc string of the name if it exists.
  -/
  doc : Option (String ⊕ VersoDocString)
  deriving Inhabited

/--
An argument to a declaration, e.g. the `(x : Nat)` in `def foo (x : Nat) := x`.
-/
structure Arg where
  /--
  The pretty printed binder syntax itself.
  -/
  binder : RenderedCode
  /--
  Whether the binder is implicit.
  -/
  implicit : Bool

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
  /--
  Whether this declaration directly uses `sorryAx`.
  -/
  sorried : Bool := false
  /--
  Whether this info item should be rendered
  -/
  render : Bool := true
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
  A value of partial is interpreted as this opaque being part of a partial def
  since the actual definition for a partial def is hidden behind an inaccessible value.
  -/
  definitionSafety : DefinitionSafety
  deriving Inhabited


/--
The maximum string length of equations before they are omitted from rendering.

This is an arbitrary cutoff that seems to work well in practice. Very long equations are unreadable
in documentation and slow down page generation and rendering, so we drop them and show a notice
instead. The number 200 has no deeper justification.

Equations exceeding this limit are stored as NULL blobs in the database (only `text_length` is
preserved). The `equationsWereOmitted` field in `DefinitionInfo` is set when any equation exceeds
this limit, causing the rendering code in `DocGen4/Output/Definition.lean` to show a notice.
-/
def equationLimit : Nat := 200

/--
Information about a `def` declaration, note that partial defs are handled by `OpaqueInfo`.
-/
structure DefinitionInfo extends Info where
  isUnsafe : Bool
  hints : ReducibilityHints
  equations : Option (Array RenderedCode)
  equationsWereOmitted : Bool := false
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
Information about a constructor of an inductive type
-/
abbrev ConstructorInfo := Info

/--
Information about an `inductive` declaration
-/
structure InductiveInfo extends Info where
  /--
  List of all constructors of this inductive type.
  -/
  ctors : List ConstructorInfo
  isUnsafe : Bool
  deriving Inhabited

/--
Stores information about a structure field.
-/
structure FieldInfo extends Info where
  /--
  Whether or not this field is new to this structure, or instead whether it was inherited from a parent.
  -/
  isDirect : Bool

/--
Information about a `structure` parent.
-/
structure StructureParentInfo where
  /-- Name of the projection function. -/
  projFn : Name
  /-- Pretty printed type. -/
  type : RenderedCode

/--
Information about a `structure` declaration.
-/
structure StructureInfo extends Info where
  /--
  Information about all the fields of the structure.
  -/
  fieldInfo : Array FieldInfo
  /--
  All the structures this one inherited from.
  -/
  parents : Array StructureParentInfo
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
| ctorInfo (info : ConstructorInfo) : DocInfo
  deriving Inhabited

def DocInfo.toInfo : DocInfo → Info
  | .axiomInfo info => info.toInfo
  | .theoremInfo info => info.toInfo
  | .opaqueInfo info => info.toInfo
  | .definitionInfo info => info.toInfo
  | .instanceInfo info => info.toInfo
  | .inductiveInfo info => info.toInfo
  | .structureInfo info => info.toInfo
  | .classInfo info => info.toInfo
  | .classInductiveInfo info => info.toInfo
  | .ctorInfo info => info

/--
Turns an `Expr` into a pretty printed `RenderedCode`.
-/
def prettyPrintTerm (expr : Expr) : MetaM RenderedCode := do
  let ⟨fmt, infos⟩ ← PrettyPrinter.ppExprWithInfos expr
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
  return renderTagged (← tagCodeInfos ctx infos tt)

def isInstance (declName : Name) : MetaM Bool := do
  return (instanceExtension.getState (← getEnv)).instanceNames.contains declName

end DocGen4.Process
