import Lean
import Lean.ReducibilityAttrs

namespace DocGen4

open Lean Meta
-- The following is probably completely overengineered but I love it

/--
Captures the notion of a value based attributes, `attrKind` is things like
`EnumAttributes`.
-/
class ValueAttr (attrKind : Type → Type) where
  /--
  Given a certain value based attribute, an `Environment` and the `Name` of
  a declaration returns the value of the attribute on this declaration if present.
  -/
  getValue {α : Type} [Inhabited α] [ToString α] : attrKind α → Environment → Name → Option String

/--
Contains a specific attribute declaration of a certain attribute kind (enum based, parametric etc.).
-/
structure ValueAttrWrapper (attrKind : Type → Type) [ValueAttr attrKind] where
  {α : Type}
  attr : attrKind α
  [str : ToString α]
  [inhab : Inhabited α]

/--
Obtain the value of an enum attribute for a certain name.
-/
def enumGetValue {α : Type} [Inhabited α] [ToString α] (attr : EnumAttributes α) (env : Environment) (decl : Name) : Option String := do
  let val ← EnumAttributes.getValue attr env decl
  some (toString val)

instance : ValueAttr EnumAttributes where
  getValue := enumGetValue

/--
Obtain the value of a parametric attribute for a certain name.
-/
def parametricGetValue {α : Type} [Inhabited α] [ToString α] (attr : ParametricAttribute α) (env : Environment) (decl : Name) : Option String := do
  let val ← ParametricAttribute.getParam? attr env decl
  some (attr.attr.name.toString ++ " " ++ toString val)

instance : ValueAttr ParametricAttribute where
  getValue := parametricGetValue

abbrev EnumAttrWrapper := ValueAttrWrapper EnumAttributes
abbrev ParametricAttrWrapper := ValueAttrWrapper ParametricAttribute

/--
The list of all tag based attributes doc-gen knows about and can recover.
-/
def tagAttributes : Array TagAttribute :=
  #[IR.UnboxResult.unboxAttr, neverExtractAttr,
    Elab.Term.elabWithoutExpectedTypeAttr, matchPatternAttr]

deriving instance Repr for Compiler.InlineAttributeKind
deriving instance Repr for Compiler.SpecializeAttributeKind

open Compiler in
instance : ToString InlineAttributeKind where
  toString kind :=
    match kind with
    | .inline => "inline"
    | .noinline => "noinline"
    | .macroInline => "macro_inline"
    | .inlineIfReduce => "inline_if_reduce"
    | .alwaysInline => "always_inline"

open Compiler in
instance : ToString SpecializeAttributeKind where
  toString kind :=
    match kind with
    | .specialize => "specialize"
    | .nospecialize => "nospecialize"

instance : ToString ReducibilityStatus where
  toString kind :=
    match kind with
    | .reducible => "reducible"
    | .semireducible => "semireducible"
    | .irreducible => "irreducible"
    | .implicitReducible => "implicit_reducible"

/--
The list of all enum based attributes doc-gen knows about and can recover.
-/
@[reducible]
def enumAttributes : Array EnumAttrWrapper := #[⟨Compiler.inlineAttrs⟩]

instance : ToString ExternEntry where
  toString entry :=
    match entry with
    | .adhoc `all => ""
    | .adhoc backend => s!"{backend} adhoc"
    | .standard `all fn => fn
    | .standard backend fn => s!"{backend} {fn}"
    | .inline backend pattern => s!"{backend} inline {String.quote pattern}"
    | .opaque .. => ""

instance : ToString ExternAttrData where
  toString data := String.intercalate " " (data.entries.map toString)

instance : ToString Linter.DeprecationEntry where
  toString entry := Id.run do
    let mut string := ""

    if let some newName := entry.newName? then
      string := string ++ s!"{newName} "
    if let some text := entry.text? then
      string := string ++ s!"\"{text}\" "
    if let some since := entry.since? then
      string := string ++ s!"(since := \"{since}\")"

    string := string.trimAsciiEnd.copy
    return string

/--
The list of all parametric attributes (that is, attributes with any kind of information attached)
doc-gen knows about and can recover.
-/
def parametricAttributes : Array ParametricAttrWrapper := #[⟨externAttr⟩, ⟨Compiler.implementedByAttr⟩, ⟨exportAttr⟩, ⟨Compiler.specializeAttr⟩, ⟨Linter.deprecatedAttr⟩]

def getTags (decl : Name) : MetaM (Array String) := do
  let env ← getEnv
  return tagAttributes.filter (TagAttribute.hasTag · env decl) |>.map (·.attr.name.toString)

def getValuesAux {α : Type} {attrKind : Type → Type} [va : ValueAttr attrKind] [Inhabited α] [ToString α] (decl : Name) (attr : attrKind α) : MetaM (Option String) := do
  let env ← getEnv
  return va.getValue attr env decl

def getValues {attrKind : Type → Type} [ValueAttr attrKind] (decl : Name) (attrs : Array (ValueAttrWrapper attrKind)) : MetaM (Array String) := do
  let mut res := #[]
  for attr in attrs do
    if let some val ← @getValuesAux attr.α attrKind _ attr.inhab attr.str decl attr.attr then
      res := res.push val
  return res

def getEnumValues (decl : Name) : MetaM (Array String) := getValues decl enumAttributes

def getParametricValues (decl : Name) : MetaM (Array String) :=
  getValues decl parametricAttributes

def getDefaultInstance (decl : Name) (className : Name) : MetaM (Option String) := do
  let insts ← getDefaultInstances className
  for (inst, prio) in insts do
    if inst == decl then
      return some s!"defaultInstance {prio}"
  return none

def hasSimp (decl : Name) : MetaM (Option String) := do
  let thms ← simpExtension.getTheorems
  if thms.isLemma (.decl decl) then
    return "simp"
  else
    return none

def hasCsimp (decl : Name) : MetaM (Option String) := do
  let env ← getEnv
  if Compiler.hasCSimpAttribute env decl then
    return some "csimp"
  else
    return none

def getReducibility (decl : Name) : MetaM (Option String) := do
  let status ← getReducibilityStatus decl
  match status with
  | .reducible => return some "reducible"
  | .irreducible => return some "irreducible"
  | .implicitReducible => return some "implicit_reducible"
  -- This is the default so we don't print it.
  | .semireducible => return none

/--
The list of custom attributes, that don't fit in the parametric or enum
attribute kinds, doc-gen konws about and can recover.
-/
def customAttrs := #[hasSimp, hasCsimp, getReducibility]

def getCustomAttrs (decl : Name) : MetaM (Array String) := do
  let mut values := #[]
  for attr in customAttrs do
    if let some value ← attr decl then
      values := values.push value
  return values

/--
The main entry point for recovering all attribute values for a given
declaration.
-/
def getAllAttributes (decl : Name) : MetaM (Array String) := do
  let tags ← getTags decl
  let enums ← getEnumValues decl
  let parametric ← getParametricValues decl
  let customs ← getCustomAttrs decl
  return customs ++ tags ++ enums ++ parametric

end DocGen4
