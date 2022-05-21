import Lean

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
  let val ← ParametricAttribute.getParam attr env decl
  some (attr.attr.name.toString ++ " " ++ toString val)

instance : ValueAttr ParametricAttribute where
  getValue := parametricGetValue

abbrev EnumAttrWrapper := ValueAttrWrapper EnumAttributes
abbrev ParametricAttrWrapper := ValueAttrWrapper ParametricAttribute

/--
The list of all tag based attributes doc-gen knows about and can recover.
-/
def tagAttributes : Array TagAttribute :=
  #[IR.UnboxResult.unboxAttr, neverExtractAttr, Elab.Term.elabWithoutExpectedTypeAttr,
    SynthInstance.inferTCGoalsRLAttr, matchPatternAttr]

deriving instance Repr for Compiler.InlineAttributeKind
deriving instance Repr for Compiler.SpecializeAttributeKind

open Compiler in
instance : ToString InlineAttributeKind where
  toString kind :=
    match kind with
    | InlineAttributeKind.inline => "inline"
    | InlineAttributeKind.noinline => "noinline"
    | InlineAttributeKind.macroInline => "macroInline"
    | InlineAttributeKind.inlineIfReduce => "inlineIfReduce"

open Compiler in
instance : ToString SpecializeAttributeKind where
  toString kind :=
    match kind with
    | SpecializeAttributeKind.specialize => "specialize"
    | SpecializeAttributeKind.nospecialize => "nospecialize"

/--
The list of all enum based attributes doc-gen knows about and can recover.
-/
def enumAttributes : Array EnumAttrWrapper := #[⟨Compiler.inlineAttrs⟩, ⟨Compiler.specializeAttrs⟩]

instance : ToString ExternEntry where
  toString entry :=
    match entry with
    | ExternEntry.adhoc `all => ""
    | ExternEntry.adhoc backend => s!"{backend} adhoc"
    | ExternEntry.standard `all fn => fn
    | ExternEntry.standard backend fn => s!"{backend} {fn}"
    | ExternEntry.inline backend pattern => s!"{backend} inline {String.quote pattern}"
    -- TODO: The docs in the module dont specific how to render this
    | ExternEntry.foreign backend fn  => s!"{backend} foreign {fn}"

instance : ToString ExternAttrData where
  toString data := (data.arity?.map toString |>.getD "") ++ " " ++ String.intercalate " " (data.entries.map toString)

/--
The list of all parametric attributes (that is, attributes with any kind of information attached)
doc-gen knows about and can recover.
-/
def parametricAttributes : Array ParametricAttrWrapper := #[⟨externAttr⟩, ⟨Compiler.implementedByAttr⟩, ⟨exportAttr⟩]

def getTags (decl : Name) : MetaM (Array String) := do
  let env ← getEnv
  pure $ tagAttributes.filter (TagAttribute.hasTag · env decl) |>.map (λ t => t.attr.name.toString)

def getValuesAux {α : Type} {attrKind : Type → Type} [va : ValueAttr attrKind] [Inhabited α] [ToString α] (decl : Name) (attr : attrKind α) : MetaM (Option String) := do
  let env ← getEnv
  pure $ va.getValue attr env decl

def getValues {attrKind : Type → Type} [ValueAttr attrKind] (decl : Name) (attrs : Array (ValueAttrWrapper attrKind)) : MetaM (Array String) := do
  let env ← getEnv
  let mut res := #[]
  for attr in attrs do
    if let some val ← @getValuesAux attr.α attrKind _ attr.inhab attr.str decl attr.attr then
      res := res.push val
  pure res

def getEnumValues (decl : Name) : MetaM (Array String) := getValues decl enumAttributes
def getParametricValues (decl : Name) : MetaM (Array String) := getValues decl parametricAttributes

def getDefaultInstance (decl : Name) (className : Name) : MetaM (Option String) := do
  let insts ← getDefaultInstances className
  for (inst, prio) in insts do
    if inst == decl then
      return some $ s!"defaultInstance {prio}"
  pure none

def hasSimp (decl : Name) : MetaM (Option String) := do
  let thms ← simpExtension.getTheorems
  pure $
    if thms.isLemma decl then
      some "simp"
    else
      none

def hasCsimp (decl : Name) : MetaM (Option String) := do
  let env ← getEnv
  pure $
    if Compiler.hasCSimpAttribute env decl then
      some "csimp"
    else
      none

/--
The list of custom attributes, that don't fit in the parametric or enum
attribute kinds, doc-gen konws about and can recover.
-/
def customAttrs := #[hasSimp, hasCsimp]

def getCustomAttrs (decl : Name) : MetaM (Array String) := do
  let mut values := #[]
  for attr in customAttrs do
    if let some value ← attr decl then
      values := values.push value
  pure values

/--
The main entry point for recovering all attribute values for a given
declaration.
-/
def getAllAttributes (decl : Name) : MetaM (Array String) := do
  let tags ← getTags decl
  let enums ← getEnumValues decl
  let parametric ← getParametricValues decl
  let customs ← getCustomAttrs decl
  pure $ customs ++ tags ++ enums ++ parametric

end DocGen4
