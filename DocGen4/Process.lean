/-
Copyright (c) 2021 Henrik Böving. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Henrik Böving
-/

import Lean
import Lean.PrettyPrinter
import Std.Data.HashMap
import Lean.Meta.SynthInstance

import DocGen4.Hierarchy

namespace DocGen4

open Lean Meta PrettyPrinter Std

abbrev InfoFormat := (Format × RBMap Delaborator.Pos Elab.Info compare)

structure NameInfo where
  name  : Name
  type : InfoFormat

def NameInfo.prettyPrint (i : NameInfo) : String :=
  s!"{i.name} : {i.type.fst}"

structure Info extends NameInfo where
  doc : Option String
  declarationRange : DeclarationRange

structure AxiomInfo extends Info where
  isUnsafe : Bool

structure TheoremInfo extends Info

structure OpaqueInfo extends Info where
  value : InfoFormat
  isUnsafe : Bool

structure DefinitionInfo extends Info where
  --value : InfoFormat
  unsafeInformation : DefinitionSafety
  hints : ReducibilityHints

abbrev InstanceInfo := DefinitionInfo

structure InductiveInfo extends Info where
  numParams : Nat     -- Number of parameters
  numIndices : Nat    -- Number of indices
  all : List Name     -- List of all (including this one) inductive datatypes in the mutual declaration containing this one
  ctors : List NameInfo   -- List of all constructors and their type for this inductive datatype
  isRec : Bool        -- `true` Iff it is recursive
  isUnsafe : Bool
  isReflexive : Bool
  isNested : Bool

structure FieldInfo extends NameInfo where
  projFn : Name
  subobject? : Option Name

structure StructureInfo extends Info where
  fieldInfo : Array FieldInfo
  parents : Array Name
  ctor : NameInfo

structure ClassInfo extends StructureInfo where
  hasOutParam : Bool
  instances : Array InfoFormat

inductive DocInfo where
| axiomInfo (info : AxiomInfo) : DocInfo
| theoremInfo (info : TheoremInfo) : DocInfo
| opaqueInfo (info : OpaqueInfo) : DocInfo
| definitionInfo (info : DefinitionInfo) : DocInfo
| instanceInfo (info : InstanceInfo) : DocInfo
| inductiveInfo (info : InductiveInfo) : DocInfo
| structureInfo (info : StructureInfo) : DocInfo
| classInfo (info : ClassInfo) : DocInfo

structure Module where
  name : Name
  doc : Option String
  members : Array DocInfo
  deriving Inhabited

def prettyPrintTerm (expr : Expr) : MetaM InfoFormat := do
  let ((expr, _), _) ← Elab.Term.TermElabM.run $ Elab.Term.levelMVarToParam (←instantiateMVars expr)
  let (stx, info) ← delabCore Name.anonymous [] expr
  let stx := sanitizeSyntax stx |>.run' { options := ←getOptions }
  let stx ← parenthesizeTerm stx
  let fmt ← PrettyPrinter.formatTerm stx
  (fmt, info)

def Info.ofConstantVal (v : ConstantVal) : MetaM Info := do
  let env ← getEnv
  let t ← prettyPrintTerm v.type
  let doc ← findDocString? env v.name
  match ←findDeclarationRanges? v.name with
  -- TODO: Maybe selection range is more relevant? Figure this out in the future
  | some range => return Info.mk ⟨v.name, t⟩ doc range.range
  | none => panic! s!"{v.name} is a declaration without position"

def AxiomInfo.ofAxiomVal (v : AxiomVal) : MetaM AxiomInfo := do
  let info ← Info.ofConstantVal v.toConstantVal
  return AxiomInfo.mk info v.isUnsafe

def TheoremInfo.ofTheoremVal (v : TheoremVal) : MetaM TheoremInfo := do
  let info ← Info.ofConstantVal v.toConstantVal
  return TheoremInfo.mk info

def OpaqueInfo.ofOpaqueVal (v : OpaqueVal) : MetaM OpaqueInfo := do
  let info ← Info.ofConstantVal v.toConstantVal
  let t ← prettyPrintTerm v.value
  return OpaqueInfo.mk info t v.isUnsafe

def isInstance (declName : Name) : MetaM Bool := do
  (instanceExtension.getState (←getEnv)).instanceNames.contains declName

def DefinitionInfo.ofDefinitionVal (v : DefinitionVal) : MetaM DefinitionInfo := do
  let info ← Info.ofConstantVal v.toConstantVal
  -- Elaborating the value yields weird exceptions
  --let value ← prettyPrintTerm v.value
  return DefinitionInfo.mk info v.safety v.hints

def getConstructorType (ctor : Name) : MetaM InfoFormat := do
  let env ← getEnv
  match env.find? ctor with
  | some (ConstantInfo.ctorInfo i) => ←prettyPrintTerm i.type
  | _ => panic! s!"Constructor {ctor} was requested but does not exist"

-- TODO: Obtain parameters that come after the inductive Name
def InductiveInfo.ofInductiveVal (v : InductiveVal) : MetaM InductiveInfo := do
  let info ← Info.ofConstantVal v.toConstantVal
  let env ← getEnv
  let ctors ← v.ctors.mapM (λ name => do NameInfo.mk name (←getConstructorType name))
  return InductiveInfo.mk info v.numParams v.numIndices v.all ctors v.isRec v.isUnsafe v.isReflexive v.isNested

def getFieldTypeAux (type : Expr) (vars : List Name) : (Expr × List Name) :=
  match type with
  | Expr.forallE `self _ b .. => (b, (`self :: vars))
  | Expr.forallE n _ b .. => getFieldTypeAux b (n :: vars)
  | _ => (type, vars)

def getFieldType (projFn : Name) : MetaM Expr := do
  let fn ← mkConstWithFreshMVarLevels projFn
  let type ← inferType fn
  let (type, vars) := getFieldTypeAux type []
  type.instantiate $ vars.toArray.map mkConst

def FieldInfo.ofStructureFieldInfo (i : StructureFieldInfo) : MetaM FieldInfo := do
  let type ← getFieldType i.projFn
  let ni := NameInfo.mk i.fieldName (←prettyPrintTerm type)
  FieldInfo.mk ni i.projFn i.subobject?

def StructureInfo.ofInductiveVal (v : InductiveVal) : MetaM StructureInfo := do
  let info ← Info.ofConstantVal v.toConstantVal
  let env ← getEnv
  let parents := getParentStructures env v.name
  let ctor := getStructureCtor env v.name |>.name
  let ctorType ← getConstructorType ctor
  match getStructureInfo? env v.name with
  | some i =>
    let fieldInfos ← i.fieldInfo.mapM FieldInfo.ofStructureFieldInfo
    return StructureInfo.mk info fieldInfos parents ⟨ctor, ctorType⟩
  | none => panic! s!"{v.name} is not a structure"

def ClassInfo.ofInductiveVal (v : InductiveVal) : MetaM ClassInfo := do
  let sinfo ← StructureInfo.ofInductiveVal v
  let fn ← mkConstWithFreshMVarLevels v.name
  let (xs, _, _) ← forallMetaTelescopeReducing (← inferType fn)
  let insts ← SynthInstance.getInstances (mkAppN fn xs)
  let insts_stx ← insts.mapM prettyPrintTerm
  return ClassInfo.mk sinfo (hasOutParams (←getEnv) v.name) insts_stx

namespace DocInfo

def isBlackListed (declName : Name) : MetaM Bool := do
  match ←findDeclarationRanges? declName with
  | some _ =>
    let env ← getEnv
    declName.isInternal
    <||> isAuxRecursor env declName
    <||> isNoConfusion env declName
    <||> isRec declName
    <||> isMatcher declName
  -- TODO: Evaluate whether filtering out declarations without range is sensible
  | none => true

-- TODO: Is this actually the best way?
def isProjFn (declName : Name) : MetaM Bool := do
  let env ← getEnv
  match declName with
  | Name.str parent name _ =>
    if isStructure env parent then
      match getStructureInfo? env parent with
      | some i =>
        match i.fieldNames.find? (· == name) with
        | some _ => true
        | none => false
      | none => panic! s!"{parent} is not a structure"
    else
      false
  | _ => false

def ofConstant : (Name × ConstantInfo) → MetaM (Option DocInfo) := λ (name, info) => do
  if (←isBlackListed name) then
    return none
  match info with
  | ConstantInfo.axiomInfo i => some $ axiomInfo (←AxiomInfo.ofAxiomVal i)
  | ConstantInfo.thmInfo i => some $ theoremInfo (←TheoremInfo.ofTheoremVal i)
  | ConstantInfo.opaqueInfo i => some $ opaqueInfo (←OpaqueInfo.ofOpaqueVal i)
  -- TODO: Find a way to extract equations nicely
  | ConstantInfo.defnInfo i =>
    if ← (isProjFn i.name) then
      none
    else
      let info ← DefinitionInfo.ofDefinitionVal i
      if (←isInstance i.name) then
        some $ instanceInfo info
      else
        some $ definitionInfo info
  | ConstantInfo.inductInfo i =>
    let env ← getEnv
    if isStructure env i.name then
      if isClass env i.name then
        some $ classInfo (←ClassInfo.ofInductiveVal i)
      else
        some $ structureInfo (←StructureInfo.ofInductiveVal i)
    else
      some $ inductiveInfo (←InductiveInfo.ofInductiveVal i)
  -- we ignore these for now
  | ConstantInfo.ctorInfo i => none
  | ConstantInfo.recInfo i => none
  | ConstantInfo.quotInfo i => none

def prettyPrint (i : DocInfo) : CoreM String := do
  match i with
  | axiomInfo i => s!"axiom {i.toNameInfo.prettyPrint}, doc string: {i.doc}"
  | theoremInfo i => s!"theorem {i.toNameInfo.prettyPrint}, doc string: {i.doc}"
  | opaqueInfo i => s!"constant {i.toNameInfo.prettyPrint}, doc string: {i.doc}"
  | definitionInfo i => s!"def {i.toNameInfo.prettyPrint}, doc string: {i.doc}"
  | instanceInfo i => s!"instance {i.toNameInfo.prettyPrint}, doc string: {i.doc}"
  | inductiveInfo i =>
    let ctorString := i.ctors.map NameInfo.prettyPrint
    s!"inductive {←i.toNameInfo.prettyPrint}, ctors: {ctorString}, doc string: {i.doc}"
  | structureInfo i =>
    let ctorString := i.ctor.prettyPrint
    let fieldString := i.fieldInfo.map (λ f => s!"{f.name} : {f.type.fst}")
    s!"structure {i.toNameInfo.prettyPrint} extends {i.parents}, ctor: {ctorString}, fields : {fieldString}, doc string: {i.doc}"
  | classInfo i =>
    let instanceString := i.instances.map Prod.fst
    let fieldString := i.fieldInfo.map (NameInfo.prettyPrint ∘ FieldInfo.toNameInfo)
    s!"class {i.toNameInfo.prettyPrint} extends {i.parents}, fields: {fieldString}, instances : {instanceString}, doc string: {i.doc}"

def getName : DocInfo → Name
| axiomInfo i => i.name
| theoremInfo i => i.name
| opaqueInfo i => i.name
| definitionInfo i => i.name
| instanceInfo i => i.name
| inductiveInfo i => i.name
| structureInfo i => i.name
| classInfo i => i.name

def getKind : DocInfo → String
| axiomInfo _ => "axiom"
| theoremInfo _ => "theorem"
| opaqueInfo _ => "constant"
| definitionInfo _ => "def"
| instanceInfo _ => "instance" -- TODO: This doesnt exist in CSS yet
| inductiveInfo _ => "inductive"
| structureInfo _ => "structure"
| classInfo _ => "class" -- TODO: This is handled as structure right now

def getType : DocInfo → InfoFormat
| axiomInfo i => i.type
| theoremInfo i => i.type
| opaqueInfo i => i.type
| definitionInfo i => i.type
| instanceInfo i => i.type
| inductiveInfo i => i.type
| structureInfo i => i.type
| classInfo i => i.type

end DocInfo

namespace Module

def prettyPrint (m : Module) : CoreM String := do
  let pretty := s!"Module {m.name}, doc string: {m.doc} with members:\n"
  Array.foldlM (λ p mem => return p ++ "  " ++ (←mem.prettyPrint) ++ "\n") pretty m.members

end Module

structure AnalyzerResult where
  name2ModIdx : HashMap Name ModuleIdx
  moduleNames : Array Name
  moduleInfo : HashMap Name Module
  hierarchy : Hierarchy
  deriving Inhabited

def process : MetaM AnalyzerResult := do
  let env ← getEnv
  let mut res := mkHashMap env.header.moduleNames.size
  for module in env.header.moduleNames do
    -- TODO: Check why modules can have multiple doc strings and add that later on
    let moduleDoc := match getModuleDoc? env module with
    | none => none
    | some #[] => none
    | some doc => doc.get! 0

    res := res.insert module (Module.mk module moduleDoc #[])

  for cinfo in env.constants.toList do
    let d := ←DocInfo.ofConstant cinfo
    match d with
    | some dinfo =>
      match (env.getModuleIdxFor? cinfo.fst) with
      | some modidx =>
        -- TODO: Check whether this is still efficient
        let moduleName := env.allImportedModuleNames.get! modidx
        let module := res.find! moduleName
        res := res.insert moduleName {module with members := module.members.push dinfo}
      | none => panic! "impossible"
    | none => ()
  return {
    name2ModIdx := env.const2ModIdx,
    moduleNames := env.header.moduleNames,
    moduleInfo := res,
    hierarchy := Hierarchy.fromArray env.header.moduleNames
  }

end DocGen4
