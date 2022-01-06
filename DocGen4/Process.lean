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

open Lean Meta PrettyPrinter Std Widget

structure NameInfo where
  name  : Name
  type : CodeWithInfos
  deriving Inhabited

structure Arg where
  name : Name
  type : CodeWithInfos
  binderInfo : BinderInfo

structure Info extends NameInfo where
  args : Array Arg
  doc : Option String
  declarationRange : DeclarationRange
  deriving Inhabited

structure AxiomInfo extends Info where
  isUnsafe : Bool
  deriving Inhabited

structure TheoremInfo extends Info
  deriving Inhabited

structure OpaqueInfo extends Info where
  value : CodeWithInfos
  isUnsafe : Bool
  deriving Inhabited

structure DefinitionInfo extends Info where
  --value : CodeWithInfos
  unsafeInformation : DefinitionSafety
  hints : ReducibilityHints
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

inductive DocInfo where
| axiomInfo (info : AxiomInfo) : DocInfo
| theoremInfo (info : TheoremInfo) : DocInfo
| opaqueInfo (info : OpaqueInfo) : DocInfo
| definitionInfo (info : DefinitionInfo) : DocInfo
| instanceInfo (info : InstanceInfo) : DocInfo
| inductiveInfo (info : InductiveInfo) : DocInfo
| structureInfo (info : StructureInfo) : DocInfo
| classInfo (info : ClassInfo) : DocInfo
  deriving Inhabited

structure Module where
  name : Name
  doc : Option String
  members : Array DocInfo
  deriving Inhabited

partial def typeToArgsType (e : Expr) : (Array (Name × Expr × BinderInfo) × Expr) :=
  match e.consumeMData with
  | Expr.lam name type body data =>
    let name := name.eraseMacroScopes
    let arg := (name, type, data.binderInfo)
    let (args, final) := typeToArgsType (Expr.instantiate1 body (mkFVar ⟨name⟩))
    (#[arg] ++ args, final)
  | Expr.forallE name type body data =>
    let name := name.eraseMacroScopes
    let arg := (name, type, data.binderInfo)
    let (args, final) := typeToArgsType (Expr.instantiate1 body (mkFVar ⟨name⟩))
    (#[arg] ++ args, final)
  | _ => (#[], e)

def prettyPrintTerm (expr : Expr) : MetaM CodeWithInfos := do
  let (fmt, infos) ← formatInfos expr
  let tt := TaggedText.prettyTagged fmt
  let ctx := {
    env := ← getEnv
    mctx := ← getMCtx
    options := ← getOptions
    currNamespace := ← getCurrNamespace
    openDecls := ← getOpenDecls
    fileMap := arbitrary
  }
  tagExprInfos ctx infos tt

def Info.ofConstantVal (v : ConstantVal) : MetaM Info := do
  let env ← getEnv
  let (args, type) := typeToArgsType v.type
  let type ← prettyPrintTerm type
  let args ← args.mapM (λ (n, e, b) => do Arg.mk n (←prettyPrintTerm e) b)
  let doc ← findDocString? env v.name
  match ←findDeclarationRanges? v.name with
  -- TODO: Maybe selection range is more relevant? Figure this out in the future
  | some range => return Info.mk ⟨v.name, type⟩ args doc range.range
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

def getConstructorType (ctor : Name) : MetaM CodeWithInfos := do
  let env ← getEnv
  match env.find? ctor with
  | some (ConstantInfo.ctorInfo i) => ←prettyPrintTerm i.type
  | _ => panic! s!"Constructor {ctor} was requested but does not exist"

def InductiveInfo.ofInductiveVal (v : InductiveVal) : MetaM InductiveInfo := do
  let info ← Info.ofConstantVal v.toConstantVal
  let env ← getEnv
  let ctors ← v.ctors.mapM (λ name => do NameInfo.mk name (←getConstructorType name))
  return InductiveInfo.mk info ctors v.isUnsafe

def dropArgs (type : Expr) (n : Nat) : (Expr × List (Name × Expr)) :=
  match type, n with
  | e, 0 => (e, [])
  | Expr.forallE name type body _, x + 1 =>
    let body := body.instantiate1 $ mkFVar ⟨name⟩
    let next := dropArgs body x
    { next with snd := (name, type) :: next.snd}
  | e, x + 1 => panic! s!"No forallE left"

def getFieldTypes (ctor : ConstructorVal) (parents : Nat) : MetaM (Array NameInfo) := do
  let type := ctor.type
  let (field_function, params) := dropArgs type (ctor.numParams + parents)
  let (_, fields) := dropArgs field_function (ctor.numFields - parents)
  let mut field_infos := #[]
  for (name, type) in fields do
    field_infos := field_infos.push { name := name, type := ←prettyPrintTerm type}
  field_infos

def StructureInfo.ofInductiveVal (v : InductiveVal) : MetaM StructureInfo := do
  let info ← Info.ofConstantVal v.toConstantVal
  let env ← getEnv
  let parents := getParentStructures env v.name
  let ctor := getStructureCtor env v.name
  let ctorType ← prettyPrintTerm ctor.type
  match getStructureInfo? env v.name with
  | some i =>
    if i.fieldNames.size - parents.size > 0 then
      return StructureInfo.mk info (←getFieldTypes ctor parents.size) parents ⟨ctor.name, ctorType⟩
    else
      return StructureInfo.mk info #[] parents ⟨ctor.name, ctorType⟩
  | none => panic! s!"{v.name} is not a structure"

def ClassInfo.ofInductiveVal (v : InductiveVal) : MetaM ClassInfo := do
  let sinfo ← StructureInfo.ofInductiveVal v
  let fn ← mkConstWithFreshMVarLevels v.name
  let (xs, _, _) ← forallMetaTelescopeReducing (← inferType fn)
  let insts ← SynthInstance.getInstances (mkAppN fn xs)
  return ClassInfo.mk sinfo (insts.map Expr.constName!)

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

def getType : DocInfo → CodeWithInfos
| axiomInfo i => i.type
| theoremInfo i => i.type
| opaqueInfo i => i.type
| definitionInfo i => i.type
| instanceInfo i => i.type
| inductiveInfo i => i.type
| structureInfo i => i.type
| classInfo i => i.type

def getArgs : DocInfo → Array Arg
| axiomInfo i => i.args
| theoremInfo i => i.args
| opaqueInfo i => i.args
| definitionInfo i => i.args
| instanceInfo i => i.args
| inductiveInfo i => i.args
| structureInfo i => i.args
| classInfo i => i.args

end DocInfo

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
