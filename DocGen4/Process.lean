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
  -- A value of partial is interpreted as this constant being part of a partial def
  -- since the actual definition for a partial def is hidden behind an inaccessible value
  unsafeInformation : DefinitionSafety
  deriving Inhabited

structure DefinitionInfo extends Info where
  -- partial defs are handled by OpaqueInfo
  isUnsafe : Bool
  hints : ReducibilityHints
  equations : Option (Array CodeWithInfos)
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

namespace DocInfo

def getDeclarationRange : DocInfo → DeclarationRange
| axiomInfo i => i.declarationRange
| theoremInfo i => i.declarationRange
| opaqueInfo i => i.declarationRange
| definitionInfo i => i.declarationRange
| instanceInfo i => i.declarationRange
| inductiveInfo i => i.declarationRange
| structureInfo i => i.declarationRange
| classInfo i => i.declarationRange

def lineOrder (l r : DocInfo) : Bool :=
  l.getDeclarationRange.pos.line < r.getDeclarationRange.pos.line

end DocInfo

structure Module where
  name : Name
  doc : Option String
  -- Sorted according to their line numbers
  members : Array DocInfo
  deriving Inhabited

partial def typeToArgsType (e : Expr) : (Array (Name × Expr × BinderInfo) × Expr) :=
  let helper := λ name type body data =>
    -- Once we hit a name with a macro scope we stop traversing the expression
    -- and print what is left after the : instead. The only exception
    -- to this is instances since these almost never have a name
    -- but should still be printed as arguments instead of after the :.
    if name.hasMacroScopes ∧ ¬data.binderInfo.isInstImplicit then
      (#[], e)
    else
      let name := name.eraseMacroScopes
      let arg := (name, type, data.binderInfo)
      let (args, final) := typeToArgsType (Expr.instantiate1 body (mkFVar ⟨name⟩))
      (#[arg] ++ args, final)

  match e.consumeMData with
  | Expr.lam name type body data => helper name type body data
  | Expr.forallE name type body data => helper name type body data
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
    fileMap := default
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
  let env ← getEnv
  let isPartial := env.find? (Compiler.mkUnsafeRecName v.name) |>.isSome
  if isPartial then
    return OpaqueInfo.mk info t DefinitionSafety.partial
  else
    let safety := if v.isUnsafe then DefinitionSafety.unsafe else DefinitionSafety.safe
    return OpaqueInfo.mk info t safety

def isInstance (declName : Name) : MetaM Bool := do
  (instanceExtension.getState (←getEnv)).instanceNames.contains declName

partial def stripArgs (e : Expr) : Expr :=
  match e.consumeMData with
  | Expr.lam name type body data =>
    let name := name.eraseMacroScopes
    stripArgs (Expr.instantiate1 body (mkFVar ⟨name⟩))
  | Expr.forallE name type body data =>
    let name := name.eraseMacroScopes
    stripArgs (Expr.instantiate1 body (mkFVar ⟨name⟩))
  | _ => e

def processEq (eq : Name) : MetaM CodeWithInfos := do
  let type ← (mkConstWithFreshMVarLevels eq >>= inferType)
  let final := stripArgs type
  prettyPrintTerm final

def valueToEq (v : DefinitionVal) : MetaM Expr := withLCtx {} {} do
  let env ← getEnv
  withOptions (tactic.hygienic.set . false) do
    lambdaTelescope v.value fun xs body => do
      let us := v.levelParams.map mkLevelParam
      let type ← mkEq (mkAppN (Lean.mkConst v.name us) xs) body
      let type ← mkForallFVars xs type
      type

def DefinitionInfo.ofDefinitionVal (v : DefinitionVal) : MetaM DefinitionInfo := do
  let info ← Info.ofConstantVal v.toConstantVal
  let isUnsafe := v.safety == DefinitionSafety.unsafe
  try
    let eqs? ← getEqnsFor? v.name
    match eqs? with
    | some eqs =>
      let prettyEqs ← eqs.mapM processEq
      DefinitionInfo.mk info isUnsafe v.hints prettyEqs
    | none =>
      let eq ← prettyPrintTerm $ stripArgs (←valueToEq v)
      DefinitionInfo.mk info isUnsafe v.hints (some #[eq])
  catch err =>
    IO.println s!"WARNING: Failed to calculate equational lemmata for {v.name}: {←err.toMessageData.toString}"
    return DefinitionInfo.mk info isUnsafe v.hints none

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

def getFieldTypes (struct : Name) (ctor : ConstructorVal) (parents : Nat) : MetaM (Array NameInfo) := do
  let type := ctor.type
  let (field_function, params) := dropArgs type (ctor.numParams + parents)
  let (_, fields) := dropArgs field_function (ctor.numFields - parents)
  let mut field_infos := #[]
  for (name, type) in fields do
    field_infos := field_infos.push { name := struct.append name, type := ←prettyPrintTerm type}
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
      return StructureInfo.mk info (←getFieldTypes v.name ctor parents.size) parents ⟨ctor.name, ctorType⟩
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
| instanceInfo _ => "instance"
| inductiveInfo _ => "inductive"
| structureInfo _ => "structure"
| classInfo _ => "class"

def getKindDescription : DocInfo → String
| axiomInfo i => if i.isUnsafe then "unsafe axiom" else "axiom"
| theoremInfo _ => "theorem"
| opaqueInfo i =>
  match i.unsafeInformation with
  | DefinitionSafety.safe => "constant"
  | DefinitionSafety.unsafe => "unsafe constant"
  | DefinitionSafety.partial => "partial def"
| definitionInfo i =>
  if i.hints.isAbbrev then
    "abbrev"
  else if i.isUnsafe then
    "unsafe def"
  else
    "def"
| instanceInfo i => if i.isUnsafe then "unsafe instance" else "instance"
| inductiveInfo i => if i.isUnsafe then "unsafe inductive" else "inductive"
| structureInfo _ => "structure"
| classInfo _ => "class"

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
  -- Indexed by ModIdx
  importAdj : Array (Array Bool)
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
    try
      let analysis := Prod.fst <$> Meta.MetaM.toIO (DocInfo.ofConstant cinfo) { maxHeartbeats := 5000000, options := ⟨[(`pp.tagAppFns, true)]⟩ } { env := env} {} {}
      if let some dinfo ← analysis then
        let some modidx ← env.getModuleIdxFor? dinfo.getName | unreachable!
        let moduleName := env.allImportedModuleNames.get! modidx
        let module := res.find! moduleName
        res := res.insert moduleName {module with members := module.members.push dinfo}
    catch e =>
      IO.println s!"WARNING: Failed to obtain information for: {cinfo.fst}: {←e.toMessageData.toString}"

  -- TODO: This is definitely not the most efficient way to store this data
  let mut adj := Array.mkArray res.size (Array.mkArray res.size false)
  -- TODO: This could probably be faster if we did an insertion sort above instead
  for (moduleName, module) in res.toArray do
    res := res.insert moduleName {module with members := module.members.qsort DocInfo.lineOrder}
    let some modIdx ← env.getModuleIdx? moduleName | unreachable!
    let moduleData := env.header.moduleData.get! modIdx
    for imp in moduleData.imports do
      let some importIdx ← env.getModuleIdx? imp.module | unreachable!
      adj := adj.set! modIdx (adj.get! modIdx |>.set! importIdx true)

  return {
    name2ModIdx := env.const2ModIdx,
    moduleNames := env.header.moduleNames,
    moduleInfo := res,
    hierarchy := Hierarchy.fromArray env.header.moduleNames,
    importAdj := adj
  }



end DocGen4
