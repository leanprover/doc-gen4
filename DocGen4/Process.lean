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
import DocGen4.Attributes

namespace DocGen4

open Lean Meta PrettyPrinter Std Widget

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
| classInductiveInfo i => i.declarationRange

end DocInfo

inductive ModuleMember where
| docInfo (info : DocInfo) : ModuleMember
| modDoc (doc : ModuleDoc) : ModuleMember
deriving Inhabited

structure Module where
  name : Name
  -- Sorted according to their line numbers
  members : Array ModuleMember
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
    fileMap := default,
    ngen := ← getNGen
  }
  pure $ tagExprInfos ctx infos tt

def NameInfo.ofTypedName (n : Name) (t : Expr) : MetaM NameInfo := do
  let env ← getEnv
  pure { name := n, type := ←prettyPrintTerm t, doc := ←findDocString? env n}

def Info.ofConstantVal (v : ConstantVal) : MetaM Info := do
  let env ← getEnv
  let (args, type) := typeToArgsType v.type
  let args ← args.mapM (λ (n, e, b) => do pure $ Arg.mk n (←prettyPrintTerm e) b)
  let doc ← findDocString? env v.name
  let nameInfo ← NameInfo.ofTypedName v.name type
  match ←findDeclarationRanges? v.name with
  -- TODO: Maybe selection range is more relevant? Figure this out in the future
  | some range => pure $ Info.mk nameInfo args range.range (←getAllAttributes v.name)
  | none => panic! s!"{v.name} is a declaration without position"

def AxiomInfo.ofAxiomVal (v : AxiomVal) : MetaM AxiomInfo := do
  let info ← Info.ofConstantVal v.toConstantVal
  pure $ AxiomInfo.mk info v.isUnsafe

def TheoremInfo.ofTheoremVal (v : TheoremVal) : MetaM TheoremInfo := do
  let info ← Info.ofConstantVal v.toConstantVal
  pure $ TheoremInfo.mk info

def OpaqueInfo.ofOpaqueVal (v : OpaqueVal) : MetaM OpaqueInfo := do
  let info ← Info.ofConstantVal v.toConstantVal
  let t ← prettyPrintTerm v.value
  let env ← getEnv
  let isPartial := env.find? (Compiler.mkUnsafeRecName v.name) |>.isSome
  if isPartial then
    pure $ OpaqueInfo.mk info t DefinitionSafety.partial
  else
    let safety := if v.isUnsafe then DefinitionSafety.unsafe else DefinitionSafety.safe
    pure $ OpaqueInfo.mk info t safety

def isInstance (declName : Name) : MetaM Bool := do
  pure $ (instanceExtension.getState (←getEnv)).instanceNames.contains declName

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
      pure type

def DefinitionInfo.ofDefinitionVal (v : DefinitionVal) : MetaM DefinitionInfo := do
  let info ← Info.ofConstantVal v.toConstantVal
  let isUnsafe := v.safety == DefinitionSafety.unsafe
  let isNonComput := isNoncomputable (←getEnv) v.name
  try
    let eqs? ← getEqnsFor? v.name
    match eqs? with
    | some eqs =>
      let prettyEqs ← eqs.mapM processEq
      pure $ DefinitionInfo.mk info isUnsafe v.hints prettyEqs isNonComput
    | none =>
      let eq ← prettyPrintTerm $ stripArgs (←valueToEq v)
      pure $ DefinitionInfo.mk info isUnsafe v.hints (some #[eq]) isNonComput
  catch err =>
    IO.println s!"WARNING: Failed to calculate equational lemmata for {v.name}: {←err.toMessageData.toString}"
    pure $ DefinitionInfo.mk info isUnsafe v.hints none isNonComput

def InstanceInfo.ofDefinitionVal (v : DefinitionVal) : MetaM InstanceInfo := do
  let info ← DefinitionInfo.ofDefinitionVal v
  let some className := getClassName (←getEnv) v.type | unreachable!
  if let some instAttr ← getDefaultInstance v.name className then
    pure { info with attrs := info.attrs.push instAttr }
  else
    pure info

def getConstructorType (ctor : Name) : MetaM Expr := do
  let env ← getEnv
  match env.find? ctor with
  | some (ConstantInfo.ctorInfo i) => pure i.type
  | _ => panic! s!"Constructor {ctor} was requested but does not exist"

def InductiveInfo.ofInductiveVal (v : InductiveVal) : MetaM InductiveInfo := do
  let info ← Info.ofConstantVal v.toConstantVal
  let env ← getEnv
  let ctors ← v.ctors.mapM (λ name => do NameInfo.ofTypedName name (←getConstructorType name))
  pure $ InductiveInfo.mk info ctors v.isUnsafe

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
  let (fieldFunction, params) := dropArgs type (ctor.numParams + parents)
  let (_, fields) := dropArgs fieldFunction (ctor.numFields - parents)
  let mut fieldInfos := #[]
  for (name, type) in fields do
    fieldInfos := fieldInfos.push $ ←NameInfo.ofTypedName (struct.append name) type
  pure $ fieldInfos

def StructureInfo.ofInductiveVal (v : InductiveVal) : MetaM StructureInfo := do
  let info ← Info.ofConstantVal v.toConstantVal
  let env ← getEnv
  let parents := getParentStructures env v.name
  let ctor := getStructureCtor env v.name
  match getStructureInfo? env v.name with
  | some i =>
    if i.fieldNames.size - parents.size > 0 then
      pure $ StructureInfo.mk info (←getFieldTypes v.name ctor parents.size) parents (←NameInfo.ofTypedName ctor.name ctor.type)
    else
      pure $ StructureInfo.mk info #[] parents (←NameInfo.ofTypedName ctor.name ctor.type)
  | none => panic! s!"{v.name} is not a structure"

def getInstances (className : Name) : MetaM (Array Name) := do
  let fn ← mkConstWithFreshMVarLevels className
  let (xs, _, _) ← forallMetaTelescopeReducing (← inferType fn)
  let insts ← SynthInstance.getInstances (mkAppN fn xs)
  pure $ insts.map Expr.constName!

def ClassInfo.ofInductiveVal (v : InductiveVal) : MetaM ClassInfo := do
  let sinfo ← StructureInfo.ofInductiveVal v
  pure $ ClassInfo.mk sinfo (←getInstances v.name)

def ClassInductiveInfo.ofInductiveVal (v : InductiveVal) : MetaM ClassInductiveInfo := do
  let info ← InductiveInfo.ofInductiveVal v
  pure $ ClassInductiveInfo.mk info (←getInstances v.name)

namespace DocInfo

def isBlackListed (declName : Name) : MetaM Bool := do
  match ←findDeclarationRanges? declName with
  | some _ =>
    let env ← getEnv
    pure (declName.isInternal)
    <||> (pure $ isAuxRecursor env declName)
    <||> (pure $ isNoConfusion env declName)
    <||> isRec declName
    <||> isMatcher declName
  -- TODO: Evaluate whether filtering out declarations without range is sensible
  | none => pure true

-- TODO: Is this actually the best way?
def isProjFn (declName : Name) : MetaM Bool := do
  let env ← getEnv
  match declName with
  | Name.str parent name _ =>
    if isStructure env parent then
      match getStructureInfo? env parent with
      | some i =>
        match i.fieldNames.find? (· == name) with
        | some _ => pure true
        | none => pure false
      | none => panic! s!"{parent} is not a structure"
    else
      pure false
  | _ => pure false

def ofConstant : (Name × ConstantInfo) → MetaM (Option DocInfo) := λ (name, info) => do
  if (←isBlackListed name) then
    return none
  match info with
  | ConstantInfo.axiomInfo i => pure <| some <| axiomInfo (←AxiomInfo.ofAxiomVal i)
  | ConstantInfo.thmInfo i => pure <| some <| theoremInfo (←TheoremInfo.ofTheoremVal i)
  | ConstantInfo.opaqueInfo i => pure <| some <| opaqueInfo (←OpaqueInfo.ofOpaqueVal i)
  | ConstantInfo.defnInfo i =>
    if ← (isProjFn i.name) then
      pure none
    else
      if (←isInstance i.name) then
        let info ← InstanceInfo.ofDefinitionVal i
        pure <| some <| instanceInfo info
      else
        let info ← DefinitionInfo.ofDefinitionVal i
        pure <| some <| definitionInfo  info
  | ConstantInfo.inductInfo i =>
    let env ← getEnv
    if isStructure env i.name then
      if isClass env i.name then
        pure <| some <| classInfo (←ClassInfo.ofInductiveVal i)
      else
        pure <| some <| structureInfo (←StructureInfo.ofInductiveVal i)
    else
      if isClass env i.name then
        pure <| some <| classInductiveInfo (←ClassInductiveInfo.ofInductiveVal i)
      else
        pure <| some <| inductiveInfo (←InductiveInfo.ofInductiveVal i)
  -- we ignore these for now
  | ConstantInfo.ctorInfo i => pure none
  | ConstantInfo.recInfo i => pure none
  | ConstantInfo.quotInfo i => pure none

def getName : DocInfo → Name
| axiomInfo i => i.name
| theoremInfo i => i.name
| opaqueInfo i => i.name
| definitionInfo i => i.name
| instanceInfo i => i.name
| inductiveInfo i => i.name
| structureInfo i => i.name
| classInfo i => i.name
| classInductiveInfo i => i.name

def getKind : DocInfo → String
| axiomInfo _ => "axiom"
| theoremInfo _ => "theorem"
| opaqueInfo _ => "constant"
| definitionInfo _ => "def"
| instanceInfo _ => "instance"
| inductiveInfo _ => "inductive"
| structureInfo _ => "structure"
| classInfo _ => "class"
| classInductiveInfo _ => "class"

def getKindDescription : DocInfo → String
| axiomInfo i => if i.isUnsafe then "unsafe axiom" else "axiom"
| theoremInfo _ => "theorem"
| opaqueInfo i =>
  match i.unsafeInformation with
  | DefinitionSafety.safe => "constant"
  | DefinitionSafety.unsafe => "unsafe constant"
  | DefinitionSafety.partial => "partial def"
| definitionInfo i => Id.run do
  if i.hints.isAbbrev then
    pure "abbrev"
  else
    let mut modifiers := #[]
    if i.isUnsafe then
      modifiers := modifiers.push "unsafe"
    if i.isNonComputable then
      modifiers := modifiers.push "noncomputable"

    modifiers := modifiers.push "def"
    pure $ String.intercalate " " modifiers.toList
| instanceInfo i => Id.run do
  let mut modifiers := #[]
  if i.isUnsafe then
    modifiers := modifiers.push "unsafe"
  if i.isNonComputable then
    modifiers := modifiers.push "noncomputable"

  modifiers := modifiers.push "instance"
  pure $ String.intercalate " " modifiers.toList
| inductiveInfo i => if i.isUnsafe then "unsafe inductive" else "inductive"
| structureInfo _ => "structure"
| classInfo _ => "class"
| classInductiveInfo _ => "class inductive"

def getType : DocInfo → CodeWithInfos
| axiomInfo i => i.type
| theoremInfo i => i.type
| opaqueInfo i => i.type
| definitionInfo i => i.type
| instanceInfo i => i.type
| inductiveInfo i => i.type
| structureInfo i => i.type
| classInfo i => i.type
| classInductiveInfo i => i.type

def getArgs : DocInfo → Array Arg
| axiomInfo i => i.args
| theoremInfo i => i.args
| opaqueInfo i => i.args
| definitionInfo i => i.args
| instanceInfo i => i.args
| inductiveInfo i => i.args
| structureInfo i => i.args
| classInfo i => i.args
| classInductiveInfo i => i.args

def getAttrs : DocInfo → Array String
| axiomInfo i => i.attrs
| theoremInfo i => i.attrs
| opaqueInfo i => i.attrs
| definitionInfo i => i.attrs
| instanceInfo i => i.attrs
| inductiveInfo i => i.attrs
| structureInfo i => i.attrs
| classInfo i => i.attrs
| classInductiveInfo i => i.attrs

def getDocString : DocInfo → Option String
| axiomInfo i => i.doc
| theoremInfo i => i.doc
| opaqueInfo i => i.doc
| definitionInfo i => i.doc
| instanceInfo i => i.doc
| inductiveInfo i => i.doc
| structureInfo i => i.doc
| classInfo i => i.doc
| classInductiveInfo i => i.doc

end DocInfo

namespace ModuleMember

def getDeclarationRange : ModuleMember → DeclarationRange
| docInfo i => i.getDeclarationRange
| modDoc i => i.declarationRange

def order (l r : ModuleMember) : Bool :=
  Position.lt l.getDeclarationRange.pos r.getDeclarationRange.pos

def getName : ModuleMember → Name
| docInfo i => i.getName
| modDoc i => Name.anonymous

def getDocString : ModuleMember → Option String
| docInfo i => i.getDocString
| modDoc i => i.doc

end ModuleMember

def filterMapDocInfo (ms : Array ModuleMember) : Array DocInfo :=
  ms.filterMap filter
  where
    filter : ModuleMember → Option DocInfo
    | ModuleMember.docInfo i => some i
    | _ => none

structure AnalyzerResult where
  name2ModIdx : HashMap Name ModuleIdx
  moduleNames : Array Name
  moduleInfo : HashMap Name Module
  hierarchy : Hierarchy
  -- Indexed by ModIdx
  importAdj : Array (Array Bool)
  deriving Inhabited

deriving instance Inhabited for ModuleDoc

def process : MetaM AnalyzerResult := do
  let env ← getEnv
  let mut res := mkHashMap env.header.moduleNames.size
  for module in env.header.moduleNames do
    let modDocs := match getModuleDoc? env module with
    | none => #[]
    | some ds => ds
    |>.map (λ doc => ModuleMember.modDoc doc)
    res := res.insert module (Module.mk module modDocs)

  for cinfo in env.constants.toList do
    try
      let analysis := Prod.fst <$> Meta.MetaM.toIO (DocInfo.ofConstant cinfo) { maxHeartbeats := 5000000, options := ⟨[(`pp.tagAppFns, true)]⟩ } { env := env} {} {}
      if let some dinfo ← analysis then
        let some modidx := env.getModuleIdxFor? dinfo.getName | unreachable!
        let moduleName := env.allImportedModuleNames.get! modidx
        let module := res.find! moduleName
        res := res.insert moduleName {module with members := module.members.push (ModuleMember.docInfo dinfo)}
    catch e =>
      IO.println s!"WARNING: Failed to obtain information for: {cinfo.fst}: {←e.toMessageData.toString}"

  -- TODO: This is definitely not the most efficient way to store this data
  let mut adj := Array.mkArray res.size (Array.mkArray res.size false)
  -- TODO: This could probably be faster if we did an insertion sort above instead
  for (moduleName, module) in res.toArray do
    res := res.insert moduleName {module with members := module.members.qsort ModuleMember.order}
    let some modIdx := env.getModuleIdx? moduleName | unreachable!
    let moduleData := env.header.moduleData.get! modIdx
    for imp in moduleData.imports do
      let some importIdx := env.getModuleIdx? imp.module | unreachable!
      adj := adj.set! modIdx (adj.get! modIdx |>.set! importIdx true)

  pure {
    name2ModIdx := env.const2ModIdx,
    moduleNames := env.header.moduleNames,
    moduleInfo := res,
    hierarchy := Hierarchy.fromArray env.header.moduleNames,
    importAdj := adj
  }

end DocGen4
