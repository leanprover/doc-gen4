import Lean
import Lean.Meta.Match.MatcherInfo
import Lean.PrettyPrinter
import Std.Data.HashMap

namespace DocGen4

open Lean Meta PrettyPrinter Std

structure Info where
  name : Name
  type : Syntax
  doc : Option String
  deriving Repr

structure AxiomInfo extends Info where
  isUnsafe : Bool
  deriving Repr

structure TheoremInfo extends Info where
  deriving Repr

structure OpaqueInfo extends Info where
  value : Syntax
  isUnsafe : Bool
  deriving Repr

structure DefinitionInfo extends Info where
  --value : Syntax
  unsafeInformation : DefinitionSafety
  hints : ReducibilityHints

structure InductiveInfo extends Info where
  numParams : Nat     -- Number of parameters
  numIndices : Nat    -- Number of indices
  all : List Name     -- List of all (including this one) inductive datatypes in the mutual declaration containing this one
  ctors : List (Name × Syntax)   -- List of all constructors and their type for this inductive datatype
  isRec : Bool        -- `true` Iff it is recursive
  isUnsafe : Bool
  isReflexive : Bool
  isNested : Bool
  deriving Repr

structure StructureInfo extends Info where
  -- TODO: Later on we probably also want the type of projection fns etc.
  fieldInfo : Array StructureFieldInfo
  parents : Array Name
  ctor : (Name × Syntax)
  deriving Repr

inductive DocInfo where
| axiomInfo (info : AxiomInfo) : DocInfo
| theoremInfo (info : TheoremInfo) : DocInfo
| opaqueInfo (info : OpaqueInfo) : DocInfo
| definitionInfo (info : DefinitionInfo) : DocInfo
| inductiveInfo (info : InductiveInfo) : DocInfo
| structureInfo (info : StructureInfo) : DocInfo
| classInfo : DocInfo
| classInductiveInfo : DocInfo

structure Module where
  name : Name
  doc : Option String
  members : Array DocInfo
  deriving Inhabited

def prettyPrintTerm (expr : Expr) : MetaM Syntax :=
  delab Name.anonymous [] expr

def Info.ofConstantVal (v : ConstantVal) : MetaM Info := do
  let env ← getEnv
  let type := (←prettyPrintTerm v.type)
  let doc := findDocString? env v.name
  return Info.mk v.name type doc

def AxiomInfo.ofAxiomVal (v : AxiomVal) : MetaM AxiomInfo := do
  let info ← Info.ofConstantVal v.toConstantVal
  return AxiomInfo.mk info v.isUnsafe

def TheoremInfo.ofTheoremVal (v : TheoremVal) : MetaM TheoremInfo := do
  let info ← Info.ofConstantVal v.toConstantVal
  return TheoremInfo.mk info

def OpaqueInfo.ofOpaqueVal (v : OpaqueVal) : MetaM OpaqueInfo := do
  let info ← Info.ofConstantVal v.toConstantVal
  let value ← prettyPrintTerm v.value
  return OpaqueInfo.mk info value v.isUnsafe

def DefinitionInfo.ofDefinitionVal (v : DefinitionVal) : MetaM DefinitionInfo := do
  let info ← Info.ofConstantVal v.toConstantVal
  -- Elaborating the value yields weird exceptions
  --let value ← prettyPrintTerm v.value
  return DefinitionInfo.mk info v.safety v.hints

def getConstructorType (ctor : Name) : MetaM Syntax := do
  let env ← getEnv
  match env.find? ctor with
  | some (ConstantInfo.ctorInfo i) => ←prettyPrintTerm i.type
  | _ => panic! s!"Constructor {ctor} was requested but does not exist"

def InductiveInfo.ofInductiveVal (v : InductiveVal) : MetaM InductiveInfo := do
  let info ← Info.ofConstantVal v.toConstantVal
  let env ← getEnv
  let ctors ← List.mapM (λ name => do (name, ←getConstructorType name)) v.ctors
  return InductiveInfo.mk info v.numParams v.numIndices v.all ctors v.isRec v.isUnsafe v.isReflexive v.isNested

def StructureInfo.ofInductiveVal (v : InductiveVal) : MetaM StructureInfo := do
  let info ← Info.ofConstantVal v.toConstantVal
  let env ← getEnv
  let parents := getParentStructures env v.name
  let ctor := getStructureCtor env v.name |>.name
  let ctorType ← getConstructorType ctor
  match getStructureInfo? env v.name with
  | some i => return StructureInfo.mk info i.fieldInfo parents (ctor, ctorType)
  | none => panic! s!"{v.name} is not a structure"

namespace DocInfo

def isBlackListed (declName : Name) : MetaM Bool := do
  let env ← getEnv
  declName.isInternal
  <||> isAuxRecursor env declName
  <||> isNoConfusion env declName
  <||> isRec declName
  <||> isMatcher declName

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
  -- TODO: Instances are definitions as well (for example coeTrans), figure out:
  -- - how we can know whether they are instances
  -- - how we can access unnamed instances (they probably have internal names?, change the blacklist?)
  -- TODO: Filter out projection fns
  | ConstantInfo.defnInfo i =>
    if (←isProjFn i.name) then
      none
    else
      some $ definitionInfo (←DefinitionInfo.ofDefinitionVal i)
  -- TODO: Differentiate between all the different types of inductives (structures, classes etc.)
  | ConstantInfo.inductInfo i =>
    if isStructure (←getEnv) i.name then
      some $ structureInfo (←StructureInfo.ofInductiveVal i)
    else
      some $ inductiveInfo (←InductiveInfo.ofInductiveVal i)
  -- we ignore these for now
  | ConstantInfo.ctorInfo i => none
  | ConstantInfo.recInfo i => none
  | ConstantInfo.quotInfo i => none


def prettyPrint (i : DocInfo) : CoreM String := do
  match i with
  | axiomInfo i => s!"axiom {i.name} : {←PrettyPrinter.formatTerm i.type}, doc string: {i.doc}"
  | theoremInfo i => s!"theorem {i.name} : {←PrettyPrinter.formatTerm i.type}, doc string: {i.doc}"
  | opaqueInfo i => s!"constant {i.name} : {←PrettyPrinter.formatTerm i.type}, doc string: {i.doc}"
  | definitionInfo i => s!"def {i.name} : {←PrettyPrinter.formatTerm i.type}, doc string: {i.doc}"
  | inductiveInfo i =>
    let ctorString ← i.ctors.mapM (λ (name, type) => do s!"{name} : {←PrettyPrinter.formatTerm type}")
    s!"inductive {i.name} : {←PrettyPrinter.formatTerm i.type}, ctors: {ctorString}, doc string: {i.doc}"
  | structureInfo i =>
    let ctorString ← s!"{i.ctor.fst} : {←PrettyPrinter.formatTerm i.ctor.snd}"
    s!"structure {i.name} : {←PrettyPrinter.formatTerm i.type}, ctor: {ctorString}, doc string: {i.doc}"
  | _ => ""

end DocInfo

namespace Module

def prettyPrint (m : Module) : CoreM String := do
  let pretty := s!"Module {m.name}, doc string: {m.doc} with members:\n"
  Array.foldlM (λ p mem => return p ++ "  " ++ (←mem.prettyPrint) ++ "\n") pretty m.members

end Module

def process : MetaM (HashMap Name Module) := do
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
  return res

end DocGen4
