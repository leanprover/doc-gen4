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

inductive DocInfo where
| axiomInfo (info : AxiomInfo) : DocInfo
| theoremInfo (info : TheoremInfo) : DocInfo
| opaqueInfo (info : OpaqueInfo) : DocInfo
| definitionInfo : DocInfo
| mutualInductiveInfo : DocInfo
| inductiveInfo : DocInfo
| structureInfo : DocInfo
| classInfo : DocInfo
| classInductiveInfo : DocInfo
deriving Repr

structure Module where
  name : Name
  doc : Option String
  members : Array DocInfo
  deriving Inhabited, Repr

def prettyPrintTerm (expr : Expr) : MetaM Syntax :=
  delab Name.anonymous [] expr

def Info.ofConstantVal (v : ConstantVal) : MetaM Info := do
  let env ← getEnv
  let type := (←prettyPrintTerm v.type )
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

namespace DocInfo

private def isBlackListed (declName : Name) : MetaM Bool := do
  let env ← getEnv
  (declName.isInternal && !isPrivateName declName)
  <||> isAuxRecursor env declName
  <||> isNoConfusion env declName
  <||> isRec declName
  <||> isMatcher declName

-- TODO: Figure out how to associate names etc. with where they actually came from module wise
def ofConstant : (Name × ConstantInfo) → MetaM (Option DocInfo) := λ (name, info) => do
  if (←isBlackListed name) then
    return none
  match info with
  | ConstantInfo.axiomInfo i => some $ axiomInfo (←AxiomInfo.ofAxiomVal i)
  | ConstantInfo.thmInfo i => some $ theoremInfo (←TheoremInfo.ofTheoremVal i)
  | ConstantInfo.opaqueInfo i => some $ opaqueInfo (←OpaqueInfo.ofOpaqueVal i)
  -- TODO: Find a way to extract equations nicely
  | ConstantInfo.defnInfo i => none
  -- TODO: Differentiate between all the different types of inductives (structures, classes etc.)
  | ConstantInfo.inductInfo i => none
  -- we ignore these for now
  | ConstantInfo.ctorInfo i => none
  | ConstantInfo.recInfo i => none
  | ConstantInfo.quotInfo i => none


def prettyPrint (i : DocInfo) : CoreM String := do
  match i with
  | axiomInfo i => s!"axiom {i.name} : {←PrettyPrinter.formatTerm i.type}, doc string: {i.doc}"
  | theoremInfo i => s!"theorem {i.name} : {←PrettyPrinter.formatTerm i.type}, doc string: {i.doc}"
  | opaqueInfo i => s!"constant {i.name} : {←PrettyPrinter.formatTerm i.type}, doc string: {i.doc}"
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
