import Lean
import Lean.Meta.Match.MatcherInfo
import Lean.PrettyPrinter

namespace DocGen4

open Lean Meta PrettyPrinter

def prettyPrintTerm (expr : Expr) : MetaM Syntax :=
  delab Name.anonymous [] expr

structure Info where
  name : Name
  type : Syntax
  doc : Option String
  module : Name
  deriving Repr

def Info.ofConstantVal (v : ConstantVal) : MetaM Info := do
  let env ← getEnv
  let type := (←prettyPrintTerm v.type )
  let doc := findDocString? env v.name
  match (env.getModuleIdxFor? v.name) with
  | some modidx =>
    let module := env.allImportedModuleNames.get! modidx
    return Info.mk v.name type doc module
  | none => panic! "impossible"

structure AxiomInfo extends Info where
  isUnsafe : Bool
  deriving Repr

def AxiomInfo.ofAxiomVal (v : AxiomVal) : MetaM AxiomInfo := do
  let info ← Info.ofConstantVal v.toConstantVal
  return AxiomInfo.mk info v.isUnsafe

structure TheoremInfo extends Info where
  deriving Repr

def TheoremInfo.ofTheoremVal (v : TheoremVal) : MetaM TheoremInfo := do
  let info ← Info.ofConstantVal v.toConstantVal
  return TheoremInfo.mk info

structure OpaqueInfo extends Info where
  value : Syntax
  isUnsafe : Bool
  deriving Repr

def OpaqueInfo.ofOpaqueVal (v : OpaqueVal) : MetaM OpaqueInfo := do
  let info ← Info.ofConstantVal v.toConstantVal
  let value ← prettyPrintTerm v.value
  return OpaqueInfo.mk info value v.isUnsafe
  
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
  | axiomInfo i => s!"axiom {i.name} : {←PrettyPrinter.formatTerm i.type} from {i.module}, doc string: {i.doc}"
  | theoremInfo i => s!"theorem {i.name} : {←PrettyPrinter.formatTerm i.type} from {i.module}, doc string: {i.doc}"
  | opaqueInfo i => s!"constant {i.name} : {←PrettyPrinter.formatTerm i.type} from {i.module}, doc string: {i.doc}"
  | _ => ""

end DocInfo

def process : Environment → MetaM (List DocInfo) := λ env => do
  let mut res := []
  for cinfo in env.constants.toList do
    let dinfo := ←DocInfo.ofConstant cinfo
    match dinfo with
    | some d => res := d :: res
    | none => ()
  return res

end DocGen4
