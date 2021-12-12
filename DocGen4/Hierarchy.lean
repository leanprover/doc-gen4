import Lean
import Std.Data.HashMap

namespace DocGen4

open Lean Std Name

def getDepth : Name → Nat
| Name.anonymous => 0
| Name.str p _ _ => getDepth p + 1
| Name.num p _ _ => getDepth p + 1

def getNLevels (name : Name) (levels: Nat) : Name :=
    (components.drop (components.length - levels)).reverse.foldl (· ++ ·) Name.anonymous
  where
    components := name.components'

inductive Hierarchy where
| node : Name → RBNode Name (λ _ => Hierarchy) → Hierarchy

instance : Inhabited Hierarchy := ⟨Hierarchy.node Name.anonymous RBNode.leaf⟩

abbrev HierarchyMap := RBNode Name (λ _ => Hierarchy)

-- Everything in this namespace is adapted from stdlib's RBNode
namespace HierarchyMap

def toList : HierarchyMap → List (Name × Hierarchy)
| t => t.revFold (fun ps k v => (k, v)::ps) []

def hForIn [Monad m] (t : HierarchyMap) (init : σ) (f : (Name × Hierarchy) → σ → m (ForInStep σ)) : m σ :=
  t.forIn init (fun a b acc => f (a, b) acc)

instance : ForIn m HierarchyMap (Name × Hierarchy) where
  forIn := HierarchyMap.hForIn

end HierarchyMap

namespace Hierarchy

def empty (n : Name) : Hierarchy := node n RBNode.leaf

def getName : Hierarchy → Name
| node n _ => n

def getChildren : Hierarchy → HierarchyMap
| node _ c => c

partial def insert! (h : Hierarchy) (n : Name) : Hierarchy := do
  let hn := h.getName
  let mut cs := h.getChildren
  if getDepth hn ≥ getDepth n then
    panic! "Invalid insert"
  else if getDepth hn + 1 == getDepth n then
    match cs.find Name.cmp n with
    | none =>
      node hn (cs.insert Name.cmp n $ empty n)
    | some _ => h
  else
    let leveledName := getNLevels n (getDepth hn + 1)
    match cs.find Name.cmp leveledName with
    | some nextLevel =>
      cs := cs.erase Name.cmp leveledName
      node hn $ cs.insert Name.cmp leveledName (nextLevel.insert! n)
    | none =>
      let child := (insert! (empty leveledName) n)
      node hn $ cs.insert Name.cmp leveledName child

partial def fromArray (names : Array Name) : Hierarchy := names.foldl insert! (empty anonymous)

end Hierarchy
end DocGen4
