/-
Copyright (c) 2021 Henrik Böving. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Henrik Böving
-/
import Lean
import Std.Data.HashMap

namespace DocGen4

open Lean Std Name

def getNLevels (name : Name) (levels: Nat) : Name :=
  let components := name.components'
  (components.drop (components.length - levels)).reverse.foldl (· ++ ·) Name.anonymous

inductive Hierarchy where
| node (name : Name) (isFile : Bool) (children : RBNode Name (λ _ => Hierarchy)) : Hierarchy

instance : Inhabited Hierarchy := ⟨Hierarchy.node Name.anonymous false RBNode.leaf⟩

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

def empty (n : Name) (isFile : Bool) : Hierarchy :=
  node n isFile RBNode.leaf

def getName : Hierarchy → Name
| node n _ _ => n

def getChildren : Hierarchy → HierarchyMap
| node _ _ c => c

def isFile : Hierarchy → Bool
| node _ f _ => f

partial def insert! (h : Hierarchy) (n : Name) : Hierarchy := Id.run $ do
  let hn := h.getName
  let mut cs := h.getChildren

  if getNumParts hn + 1 == getNumParts n then
    match cs.find Name.cmp n with
    | none =>
      node hn h.isFile (cs.insert Name.cmp n $ empty n true)
    | some (node _ true _) => h
    | some hierarchy@(node _ false ccs) =>
        cs := cs.erase Name.cmp n
        node hn h.isFile (cs.insert Name.cmp n $ node n true ccs)
  else
    let leveledName := getNLevels n (getNumParts hn + 1)
    match cs.find Name.cmp leveledName with
    | some nextLevel =>
      cs := cs.erase Name.cmp leveledName
      -- BUG?
      node hn h.isFile $ cs.insert Name.cmp leveledName (nextLevel.insert! n)
    | none =>
      let child := (insert! (empty leveledName false) n)
      node hn h.isFile $ cs.insert Name.cmp leveledName child

partial def fromArray (names : Array Name) : Hierarchy :=
  names.foldl insert! (empty anonymous false)

end Hierarchy
end DocGen4
