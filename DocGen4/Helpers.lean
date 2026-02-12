import Std.Data.Iterators

namespace DocGen4

open Std Iterators

structure ChunkArray α where
  array : Array α
  chunkSize : Nat
  curr : Nat
  chunkSize_gt_zero : chunkSize > 0 := by grind
  curr_valid : curr ≤ array.size := by grind

def chunked (xs : Array α) (n : Nat) (ok : n > 0 := by grind) :=
  IterM.mk (ChunkArray.mk xs n 0) Id (Subarray α)

def ChunkArray.PlausibleStep (it : IterM (α := ChunkArray α) m (Subarray α)) :
    (step : IterStep (IterM (α := ChunkArray α) m (Subarray α)) (Subarray α)) → Prop
  | .yield it' v  =>
    it.internalState.curr < it.internalState.array.size ∧
    it.internalState.array = it'.internalState.array ∧
    it.internalState.chunkSize = it'.internalState.chunkSize ∧
    it.internalState.curr < it'.internalState.curr ∧
    v.size ≤ it.internalState.chunkSize ∧
    v.array = it.internalState.array
  | .done => it.internalState.curr = it.internalState.array.size
  | .skip .. => False

instance [Pure m] : Iterator (ChunkArray α) m (Subarray α) where
  IsPlausibleStep := ChunkArray.PlausibleStep
  step it :=
    let { internalState := { array, chunkSize, chunkSize_gt_zero, curr, curr_valid } } := it
    if h : curr = array.size then
      pure <| .deflate <| .done h
    else
      let curr' := curr + chunkSize
      let curr'' := if curr' > array.size then array.size else curr'
      let it' : IterM (α := ChunkArray α) _ _ := ⟨{ array, chunkSize, curr := curr'' }⟩
      pure <| .deflate <| .yield it' (array[curr...curr'']) (by grind [ChunkArray.PlausibleStep])

instance [Pure m] [Monad n] : IteratorLoop (ChunkArray α) (β := Subarray α) m n := IteratorLoop.defaultImplementation

def chunksOf (xs : Array α) (chunkSize : Nat) (_ok : chunkSize > 0 := by grind) : Array (Array α) := Id.run do
  let mut out := #[]
  let mut n := 0
  while n < xs.size do
    out := out.push <| xs.extract n (n + chunkSize)
    n := n + chunkSize
  return out
