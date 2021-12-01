import Lean
import DocGen4.Process
import Std.Data.HashMap

namespace DocGen4

open Lean System Std

def printSearchPath : IO PUnit := do
  IO.println s!"{←searchPathRef.get}"

def setSearchPath (path : List FilePath) : IO PUnit := do
    searchPathRef.set path

def load (imports : List Name) : IO (HashMap Name Module) := do
  let env ← importModules (List.map (Import.mk · false) imports) Options.empty
  -- TODO parameterize maxHeartbeats
  let doc ← Prod.fst <$> (Meta.MetaM.toIO (process) { maxHeartbeats := 100000000} { env := env} {} {})
  for (_, module) in doc.toList do
    let s ← Core.CoreM.toIO module.prettyPrint {} { env := env }
    IO.println s.fst
  IO.println s!"Processed {List.foldl (λ a (_, b) => a + b.members.size) 0 doc.toList} declarations"
  IO.println s!"Processed {doc.size} modules"
  return doc

end DocGen4
