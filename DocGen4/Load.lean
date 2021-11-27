import Lean
import DocGen4.Process

namespace DocGen4

open Lean System

def printSearchPath : IO PUnit := do
  IO.println s!"{←searchPathRef.get}"

def setSearchPath (path : List FilePath) : IO PUnit := do
    searchPathRef.set path

def load (imports : List Name) : IO (List DocInfo) := do
  let env ← importModules (List.map (Import.mk · false) imports) Options.empty
  let doc ← Prod.fst <$> (Meta.MetaM.toIO (process env) {} { env := env} {} {})
  for i in doc do
    let s ← Core.CoreM.toIO i.prettyPrint {} { env := env }
    IO.println s.fst
  return doc

end DocGen4
