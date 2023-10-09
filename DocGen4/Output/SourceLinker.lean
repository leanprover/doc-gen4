/-
Copyright (c) 2022 Henrik Böving. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Henrik Böving
-/
import Lean

namespace DocGen4.Output.SourceLinker

open Lean

/--
Given a lake workspace with all the dependencies as well as the hash of the
compiler release to work with this provides a function to turn names of
declarations into (optionally positional) Github URLs.
-/
def sourceLinker (gitUrl? : Option String) : IO (Name → Option DeclarationRange → String) := do
  -- TOOD: Refactor this, we don't need to pass in the module into the returned closure
  -- since we have one sourceLinker per module
  return fun module range =>
    let parts := module.components.map Name.toString
    let path := String.intercalate "/" parts
    let root := module.getRoot
    let leanHash := Lean.githash
    let basic := if root == `Lean ∨ root == `Init then
      s!"https://github.com/leanprover/lean4/blob/{leanHash}/src/{path}.lean"
    else if root == `Lake then
      s!"https://github.com/leanprover/lean4/blob/{leanHash}/src/lake/{path}.lean"
    else
      gitUrl?.get!

    match range with
    | some range => s!"{basic}#L{range.pos.line}-L{range.endPos.line}"
    | none => basic

end DocGen4.Output.SourceLinker
