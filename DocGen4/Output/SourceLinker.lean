/-
Copyright (c) 2022 Henrik Böving. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Henrik Böving
-/
import Lean

namespace DocGen4.Output.SourceLinker

open Lean

def mkGithubSourceLinker (baseUrl : String) (range : Option DeclarationRange) : String :=
  match range with
  | some range => s!"{baseUrl}#L{range.pos.line}-L{range.endPos.line}"
  | none => baseUrl

def mkVscodeSourceLinker (baseUrl : String) (range : Option DeclarationRange) : String :=
  match range with
  -- Note. We may want to verify Lean line and column numbers match VSCode.
  | some range => s!"{baseUrl}:{range.pos.line}:{range.pos.column}"
  | none => baseUrl

/--
Given a lake workspace with all the dependencies as well as the hash of the
compiler release to work with this provides a function to turn names of
declarations into (optionally positional) Github URLs.
-/
def sourceLinker (gitUrl? : Option String) (module : Name) : Option DeclarationRange → String :=
  let root := module.getRoot
  let leanHash := Lean.githash
  if root == `Lean ∨ root == `Init then
    let parts := module.components.map Name.toString
    let path := String.intercalate "/" parts
    mkGithubSourceLinker s!"https://github.com/leanprover/lean4/blob/{leanHash}/src/{path}.lean"
  else if root == `Lake then
    let parts := module.components.map Name.toString
    let path := String.intercalate "/" parts
    mkGithubSourceLinker s!"https://github.com/leanprover/lean4/blob/{leanHash}/src/lake/{path}.lean"
  else
    match gitUrl? with
    | .some url =>
      if url.startsWith "vscode://file/" then
        mkVscodeSourceLinker url
      else
        mkGithubSourceLinker url
    | .none => panic! s!"Github URL must be defined for {module}."

end DocGen4.Output.SourceLinker
