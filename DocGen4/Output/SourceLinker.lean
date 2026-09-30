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
  -- Note. We may want to verify Lean column numbers match VSCode on complex unicode
  -- characters.  There could be encoding mismatches.
  | some range => s!"{baseUrl}:{range.pos.line}:{range.pos.column+1}"
  | none => baseUrl

/--
Returns the path of a core module's source file relative to the `src` directory of the Lean
repository, with `/` separators, or `none` for modules outside `Init`, `Std`, `Lean` and `Lake`.
-/
def coreSourcePath? (module : Name) : Option String :=
  let root := module.getRoot
  let path := "/".intercalate (module.components.map (Name.toString (escape := false)))
  if root == `Lean ∨ root == `Init ∨ root == `Std then
    some s!"{path}.lean"
  else if root == `Lake then
    some s!"lake/{path}.lean"
  else
    none

/--
Given a lake workspace with all the dependencies as well as the hash of the
compiler release to work with this provides a function to turn names of
declarations into (optionally positional) Github URLs.
-/
def sourceLinker (gitUrl? : Option String) (module : Name) : Option DeclarationRange → String :=
  if let some path := coreSourcePath? module then
    mkGithubSourceLinker s!"https://github.com/leanprover/lean4/blob/{Lean.githash}/src/{path}"
  else
    match gitUrl? with
    | .some url =>
      if url.startsWith "vscode://file/" then
        mkVscodeSourceLinker url
      else if url.startsWith "https://github.com" then
        mkGithubSourceLinker url
      else
        -- Other urls do not have range added.
        fun _ => url
    | .none => panic! s!"Github URL must be defined for {module}."

end DocGen4.Output.SourceLinker
