/-
Copyright (c) 2022 Henrik Böving. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Henrik Böving
-/
import Lean

import DocGen4.Process.Base
import DocGen4.Process.Attributes

namespace DocGen4.Process
open Lean Meta

def NameInfo.ofTypedName (n : Name) (t : Expr) : MetaM NameInfo := do
  let env ← getEnv
  pure { name := n, type := ←prettyPrintTerm t, doc := ←findDocString? env n}

partial def typeToArgsType (e : Expr) : (Array (Name × Expr × BinderInfo) × Expr) :=
  let helper := λ name type body data =>
    -- Once we hit a name with a macro scope we stop traversing the expression
    -- and print what is left after the : instead. The only exception
    -- to this is instances since these almost never have a name
    -- but should still be printed as arguments instead of after the :.
    if name.hasMacroScopes ∧ ¬data.binderInfo.isInstImplicit then
      (#[], e)
    else
      let name := name.eraseMacroScopes
      let arg := (name, type, data.binderInfo)
      let (args, final) := typeToArgsType (Expr.instantiate1 body (mkFVar ⟨name⟩))
      (#[arg] ++ args, final)
  match e.consumeMData with
  | Expr.lam name type body data => helper name type body data
  | Expr.forallE name type body data => helper name type body data
  | _ => (#[], e)

def Info.ofConstantVal (v : ConstantVal) : MetaM Info := do
  let env ← getEnv
  let (args, type) := typeToArgsType v.type
  let args ← args.mapM (λ (n, e, b) => do pure $ Arg.mk n (←prettyPrintTerm e) b)
  let doc ← findDocString? env v.name
  let nameInfo ← NameInfo.ofTypedName v.name type
  match ←findDeclarationRanges? v.name with
  -- TODO: Maybe selection range is more relevant? Figure this out in the future
  | some range => pure $ Info.mk nameInfo args range.range (←getAllAttributes v.name)
  | none => panic! s!"{v.name} is a declaration without position"

end DocGen4.Process
