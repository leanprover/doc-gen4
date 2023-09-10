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
  return { name := n, type := ← prettyPrintTerm t, doc := ← findDocString? env n}

partial def typeToArgsType (e : Expr) : (Array ((Option Name) × Expr × BinderInfo) × Expr) :=
  let helper := fun name type body data =>
    -- Once we hit a name with a macro scope we stop traversing the expression
    -- and print what is left after the : instead. The only exception
    -- to this is instances since these almost never have a name
    -- but should still be printed as arguments instead of after the :.
    if data.isInstImplicit && name.hasMacroScopes then
      let arg := (none, type, data)
      let (args, final) := typeToArgsType (Expr.instantiate1 body (mkFVar ⟨name⟩))
      (#[arg] ++ args, final)
    else if name.hasMacroScopes then
      (#[], e)
    else
      let arg := (some name, type, data)
      let (args, final) := typeToArgsType (Expr.instantiate1 body (mkFVar ⟨name⟩))
      (#[arg] ++ args, final)
  match e.consumeMData with
  | Expr.forallE name type body binderInfo => helper name type body binderInfo
  | _ => (#[], e)

def Info.ofConstantVal (v : ConstantVal) : MetaM Info := do
  let (args, type) := typeToArgsType v.type
  let args ← args.mapM (fun (n, e, b) => do return Arg.mk n (← prettyPrintTerm e) b)
  let nameInfo ← NameInfo.ofTypedName v.name type
  match ← findDeclarationRanges? v.name with
  -- TODO: Maybe selection range is more relevant? Figure this out in the future
  | some range => return {
      toNameInfo := nameInfo,
      args,
      declarationRange := range.range,
      attrs := ← getAllAttributes v.name
    }
  | none => panic! s!"{v.name} is a declaration without position"

end DocGen4.Process
