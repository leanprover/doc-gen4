/-
Copyright (c) 2021 Henrik Böving. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Henrik Böving
-/
import DocGen4.Output.ToHtmlFormat
import DocGen4.Output.Template

namespace DocGen4
namespace Output

open scoped DocGen4.Jsx

def index : BaseHtmlM Html := do templateExtends (baseHtml "Index") <|
  pure <|
    <main>
      <a id="top"></a>
      <h1> Welcome to the documentation page </h1>
      -- Temporary comment until the lake issue is resolved
      -- for commit <a href={s!"{← getProjectGithubUrl}/tree/{← getProjectCommit}"}>{s!"{← getProjectCommit} "}</a>
      <p>Welcome to the documentation for Lean 4 and any libraries shown on the left sidebar. These may include:</p>
      <ul>
        <li>Aesop: Automated Extensible Search for Obvious Proofs (AESOP) is a community-maintained proof search tactic for mathlib4, located <a href="https://github.com/leanprover-community/aesop">here</a>.</li>
        <li>Archive: Some archived parts of the community-maintained Mathlib, located <a href="https://github.com/leanprover-community/mathlib4">here</a>.</li>
        <li>Counterexamples: A community-maintained part of Mathlib, located <a href="https://github.com/leanprover-community/mathlib4">here</a>.</li>
        <li>Init: Initial code loaded by Lean 4, located <a href="https://github.com/leanprover/lean4">here</a>.</li>
        <li>Mathlib: The community-maintained Mathlib, located <a href="https://github.com/leanprover-community/mathlib4">here</a>.</li>
        <li>ProofWidgets: A community-maintained library of user interface components for Lean 4, located <a href="https://github.com/leanprover-community/ProofWidgets4">here</a>.</li>
        <li>QQ: A community-maintained library of type-safe expression quotations, located <a href="https://github.com/leanprover-community/ProofWidgets4">here</a>.</li>
        <li>Std: The standard library of Lean 4, located <a href="https://github.com/leanprover/std4">here</a>.</li>
        <li>docs: The conversion tactic for the community-maintained Mathlib, located <a href="https://github.com/leanprover-community/mathlib4">here</a>.</li>
      </ul>
      <p>This was built using Lean 4 at commit <a href={s!"https://github.com/leanprover/lean4/tree/{Lean.githash}"}>{Lean.githash}</a></p>
      
    </main>

end Output
end DocGen4
