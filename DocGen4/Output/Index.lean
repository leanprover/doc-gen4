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
      <h1> Mathlib4 Documentation </h1>
      <p>This is the API reference for mathlib 3, the library of mathematics developed for Lean 3. If you need information about installing Lean or mathlib, or getting started with a project, please visit our <a href="https://leanprover-community.github.io">community website</a></p>
      <p>For the now deprecated Lean 3 version, see <a href="https://leanprover-community.github.io/mathlib_docs">mathlib3's documentation</a>.</p>
      -- Temporary comment until the lake issue is resolved
      -- for commit <a href={s!"{← getProjectGithubUrl}/tree/{← getProjectCommit}"}>{s!"{← getProjectCommit} "}</a>
      <p>This was built using Lean 4 at commit <a href={s!"https://github.com/leanprover/lean4/tree/{Lean.githash}"}>{Lean.githash}</a></p>
    </main>

end Output
end DocGen4
