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
      <p>This was built using Lean 4 at commit <a href={s!"https://github.com/leanprover/lean4/tree/{Lean.githash}"}>{Lean.githash}</a></p>
    </main>

end Output
end DocGen4
