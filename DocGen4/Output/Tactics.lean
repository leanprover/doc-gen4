/-
Copyright (c) 2025 Anne Baanen. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Anne Baanen
-/
import DocGen4.Process.Analyze
import DocGen4.Output.Module

namespace DocGen4.Process

open scoped DocGen4.Jsx
open DocGen4 Output Lean

/--
Render the HTML for a single tactic.
-/
def TacticInfo.docStringToHtml (tac : TacticInfo MarkdownDocstring) : Output.HtmlM (TacticInfo Html) := do
  return {
    tac with
    docString := <p>[← Output.docStringToHtml (.inl tac.docString) tac.internalName.toString]</p>
  }

/--
Render the HTML for a single tactic.
-/
def TacticInfo.toHtml (tac : TacticInfo Html) : Output.BaseHtmlM Html := do
  let internalName := tac.internalName.toString
  let defLink := (← moduleNameToLink tac.definingModule) ++ "#" ++ internalName
  let tags := ", ".intercalate (tac.tags.map (·.toString)).qsort.toList
  return <div id={internalName}>
    <h2>{tac.userName}</h2>
    {tac.docString}
    <dl>
      <dt>Tags:</dt>
      <dd>{tags}</dd>
      <dt>Defined in module:</dt>
      <dd><a href={defLink}>{tac.definingModule.toString}</a></dd>
    </dl>
  </div>

def TacticInfo.navLink (tac : TacticInfo α) : Html :=
  <p><a href={"#".append tac.internalName.toString}>{tac.userName}</a></p>

end DocGen4.Process

namespace DocGen4.Output

open scoped DocGen4.Jsx
open Lean Process

/--
Render the HTML for the tactics listing page.
-/
def tactics (tacticInfo : Array (TacticInfo Html)) : BaseHtmlM Html := do
  let sectionsHtml ← tacticInfo.mapM (· |>.toHtml)
  templateLiftExtends (baseHtmlGenerator "Tactics") <| pure #[
    <nav class="internal_nav">
      <p><a href="#top">return to top</a></p>
      [tacticInfo.map (· |>.navLink)]
    </nav>,
    Html.element "main" false #[] (
      #[<p>The tactic language is a special-purpose programming language for constructing proofs, indicated using the keyword <code>by</code>.</p>] ++
      sectionsHtml)
  ]

end Output
end DocGen4
