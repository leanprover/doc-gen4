/-
Copyright (c) 2025 Anne Baanen. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Anne Baanen
-/
import DocGen4.Process.Analyze
import DocGen4.Output.Module

namespace DocGen4.Process

open scoped DocGen4.Jsx
open DocGen4 Lean

/--
Render the HTML for a single tactic.
-/
def TacticInfo.docStringToHtml (tac : TacticInfo MarkdownDocstring) : Output.HtmlM (TacticInfo Html) := do
  return {
    tac with
    docString := <p>[← Output.docStringToHtml tac.docString tac.internalName.toString]</p>
  }

/--
Render the HTML for a single tactic.
-/
def TacticInfo.toHtml (tac : TacticInfo Html) : Output.BaseHtmlM Html := do
  let internalName := tac.internalName.toString
  return <div id={internalName}>
    <h2>{tac.userName}</h2>
    {tac.docString}
    <dl>[
      #[<dt>Defined in module:</dt>, <dd>{tac.definingModule.toString}</dd>]
    ]</dl>
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
      #[<p>The tactic language is a special-purpose programming language for constructing proofs, indicated using the <code>by</code> keyword.</p>] ++
      sectionsHtml)
  ]

def loadTacticsJSON (buildDir : System.FilePath) : IO (Array (TacticInfo Html)) := do
  let mut result : Array (TacticInfo _) := #[]
  for entry in ← System.FilePath.readDir (declarationsBasePath buildDir) do
    if entry.fileName.startsWith "tactics-" && entry.fileName.endsWith ".json" then
      let fileContent ← IO.FS.readFile entry.path
      match Json.parse fileContent with
      | .error err =>
        throw <| IO.userError s!"failed to parse file '{entry.path}' as json: {err}"
      | .ok jsonContent =>
        match fromJson? jsonContent with
        | .error err =>
          throw <| IO.userError s!"failed to parse file '{entry.path}': {err}"
        | .ok (arr : Array (TacticInfo _)) => result := result ++ arr
  return result

/-- Save sections of supplementary pages declared in a specific module.

This `abbrev` exists as a type-checking wrapper around `toJson`, ensuring `loadTacticsJSON` gets
objects in the expected format.
-/
abbrev saveTacticsJSON (fileName : System.FilePath) (tacticInfo : Array (TacticInfo Html)) : IO Unit := do
  if tacticInfo.size > 0 then
    IO.FS.writeFile fileName (toString (toJson tacticInfo))

end Output
end DocGen4
