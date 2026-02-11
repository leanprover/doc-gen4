/-
Copyright (c) 2025 Anne Baanen. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Anne Baanen
-/
import DocGen4.Process.Analyze
import DocGen4.Output.Module

namespace DocGen4.Process

open scoped DocGen4.Jsx
open DocGen4 (Raw)
open DocGen4.Output
open Lean

/--
Render the HTML for a single tactic.
-/
def TacticInfo.docStringToHtml (tac : TacticInfo MarkdownDocstring) : Output.HtmlM (TacticInfo String) := do
  let captured ← Output.captureHtml do
    <p>{Output.docStringToHtml (.inl tac.docString) tac.internalName.toString}</p>
  return { tac with docString := captured }

/--
Render the HTML for a single tactic.
-/
def TacticInfo.toHtml (tac : TacticInfo String) : Output.BaseHtmlM Unit := do
  let internalName := tac.internalName.toString
  let defLink := (← moduleNameToLink tac.definingModule) ++ "#" ++ internalName
  let tags := ", ".intercalate (tac.tags.map (·.toString)).qsort.toList
  (<div id={internalName}>
    <h2>{tac.userName}</h2>
    {Raw.mk tac.docString}
    <dl>
      <dt>Tags:</dt>
      <dd>{tags}</dd>
      <dt>Defined in module:</dt>
      <dd><a href={defLink}>{tac.definingModule.toString}</a></dd>
    </dl>
  </div>)

def TacticInfo.navLink (tac : TacticInfo α) : Output.BaseHtmlM Unit := do
  <p><a href={"#".append tac.internalName.toString}>{tac.userName}</a></p>

end DocGen4.Process

namespace DocGen4.Output

open scoped DocGen4.Jsx
open Lean Process
open DocGen4 (Raw)

/--
Render the HTML for the tactics listing page.
-/
def tactics (tacticInfo : Array (TacticInfo String)) : BaseHtmlM Unit := do
  baseHtmlGenerator "Tactics" do
    <nav class="internal_nav">
      <p><a href="#top">return to top</a></p>
      {tacticInfo.forM (·.navLink)}
    </nav>
    (<main>
      <p>The tactic language is a special-purpose programming language for constructing proofs, indicated using the keyword <code>by</code>.</p>
      {tacticInfo.forM (·.toHtml)}
    </main>)

def loadTacticsJSON (buildDir : System.FilePath) : IO (Array (TacticInfo String)) := do
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
  return result.qsort (lt := (·.userName < ·.userName))

/-- Save sections of supplementary pages declared in a specific module.

This `abbrev` exists as a type-checking wrapper around `toJson`, ensuring `loadTacticsJSON` gets
objects in the expected format.
-/
abbrev saveTacticsJSON (fileName : System.FilePath) (tacticInfo : Array (TacticInfo String)) : IO Unit := do
  if tacticInfo.size > 0 then
    IO.FS.writeFile fileName (toString (toJson tacticInfo))

end Output
end DocGen4
