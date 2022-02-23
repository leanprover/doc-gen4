/-
Copyright (c) 2021 Henrik Böving. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Henrik Böving
-/
import Lean
import DocGen4.ToHtmlFormat
import DocGen4.Output.Base

namespace DocGen4
namespace Output

open Lean
open scoped DocGen4.Jsx

def moduleListFile (file : Name) : HtmlM Html := do
  pure <div «class»={if (← getCurrentName) == file then "nav_link visible" else "nav_link"}>
    <a href={← moduleNameToLink file}>{file.toString}</a>
  </div>

partial def moduleListDir (h : Hierarchy) : HtmlM Html := do
  let children := Array.mk (h.getChildren.toList.map Prod.snd)
  let dirs := children.filter (λ c => c.getChildren.toList.length != 0)
  let files := children.filter (λ c => Hierarchy.isFile c ∧ c.getChildren.toList.length = 0)
    |>.map Hierarchy.getName
  let dirNodes ← (dirs.mapM moduleListDir)
  let fileNodes ← (files.mapM moduleListFile)
  let moduleLink ← moduleNameToLink h.getName
  pure
    <details "class"="nav_sect" "data-path"={moduleLink}
      [if (←getCurrentName).any (h.getName.isPrefixOf ·) then #[("open", "")] else #[]]>
      {
        if (←getResult).moduleInfo.contains h.getName then
          Html.element "summary" true #[] #[<a "href"={← moduleNameToLink h.getName}>{h.getName.toString}</a>]
        else
          <summary>{h.getName.toString}</summary>
      }
      [dirNodes]
      [fileNodes]
    </details>

def moduleList : HtmlM Html := do
  let hierarchy := (←getResult).hierarchy
  let mut list := Array.empty
  for (n, cs) in hierarchy.getChildren do
    list := list.push $ ←moduleListDir cs
  pure <div "class"="module_list">[list]</div>

def navbar : HtmlM Html := do
  pure
    <nav «class»="nav">
      <h3>General documentation</h3>
      <div «class»="nav_link"><a href={s!"{←getRoot}"}>index</a></div>
      /-
      TODO: Add these in later
      <div «class»="nav_link"><a href={s!"{←getRoot}tactics.html"}>tactics</a></div>
      <div «class»="nav_link"><a href={s!"{←getRoot}commands.html"}>commands</a></div>
      <div «class»="nav_link"><a href={s!"{←getRoot}hole_commands.html"}>hole commands</a></div>
      <div «class»="nav_link"><a href={s!"{←getRoot}attributes.html"}>attributes</a></div>
      <div «class»="nav_link"><a href={s!"{←getRoot}notes.html"}>notes</a></div>
      <div «class»="nav_link"><a href={s!"{←getRoot}references.html"}>references</a></div>
      -/
      <h3>Library</h3>
      {← moduleList}
    </nav>

end Output
end DocGen4
