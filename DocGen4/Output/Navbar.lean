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
  let attributes := match ←getCurrentName with
  | some name =>
    if file == name then
      #[("class", "nav_link"), ("visible", "")]
    else
      #[("class", "nav_link")]
  | none => #[("class", "nav_link")]
  let nodes := #[<a href={s!"{←getRoot}{moduleNameToUrl file}"}>{file.toString}</a>]
  return Html.element "div" attributes nodes

partial def moduleListDir (h : Hierarchy) : HtmlM Html := do
  let children := Array.mk (h.getChildren.toList.map Prod.snd)
  let dirs := children.filter (λ c => c.getChildren.toList.length != 0)
  let files := children.filter Hierarchy.isFile |>.map Hierarchy.getName
  let dirNodes ← (dirs.mapM moduleListDir)
  let fileNodes ← (files.mapM moduleListFile)
  let attributes := match ←getCurrentName with
  | some name =>
    if h.getName.isPrefixOf name then
      #[("class", "nav_sect"), ("data-path", moduleNameToUrl h.getName), ("open", "")]
    else
      #[("class", "nav_sect"), ("data-path", moduleNameToUrl h.getName)]
  | none =>
      #[("class", "nav_sect"), ("data-path", moduleNameToUrl h.getName)]
  let nodes := #[<summary>{h.getName.toString}</summary>] ++ dirNodes ++ fileNodes
  return Html.element "details" attributes nodes

def moduleList : HtmlM (Array Html) := do
  let hierarchy := (←getResult).hierarchy
  let mut list := Array.empty
  for (n, cs) in hierarchy.getChildren do
    list := list.push <h4>{n.toString}</h4>
    list := list.push $ ←moduleListDir cs
  list

def navbar : HtmlM Html := do
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
    [←moduleList]
  </nav>

end Output
end DocGen4
