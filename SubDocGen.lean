/-
Copyright (c) 2025 Anne Baanen. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Anne Baanen
-/
module

public meta import Lean.Elab.Command
public import Lean.EnvExtension
public import Lean.Meta.Basic

/-!
# Library for adding supplementary pages to the DocGen output.

In addition to the pages listing declarations, DocGen can output supplementary pages.
Each page is made up of an introduction followed by a sequence of sections.
These sections can be added individually through metaprogramming, for example when adding an
attribute to a declaration.

The main user interface for adding pages and sections can be found in the commands
`register_supplement_page` and `register_supplement_section`; these correspond to
`SubDocGen.registerSupplementPage` and `SubDocGen.registerSupplementSection` respectively.
-/

namespace SubDocGen

open Lean Meta

/-- A docstring formatted in Markdown. -/
public abbrev MarkdownDocstring := String

/-- A section out of a `SupplementPage`. -/
public structure SupplementSection (textFormat : Type) where
  /-- Human-readable title of the section. -/
  name : String

  /-- Main contents of this section. -/
  text : textFormat

  /-- Module where this section is defined. Will be set by `registerSupplementSection`. -/
  definingModule : Option Name
  /-- Declaration(s) relating to this section. 

  For example, a tactic might link here to its implementation.
  -/
  relatedDecls : Array Name
deriving FromJson, ToJson

/--
A page to be rendered in addition to the module docs.

Pages have some introductory text, followed by headered sections.
-/
public structure SupplementPage (textFormat : Type) where
  /-- Human-readable name of the page. -/
  name : String
  /-- Page introduction. -/
  intro : textFormat

  /-- Module where this page is defined. Will be set by `registerSupplementPage`. -/
  definingModule : Option Name
  /-- Sections of the page. Will be computed by DocGen during processing of the supplement pages. -/
  sections : Array (SupplementSection textFormat)

/-- A section out of a `SupplementPage`, plus information on where to place the section. -/
public structure SupplementSectionEntry (textFormat) extends SupplementSection textFormat where
  /-- Identifies the page to which this section belongs. -/
  pageKey : String
deriving FromJson, ToJson

/-- A page to be rendered in addition to the module docs, plus information on
where to find its sections. -/
public structure SupplementPageEntry (textFormat) extends SupplementPage textFormat where
  /-- A unique identifier for this page, used to associate sections declared in downstream modules. -/
  key : String
deriving FromJson, ToJson

/-- This environment extension allows adding supplement pages that will be rendered by DocGen.
Add entries using the `SubDocGen.registerSupplementPage` function or
`register_supplement_page` command.
-/
-- Pages are stored as a `HashMap` for easy existence checking.
public meta initialize supplementPageExt : SimplePersistentEnvExtension (SupplementPageEntry MarkdownDocstring)
    (Std.HashMap String (SupplementPageEntry MarkdownDocstring)) ←
  registerSimplePersistentEnvExtension {
    addImportedFn as := as.foldl (fun m as => m.insertMany (as.map fun a => (a.key, a))) {}
    addEntryFn m a := m.insert a.key a
  }

/-- This environment extension allows adding sections to supplement pages.
Add entries using the `SubDocGen.registerSupplementSection` function or
`register_supplement_section` command.

See also `SubDocGen.supplementPageExt`.
-/
-- Sections are stored as an array since they are all scanned through per-module anyway.
public meta initialize supplementSectionExt : SimplePersistentEnvExtension (SupplementSectionEntry MarkdownDocstring)
    (Array (SupplementSectionEntry MarkdownDocstring)) ←
  registerSimplePersistentEnvExtension {
    addImportedFn as := as.flatten
    addEntryFn as a := as.push a
  }

variable {m} [Monad m] [MonadEnv m] [MonadError m]

/-- Add a page (with perhaps some predefined sections) to the DocGen supplement.
This function is the interface for metaprograms: the command `register_supplement_page` is the
basic wrapper for users.

Page keys should be unique. If not, this function throws an error.

Further sections to this page can be added by calling `registerSupplementSection`.
-/
public meta def registerSupplementPage (page : SupplementPageEntry MarkdownDocstring) : m Unit := do
  if (supplementPageExt.getState (← getEnv)).contains page.key then
    throwError m!"registerSupplementPage: there is already a page with key `{page.key}`."
  modifyEnv (supplementPageExt.addEntry · { page with definingModule := (← getEnv).header.mainModule })

/-- Add a section to a page in the DocGen supplement.
This function is the interface for metaprograms: the command `register_supplement_section` is the
basic wrapper for users.

The page that this section appears in should first be declared using `registerSupplementPage`.
If not, this function throws an error.
-/
public meta def registerSupplementSection (sec : SupplementSectionEntry MarkdownDocstring) : m Unit := do
  if !(supplementPageExt.getState (← getEnv)).contains sec.pageKey then
    throwError m!"registerSupplementSection: no page has been declared with key `{sec.pageKey}`."
  modifyEnv (supplementSectionExt.addEntry · { sec with definingModule := (← getEnv).header.mainModule })

open Elab Command in
/-- `register_supplement_page key "Page Title" /-- Introduction text -/`
adds a page to the DocGen supplement under the given key.

In metaprograms, you can call `registerSupplementPage` directly.
-/
elab "register_supplement_page " name:ident ppSpace title:str ppSpace dc:docComment : command => do
  registerSupplementPage {
    key := name.getId.toString
    name := title.getString
    intro := dc.getDocString
    definingModule := none
    sections := #[]
  }

open Elab Command in
/-- `register_supplement_section pageKey "Section Title" /-- Introduction text -/`
adds a section to the `pageKey` page in the DocGen supplement.

In metaprograms, you can call `registerSupplementSection` directly.
-/
elab "register_supplement_section " pageKey:ident ppSpace title:str ppSpace dc:docComment : command => do
  registerSupplementSection {
    pageKey := pageKey.getId.toString
    name := title.getString
    text := dc.getDocString
    definingModule := none
    relatedDecls := #[]
  }

end SubDocGen
