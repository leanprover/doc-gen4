/-
Copyright (c) 2021 Henrik Böving. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Henrik Böving
-/
import Lean
import Lean.PrettyPrinter

import DocGen4.ToHtmlFormat
import DocGen4.Output.Template

namespace DocGen4
namespace Output

open scoped DocGen4.Jsx
open Lean PrettyPrinter

def docInfoHeader (doc : DocInfo) : HtmlM Html := do
  let mut nodes := #[]
  -- TODO: noncomputable, partial
  -- TODO: Support all the kinds in CSS
  nodes := nodes.push <span «class»="decl_kind">{doc.getKind}</span>
  -- TODO: HTMLify the name etc.
  nodes := nodes.push <span «class»="name">doc.getName.toString</span>
  -- TODO: Figure out how we can get explicit, implicit and TC args and put them here
  nodes := nodes.push <span «class»="decl_args">:</span>
  nodes := nodes.push <div «class»="decl_type"><span «class»="fn">Type!!!</span></div>
  -- TODO: The final type of the declaration
  return <div «class»="decl_header"> [nodes] </div>

def docInfoToHtml (doc : DocInfo) : HtmlM Html := do
  <div «class»="decl" id={doc.getName.toString}>
    <div «class»={doc.getKind}>
      <div «class»="gh_link">
        -- TODO: Put the proper source link
        <a href="https://github.com">source</a>
      </div>
      -- TODO: Attributes
      {←docInfoHeader doc}
      -- TODO: The actual type information we are here for
    </div>
  </div>

def moduleToHtml (module : Module) : HtmlM Html := withReader (setCurrentName module.name) do
  -- TODO: Probably some sort of ordering by line number would be cool?
  -- maybe they should already be ordered in members.
  let docInfos ← module.members.mapM docInfoToHtml
  -- TODO: This is missing imports, imported by, source link, list of decls
  templateExtends (baseHtml module.name.toString) $
    Html.element "main" #[] docInfos

end Output
end DocGen4
