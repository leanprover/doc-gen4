/-
Copyright (c) 2021 Henrik Böving. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Henrik Böving
-/
import DocGen4.ToHtmlFormat
import DocGen4.Output.Template

namespace DocGen4
namespace Output

open scoped DocGen4.Jsx

-- TODO: This is a mix of decl.j2 and decl_header.j2, there is tons of stuff still missing
def docInfoToHtml (doc : DocInfo) : HtmlM Html := do
  <div «class»="decl" id={doc.getName.toString}>
    <div «class»={doc.getKind}>
      <div «class»="gh_link">
        -- TODO: Put the proper source link
        <a href="https://github.com">source</a>
      </div>
      -- TODO: Attributes
      -- TODO: Noncomputable, partial etc.
      <span «class»="decl_kind">{doc.getKind}</span>
      -- TODO: HTMLify the name etc.
      {doc.getName.toString}
      -- TODO: args
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
