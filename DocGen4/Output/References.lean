import DocGen4.Output.Template

/-!

# Generic functions for references support

This file contains functions for references support,
independent of actual implementation.

The main function is `DocGen4.preprocessBibFile` which preprocess
the contents of bib file using user provided `process` function,
and save the bib file and processed json file to output directory,
as "references.bib" and "references.json", respectively.

Note that "references.bib" is only for user to download it, the actual file
used later is "references.json". It contains an array of objects with 4 fields:
'citekey', 'tag', 'html' and 'plaintext'. For the meaning of these fields,
see `DocGen4.Output.BibItem`.

-/

open Lean DocGen4 Output

namespace DocGen4

/-- Preprocess (using the user provided `process` function)
and save the bib file to the output path. -/
def preprocessBibFile (buildDir : System.FilePath) (contents : String) (process : String → IO (Array BibItem)) : IO Unit := do
  -- create directories
  IO.FS.createDirAll <| basePath buildDir
  IO.FS.createDirAll <| declarationsBasePath buildDir
  -- save the contents to "references.bib" and erase "references.json"
  IO.FS.writeFile (basePath buildDir / "references.bib") contents
  IO.FS.writeFile (declarationsBasePath buildDir / "references.json") "[]"
  -- if contents is empty, just do nothing
  if contents.trimAscii.isEmpty then
    return
  -- run the user provided process function
  let items ← process contents
  -- save the result to "references.json"
  IO.FS.writeFile (declarationsBasePath buildDir / "references.json") (toString (toJson items))

/-- Save the bib json to the output path. -/
def preprocessBibJson (buildDir : System.FilePath) (contents : String) : IO Unit := do
  -- create directories
  IO.FS.createDirAll <| basePath buildDir
  IO.FS.createDirAll <| declarationsBasePath buildDir
  -- erase "references.bib" (since we can't recover it from json)
  -- and save the contents to "references.json"
  IO.FS.writeFile (basePath buildDir / "references.bib") ""
  IO.FS.writeFile (declarationsBasePath buildDir / "references.json") contents

/-- Erase the contents of bib file in the output path. -/
def disableBibFile (buildDir : System.FilePath) : IO Unit := do
  -- create directories
  IO.FS.createDirAll <| basePath buildDir
  IO.FS.createDirAll <| declarationsBasePath buildDir
  -- erase files
  IO.FS.writeFile (basePath buildDir / "references.bib") ""
  IO.FS.writeFile (declarationsBasePath buildDir / "references.json") "[]"

namespace Output

open scoped DocGen4.Jsx
open DocGen4 (Raw)

def refItem (ref : BibItem) (backrefs : Array BackrefItem) : BaseHtmlM Unit := do
  let backrefs := backrefs.filter (fun x => x.citekey == ref.citekey)
  let backrefLinks : BaseHtmlM Unit := do
    if !backrefs.isEmpty then
      (<small>
        {do for i in [:backrefs.size] do
          let backref := backrefs[i]!
          let href := s!"{← moduleNameToLink backref.modName}#_backref_{backref.index}"
          let title := s!"File: {backref.modName}" ++
            if backref.funName.isEmpty then "" else s!"\nLocation: {backref.funName}"
          Html.rawText " "
          (<a href={href} title={title}>{s!"[{i + 1}]"}</a>)}
      </small>)
  (<li id={s!"ref_{ref.citekey}"}>
    <a href={s!"#ref_{ref.citekey}"}>{ref.tag}</a>
    {Raw.mk " "}
    {Raw.mk ref.html}
    {backrefLinks}
  </li>)

def references (backrefs : Array BackrefItem) : BaseHtmlM Unit := do
  baseHtmlGenerator "References" do
    <main>
      <a id="top"></a>
      <h1>References</h1>
      <ul>
        {(← read).refs.forM (refItem · backrefs)}
      </ul>
    </main>

end Output

end DocGen4
