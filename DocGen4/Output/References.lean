import DocGen4.Output.Template

/-!

# Generic functions for references support

This file contains functions for references support,
independent of actual implementation.

The main function is `DocGen4.preprocessBibFile` which preprocess
the contents of bib file using user provided `process` function,
and save the bib file and processed json file to output directory.

-/

open Lean DocGen4 Output

namespace DocGen4

/-- Preprocess (using the user provided `process` function)
and save the bib file to the output path. -/
def preprocessBibFile (contents : String) (process : String → IO (Array BibItem)) : IO Unit := do
  -- create directories
  IO.FS.createDirAll basePath
  IO.FS.createDirAll declarationsBasePath
  -- erase all files
  IO.FS.writeFile (basePath / "references.bib") contents
  IO.FS.writeFile (declarationsBasePath / "references.json") "[]"
  -- if contents is empty, just do nothing
  if contents.trim.isEmpty then
    return
  -- run the user provided process function
  let items ← process contents
  -- save the result
  IO.FS.writeFile (declarationsBasePath / "references.json") (toString (toJson items))

namespace Output

open scoped DocGen4.Jsx

def refItem (ref : BibItem) (backrefs : Array BackrefItem) : BaseHtmlM Html := do
  let backrefs := backrefs.filter (fun x => x.citekey == ref.citekey)
  let toHtml (i : Nat) : BaseHtmlM (Array Html) := do
    let backref := backrefs[i]!
    let href := s!"{moduleNameToFile "" backref.modName}#_backref_{backref.index}"
    let title := s!"File: {backref.modName}" ++
      if backref.funName.isEmpty then "" else s!"\nLocation: {backref.funName}"
    pure #[.raw " ", <a href={href} title={title}>{.text s!"[{i + 1}]"}</a>]
  let backrefHtml : Html ← (do
    if backrefs.isEmpty then
      pure (.raw "")
    else
      pure <small>[(← (Array.range backrefs.size).mapM toHtml).foldl (· ++ ·) #[]]</small>)
  pure <|
    <li id={s!"ref_{ref.citekey}"}>
      <a href={s!"#ref_{ref.citekey}"}>{.text ref.tag}</a>
      {.raw " "}{.raw ref.html}{backrefHtml}
    </li>

def references (backrefs : Array BackrefItem) :
    BaseHtmlM Html := templateLiftExtends (baseHtml "References") do
  pure <|
    <main>
      <a id="top"></a>
      <h1>References</h1>
      <ul>
      [← (← read).refs.mapM (refItem · backrefs)]
      </ul>
    </main>

end Output

end DocGen4
