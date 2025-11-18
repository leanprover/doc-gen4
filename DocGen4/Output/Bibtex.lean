import DocGen4.Output.References
import BibtexQuery.Parser
import BibtexQuery.Format

/-!

# bib file processor using `BibtexQuery`

This file contains functions for bib file processor using
pure Lean library `BibtexQuery`.

The main function is `DocGen4.Bibtex.process`.

-/

open Lean Xml DocGen4 Output BibtexQuery

namespace DocGen4.Bibtex

/-- Process the contents of bib file. -/
def process' (contents : String) : Except String (Array BibItem) := do
  match BibtexQuery.Parser.bibtexFile ⟨contents, contents.startValidPos⟩ with
  | .success _ arr =>
    let arr ← arr.toArray.filterMapM ProcessedEntry.ofEntry
    return arr |> sortEntry |> deduplicateTag |>.map fun x =>
      let html := Formatter.format x
      {
        citekey := x.name
        tag := x.tag
        html := html.map cToStringEscaped |>.toList |> String.join
        plaintext := html.map cToPlaintext |>.toList |> String.join
      }
  | .error it err =>
    throw s!"failed to parse bib file at pos {it.2.offset}: {err}"

/-- Process the contents of bib file. -/
def process (contents : String) : IO (Array BibItem) := do
  match process' contents with
  | .ok ret => return ret
  | .error err => throw <| IO.userError err

end DocGen4.Bibtex
