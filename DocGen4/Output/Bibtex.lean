import DocGen4.Output.References
import BibtexQuery.Parser
import BibtexQuery.Format

/-!

# bib file processor using `BibtexQuery`

This file contains functions for bib file processor using
pure Lean library `BibtexQuery`.

The main function is `DocGen4.Bibtex.process`.

-/

open DocGen4 Output BibtexQuery BibtexQuery.Xml

namespace DocGen4.Bibtex

mutual

partial def eToStringEscaped : Element → String
  | .Element n a c =>
    let attrs := a.foldl (fun s n v => s ++ s!" {n}=\"{Html.escape v}\"") ""
    s!"<{n}{attrs}>{c.map cToStringEscaped |>.foldl (· ++ ·) ""}</{n}>"

partial def cToStringEscaped : Content → String
  | .Element e => eToStringEscaped e
  | .Comment c => s!"<!--{c}-->"
  | .Character c => Html.escape c

end

mutual

partial def eToPlaintext : Element → String
  | .Element _ _ c => s!"{c.map cToPlaintext |>.foldl (· ++ ·) ""}"

partial def cToPlaintext : Content → String
  | .Element e => eToPlaintext e
  | .Comment _ => ""
  | .Character c => c

end

/-- Process the contents of bib file. -/
def process' (contents : String) : Except String (Array BibItem) := do
  match BibtexQuery.Parser.bibtexFile ⟨contents, contents.startPos⟩ with
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
