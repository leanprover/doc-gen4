import DocGen4.Output.References

/-!

# bib file processor using `pybtex`

This file contains functions for bib file processor using
external program `pybtex` which is written in Python.

The main function is `DocGen4.Pybtex.process`.

-/

open Lean DocGen4 Output

namespace DocGen4

namespace Pybtex

private def inputTmpFile := declarationsBasePath / "input.tmp"
private def outputTmpFile := declarationsBasePath / "output.tmp"

private def proc (args : IO.Process.SpawnArgs) : IO Unit := do
  let child ← IO.Process.spawn args
  let exitCode ← child.wait
  if exitCode != 0 then
    throw (IO.userError s!"external command '{args.cmd}' exited with code {exitCode}")

/-- Get the array of cite keys from bibtexml contents. -/
def getCitekeys (contents : String) : Except String (Array String) :=
  match Xml.parse contents with
  | .ok (.Element tag _ arr) =>
    if tag != "bibtex:file" then
      .error s!"node is '{tag}', but 'bibtex:file' expected"
    else
      let arr2 : Array (Except String String) := arr.map fun node =>
        match node with
        | .Element (.Element tag attr _) =>
          if tag != "bibtex:entry" then
            .error s!"node is '{tag}', but 'bibtex:entry' expected"
          else
            match attr.find? "id" with
            | .some s => .ok s
            | .none => .error "failed to find 'id' attribute"
        | _ => .ok ""
      match arr2.findSome? (fun x => match x with | .error s => .some s | _ => .none) with
      | .some s => .error s
      | .none => .ok (arr2.map (fun x => match x with | .ok s => s | _ => unreachable!)
        |>.filter fun s => not s.trim.isEmpty)
  | .error err => .error err

private def deleteTempFile : IO Unit := do
  IO.FS.removeFile inputTmpFile <|> pure ()
  IO.FS.removeFile outputTmpFile <|> pure ()

private def getCitekeysFromTempFile : IO (Array String) := do
  -- run `pybtex-convert`
  proc {
    cmd := "pybtex-convert"
    args := #[
      "-f", "bibtex", "-t", "bibtexml", "--preserve-case",
      inputTmpFile.toString,
      outputTmpFile.toString
    ]
  }
  -- parse the returned XML file
  match getCitekeys (← IO.FS.readFile outputTmpFile) with
  | .ok ret =>
    pure ret
  | .error err =>
    throw (IO.userError s!"failed to parse bibtexml file: {err}")

section getHtmlFromTempFile

private partial def naiveFindSubstr (it : String.Iterator) (p : String) : Option String.Iterator :=
  if it.atEnd then
    .none
  else if it.1.substrEq it.2 p 0 p.utf8ByteSize then
    .some it
  else
    naiveFindSubstr it.next p

private partial def extractItemsAux (arr : Array Xml.Content) (f : Xml.Content → String)
    (s : String) (i : Nat := 0) (ret : Array String := #[]) : Array String :=
  if h : i < arr.size then
    let new : Array String :=
      if let .Element (.Element name _ contents) := arr.get ⟨i, h⟩ then
        if name == s then
          ret.push (String.join (contents.map f).toList)
        else
          ret
      else
        ret
    extractItemsAux arr f s (i + 1) new
  else
    ret

private def extractItems (htmlTmp : String) : Except String (Array (String × String × String)) :=
  match naiveFindSubstr htmlTmp.mkIterator "<dl>" with
  | .none => .error "failed to find '<dl>'"
  | .some lps =>
    match naiveFindSubstr lps "</dl>" with
    | .none => .error "failed to find '</dl>'"
    | .some lpe =>
      match Xml.parse (Substring.toString ⟨htmlTmp, lps.2, ⟨lpe.2.1 + 5⟩⟩) with
      | .ok (.Element _ _ arr) =>
        .ok <| (extractItemsAux arr Xml.cToPlaintext "dt").zip
          ((extractItemsAux arr Xml.cToStringEscaped "dd").zip
            (extractItemsAux arr Xml.cToPlaintext "dd"))
      | .error err => .error err

private def getHtmlFromTempFile : IO (Array (String × String × String)) := do
  -- run `pybtex-format`
  proc {
    cmd := "pybtex-format"
    args := #[
      "-f", "bibtex", "-b", "html", "--label-style=alpha",
      inputTmpFile.toString,
      outputTmpFile.toString
    ]
  }
  -- parse the returned HTML file
  let contents ← IO.FS.readFile outputTmpFile
  match extractItems (contents.replace "&nbsp;" "\u00A0") with
  | .ok ret =>
    pure ret
  | .error err =>
    throw (IO.userError s!"failed to parse html file: {err}")

end getHtmlFromTempFile

/-- Process the contents of bib file by calling external program `pybtex`. -/
def process (contents : String) : IO (Array BibItem) := do
  try
    -- create directory
    IO.FS.createDirAll basePath
    -- save temp file
    IO.FS.writeFile inputTmpFile contents
    -- get cite keys
    let citekeys ← getCitekeysFromTempFile
    -- get items
    let items ← getHtmlFromTempFile
    -- delete temp file
    deleteTempFile
    -- combine the result
    let ret : Array BibItem := (citekeys.zip items).map fun x => {
      citekey := x.1
      tag := "[" ++ x.2.1 ++ "]"
      html := x.2.2.1
      plaintext := x.2.2.2
    }
    pure ret
  catch e =>
    -- delete temp file
    deleteTempFile
    -- report error
    throw e

end Pybtex

end DocGen4
