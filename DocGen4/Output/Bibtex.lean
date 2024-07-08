import DocGen4.Output.References
import BibtexQuery.Parser
import BibtexQuery.Name
import Lean.Data.HashMap

/-!

# bib file processor using `BibtexQuery`

This file contains functions for bib file processor using
pure Lean library `BibtexQuery`.

The main function is `DocGen4.Bibtex.process`.

-/

open Lean DocGen4 Output BibtexQuery TexDiacritics Unicode

namespace DocGen4.Bibtex

structure BibItemEx extends BibItem where
  authors : Array BibtexName
  date : Nat
  titleWithoutDiacritics : String

def getAuthors (tags : HashMap String String) : Except String (Array BibtexName × Bool) := do
  if let .some s := tags.find? "author" then
    pure (← processNames s, false)
  else if let .some s := tags.find? "editor" then
    pure (← processNames s, true)
  else
    pure (#[], false)

def authorsToString (authors : Array BibtexName) (isEditor : Bool) : String :=
  let arr := authors.map (fun x =>
    (x.firstName ++ " " ++ x.lastName).trim) |>.filter (not ·.isEmpty)
  if arr.isEmpty then
    ""
  else
    ", ".intercalate arr.toList ++ if isEditor then
      if arr.size > 1 then ", editors." else ", editor."
    else
      "."

def getDate (tags : HashMap String String) : Nat × String :=
  if let .some yearStr := tags.find? "year" then
    let yearStr := yearStr.trim
    if let .some year := yearStr.toList.filter Char.isDigit |> String.mk |>.toNat? then
      let month : Nat :=
        if let .some monthStr := tags.find? "month" then
          let monthStr := monthStr.trim.toLower
          match monthStr with
          | "jan" => 1 | "feb" => 2 | "mar" => 3
          | "apr" => 4 | "may" => 5 | "jun" => 6
          | "jul" => 7 | "aug" => 8 | "sep" => 9
          | "oct" => 10 | "nov" => 11 | "dec" => 12
          | _ =>
            let month := monthStr.toNat?.getD 0
            if month ≥ 1 ∧ month ≤ 12 then month else 0
        else
          0
      let monthStr : String :=
        match month with
        | 1 => "Jan" | 2 => "Feb" | 3 => "Mar"
        | 4 => "Apr" | 5 => "May" | 6 => "Jun"
        | 7 => "Jul" | 8 => "Aug" | 9 => "Sep"
        | 10 => "Oct" | 11 => "Nov" | 12 => "Dec"
        | _ => ""
      (year * 100 + month, (monthStr ++ " " ++ yearStr).trim)
    else
      (0, yearStr)
  else
    (0, "")

def getTag (authors : Array BibtexName) (date : Nat) : String :=
  let authorString :=
    if authors.size ≥ 5 then
      (authors.toSubarray.take 3 |>.toArray.map (·.oneLetterAbbr) |>.toList |> String.join) ++ "+"
    else if authors.size ≥ 2 then
      authors.map (·.oneLetterAbbr) |>.toList |> String.join
    else
      authors.map (·.threeLetterAbbr) |>.toList |> String.join
  let dateString := if date > 0 then (toString (date / 100 + 100)).takeRight 2 else ""
  "[" ++ authorString ++ dateString ++ "]"

def processItem (_category : String) (citekey : String)
    (tags : HashMap String String) : Except String BibItemEx := do
  let (authors, isEditor) ← getAuthors tags
  let authorStr := authorsToString authors isEditor
  let (date, dateStr) := getDate tags
  let title := tags.findD "title" ""
  -- TODO: other fields
  let htmlArr : Array String := #[
    Html.escape authorStr,
    if title.isEmpty then "" else ("<em>" ++ Html.escape title ++ "</em>."),
    if dateStr.isEmpty then "" else (Html.escape dateStr ++ ".")
  ]
  let plaintextArr : Array String := #[
    authorStr,
    if title.isEmpty then "" else (title ++ "."),
    if dateStr.isEmpty then "" else (dateStr ++ ".")
  ]
  return {
    citekey := citekey
    tag := getTag authors date
    html := "\n".intercalate (htmlArr.filter (not ·.isEmpty) |>.toList)
    plaintext := "\n".intercalate (plaintextArr.filter (not ·.isEmpty) |>.toList)
    authors := authors
    date := date
    titleWithoutDiacritics := stripDiacriticsFromString title |>.map getUpperChar
  }

partial def compareAuthors (a b : Array BibtexName) (i : Nat := 0) : Ordering :=
  if ha : i < a.size then
    if hb : i < b.size then
      if a[i].lastNameWithoutDiacritics < b[i].lastNameWithoutDiacritics then
        .lt
      else if a[i].lastNameWithoutDiacritics > b[i].lastNameWithoutDiacritics then
        .gt
      else if a[i].firstNameWithoutDiacritics < b[i].firstNameWithoutDiacritics then
        .lt
      else if a[i].firstNameWithoutDiacritics > b[i].firstNameWithoutDiacritics then
        .gt
      else
        compareAuthors a b (i + 1)
    else
      .gt
  else
    if i < b.size then .lt else .eq

def compareItem (a b : BibItemEx) : Ordering :=
  match compareAuthors a.authors b.authors with
  | .lt => .lt | .gt => .gt
  | .eq =>
    if a.date < b.date then
      .lt
    else if a.date > b.date then
      .gt
    else if a.titleWithoutDiacritics < b.titleWithoutDiacritics then
      .lt
    else if a.titleWithoutDiacritics > b.titleWithoutDiacritics then
      .gt
    else
      .eq

-- TODO: `n ≥ 26` case
def toBase26 (n : Nat) : String :=
  toString (Char.ofNat ('a'.toNat + n))

/-- Process the contents of bib file. -/
def process' (contents : String) : Except String (Array BibItem) := do
  match BibtexQuery.Parser.bibtexFile contents.iter with
  | .success _ arr =>
    let arr : Array BibItemEx ← arr.toArray.filterMapM fun x => do
      match x with
      | .normalType category citekey tags =>
        let lst : List (String × String) ← tags.mapM fun x => do
          match x.name with
          | "author" | "editor" => pure (x.name, x.content.trim)
          | _ =>
            match texDiacritics x.content.iter with
            | .success _ s =>
              match removeBraces s.iter with
              | .success _ s => pure (x.name, s.trim)
              | .error it err => .error s!"failed to run removeBraces on '{it.1}' at pos {it.2}: {err}"
            | .error it err => .error s!"failed to run texDiacritics on '{it.1}' at pos {it.2}: {err}"
        .some <$> processItem category citekey (.ofList lst)
      | _ => pure .none
    let mut ret := arr.qsort (compareItem · · |>.isLT) |>.map BibItemEx.toBibItem
    let mut tags : HashMap String (Nat × Nat) := .empty
    let mut i := 0
    while h : i < ret.size do
      let tag := ret[i].tag
      if let .some (first, count) := tags.find? tag then
        if count = 0 then
          ret := ret.modify first fun x => { x with tag := x.tag.dropRight 1 ++ "a]" }
        ret := ret.modify i fun x => { x with tag := x.tag.dropRight 1 ++ toBase26 (count + 1) ++ "]" }
        tags := tags.insert tag (first, count + 1)
      else
        tags := tags.insert tag (i, 0)
      i := i + 1
    return ret
  | .error it err =>
    throw s!"failed to parse bib file at pos {it.2}: {err}"

/-- Process the contents of bib file. -/
def process (contents : String) : IO (Array BibItem) := do
  match process' contents with
  | .ok ret => return ret
  | .error err => throw <| IO.userError err

end DocGen4.Bibtex
