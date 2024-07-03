import DocGen4.Output.Bibtex.TexDiacritics
import UnicodeBasic

/-!

This file contains functions for bibtex name processing.
Unless stated otherwise, the input string should have no TeX commands
and math equations. Braces are allowed.
An exception is `processNames` which allows input string contains TeX diacritics commands.
The math equations are still not allowed.

-/

open Lean Parsec Unicode

namespace DocGen4.Bibtex

partial def getNameBracedAux : Parsec String := do
  let doOne : Parsec (Option String) := fun it =>
    if it.hasNext then
      match it.curr with
      | '{' => (.some <$> bracedContent getNameBracedAux) it
      | '}' => .success it .none
      | _ => (.some <$> normalChars) it
    else
      .success it .none
  return String.join (← manyOptions doOne).toList

/-- Input a bibtex name string without TeX commands
and math equations, split the string by " " and ",". -/
def getNameAux : Parsec (Array String) := do
  let normalChars' : Parsec String := many1Chars <| satisfy fun c =>
    match c with
    | '\\' | '$' | '{' | '}' | ' ' | '\t' | '\r' | '\n' | ',' => false
    | _ => true
  let doOne' : Parsec (Option String) := fun it =>
    if it.hasNext then
      match it.curr with
      | '{' => (.some <$> bracedContent getNameBracedAux) it
      | '}' | ' ' | '\t' | '\r' | '\n' | ',' => .success it .none
      | _ => (.some <$> normalChars') it
    else
      .success it .none
  let doOne : Parsec (Option String) := fun it =>
    if it.hasNext then
      match it.curr with
      | '}' => .success it .none
      | ' ' | '\t' | '\r' | '\n' => (.some <$> ws') it
      | ',' => .success it.next ","
      | _ => ((.some <| String.join ·.toList) <$> manyOptions doOne') it
    else
      .success it .none
  let arr ← manyOptions doOne
  return arr.filterMap fun s =>
    let t := s.trim
    if t.isEmpty then .none else .some t

/-- Input a name string already split by spaces and comma,
return `(Firstname, Lastname)`.
The braces in the name are preserevd. Supported formats:

- `Lastname, Firstname => (Firstname, Lastname)`
- `Firstname {Last {N}ame} => (Firstname, Last {N}ame)`
- `Firstname Lastname => (Firstname, Lastname)`
- `First Name Lastname => (First Name, Lastname)`
- `Firstname last Name => (Firstname, last Name)`
-/
def getName (arr : Array String) : String × String :=
  if arr.isEmpty then
    ("", "")
  else
    let join' (arr : Subarray String) : String := arr.foldl (fun acc s =>
      acc ++ (if acc.isEmpty || s.isEmpty || s == "," then "" else " ") ++ s) ""
    match arr.getIdx? "," with
    | .some n =>
      (arr.toSubarray.drop (n + 1) |> join', arr.toSubarray.take n |> join')
    | .none =>
      let s := arr.get! (arr.size - 1)
      if s.startsWith "{" && s.endsWith "}" then
        (arr.toSubarray.take (arr.size - 1) |> join',
          s.toSubstring.drop 1 |>.dropRight 1 |>.toString)
      else
        let lastName := arr.reverse.toSubarray.drop 1 |>.toArray.takeWhile
          (fun s => GeneralCategory.isLowercaseLetter s.front) |>.reverse.push s
        (arr.toSubarray.take (arr.size - lastName.size) |> join',
          lastName.toSubarray |> join')

/-- Input a bibtex name string without TeX commands
and math equations, return an array of `(Firstname, Lastname)`.
The braces in the name are preserevd. -/
def getNames : Parsec (Array (String × String)) := do
  let arr ← getNameAux
  let arr2 : Array (Array String) := arr.foldl (fun acc s =>
    if s = "and" then
      acc.push #[]
    else
      acc.modify (acc.size - 1) (Array.push · s)) #[#[]]
  return arr2.filterMap fun arr =>
    let ret := getName arr
    if ret.1.isEmpty && ret.2.isEmpty then .none else .some ret

/-- Strip diacritics from a character. -/
def stripDiacritics (c : Char) : Char :=
  match c with
  | 'œ' => 'o' | 'Œ' => 'O'
  | 'æ' => 'a' | 'Æ' => 'A'
  | 'å' => 'a' | 'Å' => 'A'
  | 'ø' => 'o' | 'Ø' => 'O'
  | 'ł' => 'l' | 'Ł' => 'L'
  | 'ı' => 'i' | 'ȷ' => 'J'
  | '\u00DF' => 's' | '\u1E9E' => 'S'
  | _ =>
    let s := getCanonicalDecomposition c
    s.get? (s.find GeneralCategory.isLetter) |>.getD c

/-- Input a last name string without TeX commands, braces
and math equations, already split by spaces and comma,
return `(oneLetterAbbr, threeLetterAbbr)` of the last name. -/
def getLastNameAbbr (arr : Array String) : String × String :=
  match arr with
  | #[] => ("", "")
  | #[s] =>
    let s := s.toList.toArray.map stripDiacritics |>.filter GeneralCategory.isLetter
    let arr : Array Nat := s.zipWithIndex.filterMap fun x =>
      if GeneralCategory.isUppercaseLetter x.1 then .some x.2 else .none
    if arr.size ≥ 2 then
      if arr[0]! + 2 = arr[1]! then
        let s := s.toSubarray.drop arr[0]! |>.take 3 |>.toArray.toList |> String.mk
        (s, s)
      else
        let s := arr.map s.get! |>.toList |> String.mk
        (s, s)
    else
      let s := String.mk s.toList
      (s.take 1, s.take 3)
  | _ =>
    let s := arr.filterMap (·.get? 0) |>.toList |> String.mk
    (s, s)

/-- Represents the name of a person in bibtex author field. -/
structure BibtexName where
  firstName : String
  lastName : String
  oneLetterAbbr : String
  threeLetterAbbr : String
  deriving Repr

/-- Process the first name and last name without TeX commands
and math equations, remove all braces in them, and produce
one-letter and three-letter abbreviations from the last name. -/
def processName (s : String × String) : Except String BibtexName :=
  let removeBraces' (s : String) : Except String String :=
    match removeBraces s.iter with
    | .success _ s => .ok s
    | .error it err => .error s!"failed to run removeBraces on '{it.1}' at pos {it.2}: {err}"
  match removeBraces' s.1 with
  | .ok firstName =>
    match removeBraces' s.2 with
    | .ok lastName =>
      match getNameAux s.2.iter with
      | .success _ arr =>
        match arr.mapM removeBraces' with
        | .ok arr =>
          let abbr := getLastNameAbbr <| arr.filter (not ·.trim.isEmpty)
          .ok {
            firstName := firstName
            lastName := lastName
            oneLetterAbbr := abbr.1
            threeLetterAbbr := abbr.2
          }
        | .error err => .error err
      | .error it err => .error s!"failed to run getNameAux on '{it.1}' at pos {it.2}: {err}"
    | .error err => .error err
  | .error err => .error err

/-- Input a bibtex name string without math equations, return an array of `BibtexName`. -/
def processNames (s : String) : Except String (Array BibtexName) :=
  match texDiacritics s.iter with
  | .success _ s =>
    match getNames s.iter with
    | .success _ arr => arr.mapM processName
    | .error it err => .error s!"failed to run getNames on '{it.1}' at pos {it.2}: {err}"
  | .error it err => .error s!"failed to run texDiacritics on '{it.1}' at pos {it.2}: {err}"

end DocGen4.Bibtex
