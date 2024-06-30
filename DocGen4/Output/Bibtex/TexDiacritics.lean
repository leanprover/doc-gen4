import Lean.Data.Parsec

/-!

This file contains functions for TeX diacritics process.
The main function is `texDiacritics`, which
will convert all TeX commands for diacritics into UTF-8 characters,
and error on any other TeX commands which are not in math environment.

-/

open Lean Parsec

namespace DocGen4.Bibtex

/-- Match a sequence of space characters and return it. -/
def ws' : Parsec String :=  manyChars <| satisfy fun c =>
  c == ' ' || c == '\n' || c == '\r' || c == '\t'

/-- Match a normal characters which is not the beginning of TeX command. -/
def normalChar : Parsec Char := satisfy fun c =>
  c != '\\' && c != '$' && c != '{' && c != '}'

/-- Match at least one normal characters which is not the beginning of TeX command. -/
def normalChars : Parsec String := many1Chars normalChar

/-- Match a TeX command starting with `\`, potentially with trailing whitespaces. -/
def texCommand : Parsec String := pchar '\\' *> attempt do
  let s ← manyChars asciiLetter
  if s.isEmpty then
    return "\\" ++ toString (← anyChar) ++ (← ws')
  else
    match ← peek? with
    | .some c =>
      match c with
      | '*' =>
        skip
        return "\\" ++ s ++ toString c ++ (← ws')
      | _ =>
        return "\\" ++ s ++ (← ws')
    | .none =>
      return "\\" ++ s

/-- Similar to `texCommand` but it excludes some commands. -/
def texCommand' (exclude : Array String) : Parsec String := attempt do
  let s ← texCommand
  match exclude.find? (· == s.trim) with
  | .some _ => fail s!"'{s.trim}' is not allowed"
  | .none => return s

/-- Match a sequence starting with `{` and ending with `}`. -/
def bracedContent (p : Parsec String) : Parsec String :=
  pchar '{' *> (("{" ++ · ++ "}") <$> p) <* pchar '}'

/-- Similar to `bracedContent` but it does not output braces. -/
def bracedContent' (p : Parsec String) : Parsec String :=
  pchar '{' *> p <* pchar '}'

partial def manyOptions {α} (p : Parsec (Option α)) (acc : Array α := #[]) :
    Parsec (Array α) := fun it =>
  match p it with
  | .success it ret =>
    match ret with
    | .some ret => manyOptions p (acc.push ret) it
    | .none => .success it acc
  | .error it err => .error it err

partial def mathContentAux2 : Parsec String := do
  let doOne : Parsec (Option String) := fun it =>
    if it.hasNext then
      match it.curr with
      | '{' => (.some <$> bracedContent mathContentAux2) it
      | '\\' =>
        match texCommand' #["\\(", "\\)", "\\[", "\\]"] it with
        | .success it ret => .success it (.some ret)
        | .error _ _ => .success it .none
      | '}' | '$' => .success it .none
      | _ => (.some <$> normalChars) it
    else
      .success it .none
  return String.join (← manyOptions doOne).toList

def mathContentAux (beginning ending dollar : String) : Parsec String :=
  pstring beginning *> ((dollar ++ · ++ dollar) <$> mathContentAux2) <* pstring ending

/-- Match a math content. Returns `Option.none` if it does not start with `\(`, `\[` or `$`. -/
def mathContent : Parsec (Option String) := fun it =>
  let substr := it.extract (it.forward 2)
  if substr = "\\[" then
    (.some <$> mathContentAux "\\[" "\\]" "$$") it
  else if substr = "\\(" then
    (.some <$> mathContentAux "\\(" "\\)" "$") it
  else if substr = "$$" then
    (.some <$> mathContentAux "$$" "$$" "$$") it
  else if it.curr = '$' then
    (.some <$> mathContentAux "$" "$" "$") it
  else
    .success it .none

/-- Match a TeX command for diacritics, return the corresponding UTF-8 string.
Sometimes it needs to read the character after the command,
in this case the `p` is used to read braced content. -/
def texDiacriticsCommand (p : Parsec String) : Parsec String := do
  let cmd ← String.trim <$> texCommand
  match cmd with
  | "\\oe" => pure "œ"
  | "\\OE" => pure "Œ"
  | "\\ae" => pure "æ"
  | "\\AE" => pure "Æ"
  | "\\aa" => pure "å"
  | "\\AA" => pure "Å"
  | "\\o" => pure "ø"
  | "\\O" => pure "Ø"
  | "\\l" => pure "ł"
  | "\\L" => pure "Ł"
  | "\\i" => pure "ı"
  | "\\j" => pure "ȷ"
  | "\\ss" => pure "\u00DF"
  | "\\SS" => pure "\u1E9E"
  | "\\cprime" => pure "\u02B9"
  | _ =>
    let ch : String := match cmd with
    | "\\`" => "\u0300"
    | "\\'" => "\u0301"
    | "\\^" => "\u0302"
    | "\\\"" => "\u0308"
    | "\\~" => "\u0303"
    | "\\=" => "\u0304"
    | "\\." => "\u0307"
    | "\\u" => "\u0306"
    | "\\v" => "\u030C"
    | "\\H" => "\u030B"
    | "\\t" => "\u0361"
    | "\\c" => "\u0327"
    | "\\d" => "\u0323"
    | "\\b" => "\u0331"
    | "\\k" => "\u0328"
    | _ => ""
    if ch.isEmpty then
      fail s!"unsupported command: '{cmd}'"
    else
      let doOne : Parsec String := fun it =>
        if it.hasNext then
          match it.curr with
          | '{' => bracedContent p it
          | _ => normalChars it
        else
          .error it "character expected"
      let s ← doOne
      if s.startsWith "{" then
        if s.length < 3 then
          fail s!"expected string of length at least 3, but got '{s}'"
        else
          return s.take 2 ++ ch ++ s.drop 2
      else
        if s.isEmpty then
          fail "expected a non-empty string"
        else
          return s.take 1 ++ ch ++ s.drop 1

/-- Convert all TeX commands for diacritics into UTF-8 characters,
and error on any other TeX commands which are not in math environment. -/
partial def texDiacritics : Parsec String := do
  let doOne : Parsec (Option String) := fun it =>
    if it.hasNext then
      match mathContent it with
      | .success it ret =>
        match ret with
        | .some ret => .success it (.some ret)
        | .none =>
          match it.curr with
          | '{' => (.some <$> bracedContent texDiacritics) it
          | '\\' => (.some <$> texDiacriticsCommand texDiacritics) it
          | '}' => .success it .none
          | _ => (.some <$> normalChars) it
      | .error it err => .error it err
    else
      .success it .none
  return String.join (← manyOptions doOne).toList

/-- Remove all braces except for those in math environment,
and error on any TeX commands which are not in math environment. -/
partial def removeBraces : Parsec String := do
  let doOne : Parsec (Option String) := fun it =>
    if it.hasNext then
      match mathContent it with
      | .success it ret =>
        match ret with
        | .some ret => .success it (.some ret)
        | .none =>
          match it.curr with
          | '{' => (.some <$> bracedContent' removeBraces) it
          | '}' => .success it .none
          | _ => (.some <$> normalChars) it
      | .error it err => .error it err
    else
      .success it .none
  return String.join (← manyOptions doOne).toList

end DocGen4.Bibtex
