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

/-- Match a TeX command (with `\` removed) starting with a letter.
Return value will add the `\` back. -/
def texCommandAux : Parsec String := attempt do
  let s ← many1Chars asciiLetter
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

/-- Match a TeX command starting with `\`, potentially with trailing whitespaces. -/
def texCommand : Parsec String :=
  pchar '\\' *> (texCommandAux <|> ("\\".push <$> anyChar))

/-- Similar to `texCommand` but it excludes some commands. -/
def texCommand' (exclude : Array String) : Parsec String := attempt do
  let s ← texCommand
  match exclude.find? (· == s.trim) with
  | .some _ => fail ""
  | .none => return s

/-- Match a sequence starting with `{` and ending with `}`. -/
def bracedContent (p : Parsec String) : Parsec String :=
  pchar '{' *> (("{" ++ · ++ "}") <$> p) <* pchar '}'

/-- Similar to `bracedContent` but it does not output braces. -/
def bracedContent' (p : Parsec String) : Parsec String :=
  pchar '{' *> p <* pchar '}'

partial def mathContentAux2 : Parsec String := attempt do
  let ret : Array String ← many <|
    bracedContent mathContentAux2 <|>
    texCommand' #["\\(", "\\)", "\\[", "\\]"] <|>
    normalChars
  return String.join ret.toList

def mathContentAux (beginning ending dollar : String) : Parsec String :=
  pstring beginning *> ((dollar ++ · ++ dollar) <$> mathContentAux2) <* pstring ending

/-- Match a math content. -/
def mathContent : Parsec String :=
  mathContentAux "\\[" "\\]" "$$" <|> mathContentAux "\\(" "\\)" "$" <|>
  mathContentAux "$$" "$$" "$$" <|> mathContentAux "$" "$" "$"

/-- Match a TeX command for diacritics, return the corresponding UTF-8 string. -/
def texDiacriticsCommand : Parsec String := attempt do
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
  | "\\`" => pure "\u0300"
  | "\\'" => pure "\u0301"
  | "\\^" => pure "\u0302"
  | "\\\"" => pure "\u0308"
  | "\\~" => pure "\u0303"
  | "\\=" => pure "\u0304"
  | "\\." => pure "\u0307"
  | "\\u" => pure "\u0306"
  | "\\v" => pure "\u030C"
  | "\\H" => pure "\u030B"
  | "\\t" => pure "\u0361"
  | "\\c" => pure "\u0327"
  | "\\d" => pure "\u0323"
  | "\\b" => pure "\u0331"
  | "\\k" => pure "\u0328"
  | _ => fail s!"unsupported command: {cmd}"

/-- Convert all TeX commands for diacritics into UTF-8 characters,
and stop on any other TeX commands which are not in math environment. -/
partial def texDiacritics : Parsec String := attempt do
  let ret : Array String ← many <|
    bracedContent texDiacritics <|>
    mathContent <|>
    texDiacriticsCommand <|>
    normalChars
  return String.join ret.toList

/-- Remove all braces except for those in math environment,
any stop on any TeX commands which are not in math environment. -/
partial def removeBraces : Parsec String := attempt do
  let ret : Array String ← many <|
    bracedContent' removeBraces <|>
    mathContent <|>
    normalChars
  return String.join ret.toList

end DocGen4.Bibtex
