/-
Copyright (c) 2022 Henrik Böving. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Henrik Böving
-/
import Lean.Data.Json

namespace DocGen4.Output.LeanInk

open Lean
open IO

def runInk (inkPath : System.FilePath) (sourceFilePath : System.FilePath) : IO Json := do
  let arguments := #[
    "analyze", sourceFilePath.toString,
    "--lake", "lakefile.lean",
    "--x-enable-type-info",
    "--x-enable-docStrings",
    "--x-enable-semantic-token"
  ]
  let inkProcess ← Process.spawn {
    stdin := Process.Stdio.null
    stdout := Process.Stdio.piped
    stderr := Process.Stdio.piped
    cmd := inkPath.toString
    args := arguments
  }
  match (←inkProcess.wait) with
  | 0 =>
    let outputFilePath := sourceFilePath.withExtension "lean.leanInk"
    let output ← FS.readFile outputFilePath
    FS.removeFile outputFilePath
    match Json.parse output with
    | .ok out => pure out
    | .error err =>
      throw $ IO.userError s!"LeanInk returned invalid JSON for file: {sourceFilePath}:\n{err}"
  | code =>
    throw $ IO.userError s!"LeanInk exited with code {code} for file: {sourceFilePath}:\n{←inkProcess.stderr.readToEnd}"

end DocGen4.Output.LeanInk
