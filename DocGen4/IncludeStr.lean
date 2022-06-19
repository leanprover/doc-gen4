/-
Copyright (c) 2021 Henrik Böving. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Henrik Böving
-/
import Lean

namespace DocGen4

open Lean System IO Lean.Elab.Term FS

deriving instance DecidableEq for FileType

/--
  Traverse all subdirectories fo `f` to find if one satisfies `p`.
-/
partial def traverseDir (f : FilePath) (p : FilePath → IO Bool) : IO (Option FilePath) := do
  if (← p f) then
    return f
  for d in (← System.FilePath.readDir f) do
    let subDir := d.path
    let metadata ← subDir.metadata
    if metadata.type = FileType.dir then
      if let some p ← traverseDir subDir p then
        return p
  return none

syntax (name := includeStr) "include_str" str : term

/--
Provides a way to include the contents of a file at compile time as a String.
This is used to include things like the CSS and JS in the binary so we
don't have to carry them around as files.
-/
@[termElab includeStr] def includeStrImpl : TermElab := λ stx _ => do
  let str := stx[1].isStrLit?.get!
  let srcPath := FilePath.mk $ ←getFileName
  let currentDir ← IO.currentDir
  -- HACK: Currently we cannot get current file path in VSCode, we have to traversely find the matched subdirectory in the current directory.
  if let some path ← match srcPath.parent with
  | some p => pure $ some $ p / str
  | none => do
    let foundDir ← traverseDir currentDir λ p => p / str |>.pathExists 
    pure $ foundDir.map (· / str)
  then 
    if ←path.pathExists then
      if ←path.isDir then
        throwError s!"{str} is a directory"
      else
        let content ← FS.readFile path
        pure $ mkStrLit content
    else
      throwError s!"{path} does not exist as a file"
  else
    throwError s!"No such file in whole directory: {str}"

end DocGen4
