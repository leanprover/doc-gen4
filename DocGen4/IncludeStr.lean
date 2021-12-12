import Lean

namespace DocGen4

open Lean System IO Lean.Elab.Term

syntax (name := includeStr) "include_str" str : term

@[termElab includeStr] def includeStrImpl : TermElab := λ stx expectedType? => do
    let str := stx[1].isStrLit?.get!
    let path := FilePath.mk str
    if ←path.pathExists then
      if ←path.isDir then
        throwError s!"{str} is a directory"
      else
        let content ← FS.readFile path
        return mkStrLit content
    else
      throwError s!"\"{str}\" does not exist as a file"

end DocGen4
