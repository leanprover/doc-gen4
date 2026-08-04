/-
Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Nicolas Rouquette
-/
import Lean

/-!
# Per-declaration extra documentation (`declMathExt`)

A generic extension point: any downstream library can attach an extra block of rendered documentation
to a declaration via `addDeclMath` (typically a block of Markdown containing LaTeX, e.g.
`$$\sigma^0 = a\,\mathrm{NDVI} + \ldots$$`). doc-gen4 appends it to the declaration's docstring in
`DocGen4.Process.getDocString?`, so it flows through the normal, MathJax-processed docstring rendering
path — no special-casing anywhere in the renderer.

This lets a library surface human-friendly notation for a declaration (a math formula, a diagram, a
prose note computed from metadata) without doc-gen4 knowing anything about the library. The data rides
in the environment (serialized into the `.olean`), so it is available to the separate doc-gen4 process
that loads the target project. This mirrors how doc-gen4 already augments docstrings with
`getRecommendedSpellingText` / `getTacticExtensionText`.
-/

namespace DocGen4.Process

open Lean

/--
Environment extension mapping a declaration name to extra rendered documentation. The value is
Markdown and may contain `$…$` / `$$…$$`, which the docstring renderer passes through to MathJax.
-/
initialize declMathExt : SimplePersistentEnvExtension (Name × String) (NameMap String) ←
  registerSimplePersistentEnvExtension {
    addEntryFn := fun m (n, s) => m.insert n s
    addImportedFn := fun ass =>
      ass.foldl (fun m as => as.foldl (fun m (n, s) => m.insert n s) m) ({} : NameMap String)
  }

/--
Attach extra rendered documentation `markdown` to declaration `declName`. Downstream libraries call
this (e.g. from an attribute) so doc-gen4 renders `markdown` inside the declaration's docstring.
-/
def addDeclMath [Monad m] [MonadEnv m] (declName : Name) (markdown : String) : m Unit :=
  modifyEnv fun env => declMathExt.addEntry env (declName, markdown)

/-- The extra rendered documentation attached to `declName`, if any. -/
def getDeclMath? (env : Environment) (declName : Name) : Option String :=
  (declMathExt.getState env).get? declName

end DocGen4.Process
