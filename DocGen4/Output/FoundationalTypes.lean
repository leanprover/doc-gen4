import DocGen4.Output.Template
import DocGen4.Output.Inductive

namespace DocGen4.Output

open scoped DocGen4.Jsx

def foundationalTypes : BaseHtmlM Html := templateLiftExtends (baseHtml "Foundational Types") do
  pure <|
    <main>
      <a id="top"></a>
      <h1>Foundational Types</h1>

      <p>Some of Lean's types are not defined in any Lean source files (even the <code>prelude</code>) since they come from its foundational type theory. This page provides basic documentation for these types.</p>
      <p>For a more in-depth explanation of Lean's type theory, refer to
      <a href="https://leanprover.github.io/theorem_proving_in_lean4/Dependent-Type-Theory/">TPiL</a>.</p>


      <h2 id="codesort-ucode"><code>Sort u</code></h2>
      <p><code>Sort u</code> is the type of types in Lean, and <code>Sort u : Sort (u + 1)</code>.</p>
      {← instancesForToHtml `_builtin_sortu}

      <h2 id="codetype-ucode"><code>Type u</code></h2>
      <p><code>Type u</code> is notation for <code>Sort (u + 1)</code>.</p>
      {← instancesForToHtml `_builtin_typeu}

      <h2 id="codepropcode"><code>Prop</code></h2>
      <p><code>Prop</code> is notation for <code>Sort 0</code>.</p>
      {← instancesForToHtml `_builtin_prop}

      <h2 id="pi-types-codeπ-a--α-β-acode">Pi types, <code>{"(a : α) → β a"}</code></h2>
      <p>The type of dependent functions is known as a pi type.
      Non-dependent functions and implications are a special case.</p>
      <p>Note that these can also be written with the alternative notations:</p>
      <ul>
      <li><code>∀ a : α, β a</code>, conventionally used where <code>β a : Prop</code>.</li>
      <li><code>(a : α) → β a</code></li>
      <li><code>α → γ</code>, possible only if <code>β a = γ</code> for all <code>a</code>.</li>
      </ul>
      <p>Lean also permits ASCII-only spellings of the three variants:</p>
      <ul>
      <li><code>forall a : A, B a</code>, for <code>{"∀ a : α, β a"}</code></li>
      <li><code>{"(a : A) -> B a"}</code>, for <code>(a : α) → β a</code></li>
      <li><code>{"A -> B"}</code>, for <code>α → β</code></li>
      </ul>
      <p>Note that despite not itself being a function, <code>(→)</code> is available as infix notation for
      <code>{"fun α β, α → β"}</code>.</p>
      -- TODO: instances for pi types
    </main>

end DocGen4.Output
