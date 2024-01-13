# `doc-gen4`
Document Generator for Lean 4

## Usage
`doc-gen4` is the easiest to use via its custom Lake facet, in order
to do this you have to add it to your `lakefile.lean` like this:
```
meta if get_config? env = some "dev" then -- dev is so not everyone has to build it
require «doc-gen4» from git "https://github.com/leanprover/doc-gen4" @ "main"
```

Then update your dependencies:
```
lake -R -Kenv=dev update
```

Then you can generate documentation for an entire library and all files imported
by that library using:
```
lake -R -Kenv=dev build Test:docs
```
If you have multiple libraries you want to generate full documentation for:
```
lake -R -Kenv=dev build Test:docs Foo:docs
```

Note that `doc-gen4` currently always generates documentation for `Lean`, `Init`
and `Lake` in addition to the provided targets.

The root of the built docs will be `.lake/build/doc/index.html`. However, due to the "Same Origin Policy", the
generated website will be partially broken if you just open the generated html files in your browser.  You
need to serve them from a proper http server for it to work. An easy way to do that is to run
`python3 -m http.server` from the `.lake/build/doc` directory.

## Requirements to run `doc-gen4`
In order to compile itself `doc-gen4` requires:
- a Lean 4 or `elan` installation
- a C compiler
- being on a Linux or MacOS machine (other operating systems, including Windows, are not tested)

Apart from this the only requirement for `lake -Kenv=dev build Test:docs` to work is that your
target library builds, that is `lake build Test` exits without an error. If this requirement
is not fulfilled, the documentation generation will fail and you will end up with
partial build artefacts in `.lake/build/doc`. Note that `doc-gen4` is perfectly capable of
generating documentation for Lean code that contains `sorry`, just not for code
that doesn't compile.

If you are working on a project that only partially compiles but can't fix the
errors from the top of your head, you can try to remove `import`s of both the failing files
and all files that refer to the failing ones from your top level library file.
Like this you will end up with an incomplete documentation but at least working
documentation of your project.

Note that we do not recommend this approach and suggest to instead make sure your
projects always compile by using CI to prevent broken code from being added and `sorry`-ing
out things that you intend to complete later.

## Source locations

Source locations default to guessing the Github repo for the library, but different different schemas can be used by setting the `DOCGEN_SRC` environment variable.  For
example, one can use links that open the local source file in VSCode by running lake with:
```
DOCGEN_SRC="vscode" lake -R -Kenv=dev ...
```

The different options are:

 * `DOCGEN_SRC="github"` infers the
   Github project for each library and uses source links to the Github source view.
   This is the default if `DOCGEN_SRC` is unset.
 * `DOCGEN_SRC="file"` creates references to local file references.
 * `DOCGEN_SRC="vscode"` creates [VSCode URLs](https://code.visualstudio.com/docs/editor/command-line#_opening-vs-code-with-urls) to local files.

## How does `docs#Nat.add` from the Lean Zulip work?
If someone sends a message that contains `docs#Nat.add` on the Lean Zulip this will
automatically link to `Nat.add` from the `mathlib4` documentation. The way that this
feature is implemented is by linking to `/find` of generated documentation in the following way:
<https://example.com/path/to/docs/find/?pattern=Nat.add#doc> in the case of the `mathlib4`
documentation this ends up being to: <https://leanprover-community.github.io/mathlib4_docs/find/?pattern=Nat.add#doc>.
If you wish to provide a similar feature to `docs#Nat.add` from the Lean Zulip for your documentation,
this is the way to go.

## Development of doc-gen4
You can build docs using a modified `doc-gen4` as follows:  Replace the `from git  "..." @ "main"` in the `lakefile.lean` with just `from "..."` using the path to the modified version of `doc-gen4`.  E.g. if the
path to the modified version of `doc-gen4` is `../doc-gen4`, it would be:
```
meta if get_config? env = some "dev" then -- dev is so not everyone has to build it
require «doc-gen4» from "../doc-gen4"
```

Note that if you modify the `.js` or `.css` files in `doc-gen4`, they won't necessarily be copied over when
you rebuild the documentation.  You can manually copy the changes to the `.lake/build/doc` directory to make
sure the changes appear, or just do a full recompilation (`lake clean` and `lake build` inside the `doc-gen4`
directory.)
