# doc-gen4
Document Generator for Lean 4

## Usage
`doc-gen4` is the easiest to use via its custom Lake facet, in order
to do this you have to add it to your `lakefile.lean` like this:
```
meta if get_config? env = some "dev" then -- dev is so not everyone has to build it
require «doc-gen4» from git "https://github.com/leanprover/doc-gen4" @ "main"
```
Then you can generate documentation for an entire library using:
```
lake -Kenv=dev build Test:docs
```
If you have multiple libraries you want to generate documentation for
the recommended way right now is to run it for each library.

## Development of doc-gen4
You can build docs using a modified `doc-gen4` as follows:  Replace the `from git  "..." @ "main"` in the `lakefile.lean` with just `from "..."` using the path to the modified version of `doc-gen4`.  E.g. if the
path to the modified version of `doc-gen4` is `../doc-gen4`, it would be:
```
meta if get_config? env = some "dev" then -- dev is so not everyone has to build it
require «doc-gen4» from "../doc-gen4"
```

The root of the built docs will be `build/docs/index.html`.  However, due to the "Same Origin Policy", the
generated website will be partially broken if you just open the generated html files in your browser.  You
need to serve them from a proper http server for it to work.  An easy way to do that is to run 
`python3 -m http.server` from the `build/docs` directory.