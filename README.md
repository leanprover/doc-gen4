# doc-gen4
Document Generator for Lean 4

## Usage
You can call `doc-gen4` from the top of a Lake project like this:
```sh
$ /path/to/doc-gen4 Module
```

where `Module` is one or more of the top level modules you want to document.
The tool will then proceed to compile the project using lake (if that hasn't happened yet),
analyze it and put the result in `./build/doc`.

You can optionally provide the path to a `LeanInk` binary using the `--ink` flag which will make
the tool produce `Alectryon` style rendered output along the usual documentation.

You could e.g. host the files locally with the built-in Python webserver:
```sh
$ cd build/doc && python -m http.server
```

### Multi stage
You can also use `doc-gen4` in multiple separate stages to generate the whole documentation.
For example `mathlib4` consists out of 4 modules, the 3 Lean compiler ones and itself:
- `Init`
- `Std`
- `Lean`
- `Mathlib`
The first build stage is to run doc-gen for all modules separately:
1. `doc-gen4 single Init Mathlib`
2. `doc-gen4 single Std Mathlib`
3. `doc-gen4 single Lean Mathlib`
4. `doc-gen4 single Mathlib Mathlib`
We have to pass the `Mathlib` top level module on each invocation here so
it can generate the navbar on the left hand side properly, it will only
generate documentation for its first argument module.

Furthermore one can use the `--ink` flag here to also generate LeanInk
documentation in addition.

The second and last stage is the finalize one which zips up some
information relevant for the search:
```sh
$ doc-gen4 finalize Mathlib
```
Now `build/doc` should contain the same files with the same context as if one had run
```
$ doc-gen4 Mathlib
```
