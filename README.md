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

1. `doc-gen4 single Init`
2. `doc-gen4 single Std`
3. `doc-gen4 single Lean`
4. `doc-gen4 single Mathlib`

Note that you can also just make a call to submodules so `Mathlib.Algebra`
will work standalone as well. Furthermore one can use the `--ink` flag
here to also generate LeanInk documentation in addition.

The second and last stage is the index one which zips up some
information relevant for the search:
```sh
$ doc-gen4 index Mathlib
```
Now `build/doc` should contain the same files with the same context as if one had run
```
$ doc-gen4 Mathlib
```
