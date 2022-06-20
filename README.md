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
