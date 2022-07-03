import Lake
open Lake DSL

package «doc-gen4»

lean_lib DocGen4

@[defaultTarget]
lean_exe «doc-gen4» {
  root := `Main
  supportInterpreter := true
}

require CMark from git
  "https://github.com/xubaiw/CMark.lean" @ "192939e27263b0932700ade3442e1bf2ce67c3a6"

require Unicode from git
  "https://github.com/hargonix/Unicode.lean" @ "b73232aeefd6391951f9bd256e3dc4ec937c7238"

require Cli from git
  "https://github.com/mhuisi/lean4-cli" @ "f7590ce072b0321752a7b9942892d0104dee4036"

require lake from git
  "https://github.com/leanprover/lake" @ "401e738e4ca989ced8d9bb0cf7f66be9133fc435"

require leanInk from git
  "https://github.com/hargonix/LeanInk" @ "doc-gen"
