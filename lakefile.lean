import Lake
import Lake.CLI.Main
open System Lake DSL

package «doc-gen4»

lean_lib DocGen4

@[defaultTarget]
lean_exe «doc-gen4» {
  root := `Main
  supportInterpreter := true
}

require CMark from git
  "https://github.com/xubaiw/CMark.lean" @ "main"

require Unicode from git
  "https://github.com/xubaiw/Unicode.lean" @ "main"

require Cli from git
  "https://github.com/mhuisi/lean4-cli" @ "nightly"

require lake from git
  "https://github.com/leanprover/lake" @ "master"

require leanInk from git
  "https://github.com/hargonix/LeanInk" @ "doc-gen-json"
