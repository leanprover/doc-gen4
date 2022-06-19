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
  "https://github.com/xubaiw/CMark.lean" @ "b3848a9c7781b3a0dda4d78b62a7f15a7941462d"

require Unicode from git
  "https://github.com/hargonix/Unicode.lean" @ "603450c82cf5066c6db6df0e8ee115f93d71f5fb"

require Cli from git
  "https://github.com/hargonix/lean4-cli" @ "f8fe306d00b31cdfcf5d24e6c0d050e34bec6bb0"

require lake from git
  "https://github.com/leanprover/lake" @ "12e2463b35829368a59d18a5504dd2f73ac1621d"

require leanInk from git
  "https://github.com/leanprover/LeanInk" @ "0a160d91458c1873937449a7c78d25b34b8686df"
