import Lake
open Lake DSL

package «doc-gen4» {
  -- add configuration options here
  supportInterpreter := true
  dependencies := #[
    {
      name := `CMark
      src := Source.git "https://github.com/xubaiw/CMark.lean" "0c59e4fa0f8864502dc9e661d437be842d29d708"
    },
    {
      name := `Unicode
      src := Source.git "https://github.com/xubaiw/Unicode.lean" "3b7b85472d42854a474099928a3423bb97d4fa64"
    },
    {
      name := `Cli
      src := Source.git "https://github.com/mhuisi/lean4-cli" "159a20e5e165b1bbe070594b5969d8147241bb04"
    },
    {
      name := `lake
      src := Source.git "https://github.com/leanprover/lake" "cb0eab4cbcfe58090b3c739e1e90982804597704"
    }
  ]
}
