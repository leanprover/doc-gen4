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
      src := Source.git "https://github.com/mhuisi/lean4-cli" "1f8663e3dafdcc11ff476d74ef9b99ae5bdaedd3"
    },
    {
      name := `lake
      src := Source.git "https://github.com/leanprover/lake" "9378575b5575f49a185d50130743a190a9be2f82"
    },
    {
      name := `leanInk
      src := Source.git "https://github.com/leanprover/LeanInk.git" "31b9bce0d08e282d777a6c88f653d41a18c5b894"
    }
    
  ]
}
