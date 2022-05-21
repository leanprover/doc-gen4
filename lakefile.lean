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
      src := Source.git "https://github.com/mhuisi/lean4-cli" "dbbcfdd1780612d7455d7e78af1f4a3562f45beb"
    },
    {
      name := `lake
      src := Source.git "https://github.com/leanprover/lake" "463c4f02a399dd296f9fa00b45a95792b70b6e48"
    }
  ]
}
