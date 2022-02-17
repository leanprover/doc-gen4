import Lake
open Lake DSL

package «doc-gen4» {
  -- add configuration options here
  supportInterpreter := true
  dependencies := #[
    {
      name := `CMark
      src := Source.git "https://github.com/xubaiw/CMark.lean" "0c59e4f"
    }
  ]
}
