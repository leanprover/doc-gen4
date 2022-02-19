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
      src := Source.git "https://github.com/xubaiw/Unicode.lean" "88ad4aacfcc7ab941a22c54de3e4fef0809cda87"
    }
  ]
}
