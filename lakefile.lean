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

module_facet docs : FilePath := fun mod => do
  let some docGen4 ← findLeanExe? `«doc-gen4»
    | error "no doc-gen4 executable configuration found in workspace"
  let exeTarget ← docGen4.exe.recBuild
  let modTarget ← mod.leanBin.recBuild
  let buildDir := (← getWorkspace).root.buildDir
  let docFile := mod.filePath (buildDir / "doc") "html"
  let task ← show SchedulerM _ from do
    exeTarget.bindAsync fun exeFile exeTrace => do
    modTarget.bindSync fun _ modTrace => do
      let depTrace := exeTrace.mix modTrace
      buildFileUnlessUpToDate docFile depTrace do
        proc {
          cmd := exeFile.toString
          args := #["single", mod.name.toString]
          env := #[("LEAN_PATH", (← getAugmentedLeanPath).toString)]
        }
  return ActiveTarget.mk docFile task
