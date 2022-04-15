import DocGen4.Output.Template
import DocGen4.Output.Inductive
import DocGen4.Output.Structure
import DocGen4.Output.Class
import DocGen4.Output.Definition
import DocGen4.Output.Instance
import DocGen4.Output.ClassInductive
import DocGen4.Output.DocString
import Lean.Elab.Frontend
import Lean.Elab.Import
import Lean.Elab.Command
import Lean.Parser
import LeanInk

namespace DocGen4
namespace Output

open scoped DocGen4.Jsx
open Lean System Elab LeanInk

def configureCommandState (env : Environment) (msg : MessageLog) : Command.State :=
  { Command.mkState env msg with infoState := { enabled := true }}

def analyzeInput : LeanInk.AnalysisM LeanInk.Analysis.AnalysisResult := do
  let config := ← read
  let context := Parser.mkInputContext config.inputFileContents config.inputFileName
  let (header, state, messages) ← Parser.parseHeader context
  let options := Options.empty.setBool `trace.Elab.info true
  let (environment, messages) ← processHeader header options messages context 0
  let commandState := configureCommandState environment messages
  let s ← IO.processCommands context state commandState
  let result ← LeanInk.Analysis.resolveTacticList s.commandState.infoState.trees.toList
  let messages := s.commandState.messages.msgs.toList.filter (λ m => m.endPos.isSome )
  return ← result.insertMessages messages context.fileMap 

def moduleToSrcHtml (module : Module) (inputPath : FilePath) : HtmlT IO Html := withReader (setCurrentName module.name) do
  let config := {
    inputFilePath := inputPath
    inputFileContents := (← IO.FS.readFile inputPath)
    lakeFile := none
    verbose := false
    experimentalTypeInfo := true
    experimentalDocString := true
    experimentalSemanticType := true
    : LeanInk.Configuration
  }
  let result ← analyzeInput config
  let annotation ← LeanInk.Annotation.annotateFile result config
  let fragments ← LeanInk.Annotation.Alectryon.annotateFileWithCompounds [] config.inputFileContents annotation config
  let rawContents ← LeanInk.generateOutput fragments.toArray config
  return (
    <html>
      <p>{inputPath.toString}</p>
      <h2>Raw Contents</h2>
      <p>{rawContents}</p>
    </html>
  )