import DocGen4.Output.Template
import DocGen4.Output.Inductive
import DocGen4.Output.Structure
import DocGen4.Output.Class
import DocGen4.Output.Definition
import DocGen4.Output.Instance
import DocGen4.Output.ClassInductive
import DocGen4.Output.DocString
import LeanInk

namespace DocGen4
namespace Output

open scoped DocGen4.Jsx
open Lean System

def moduleToSrcHtml (module : Module) (inputPath lakefilePath : FilePath) : HtmlT IO Html := withReader (setCurrentName module.name) do
  let config := {
    inputFilePath := inputPath
    inputFileContents := (← IO.FS.readFile inputPath)
    lakeFile := lakefilePath
    verbose := false
    experimentalTypeInfo := true
    experimentalDocString := true
    experimentalSemanticType := true
    : LeanInk.Configuration
  }
  let result ← LeanInk.Analysis.analyzeInput config
  let annotation ← LeanInk.Annotation.annotateFile result config
  let fragments ← LeanInk.Annotation.Alectryon.annotateFileWithCompounds [] config.inputFileContents annotation config
  let rawContents ← LeanInk.generateOutput fragments.toArray config
  return (
    <html>
      <p>{inputPath.toString}</p>
      <p>{lakefilePath.toString}</p>
      <h2>Raw Contents</h2>
      <p>{rawContents}</p>
    </html>
  )