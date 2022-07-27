/-
Copyright (c) 2022 Henrik Böving. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Henrik Böving
-/
import Lean
import LeanInk.Analysis
import LeanInk.Annotation
import DocGen4.LeanInk.Output
import DocGen4.Output.Base

namespace DocGen4.Process.LeanInk

open Lean
open DocGen4.Output

def docGenOutput (as : List LeanInk.Annotation.Annotation) : HtmlT LeanInk.AnalysisM UInt32 := do
  let some modName ← getCurrentName | unreachable!
  let srcHtml ← LeanInk.Annotation.Alectryon.renderAnnotations as
  let srcDir := moduleNameToDirectory srcBasePath modName
  let srcPath := moduleNameToFile srcBasePath modName
  IO.FS.createDirAll srcDir
  IO.FS.writeFile srcPath srcHtml.toString
  pure 0

def execAuxM : HtmlT LeanInk.AnalysisM UInt32 := do
  let ctx ← readThe SiteContext
  let baseCtx ← readThe SiteBaseContext
  let outputFn := (docGenOutput · |>.run ctx baseCtx)
  return ← LeanInk.Analysis.runAnalysis { 
    name := "doc-gen4"
    genOutput := outputFn
  } 

def execAux (config : LeanInk.Configuration) : HtmlT IO UInt32 := do
  execAuxM.run (←readThe SiteContext) (←readThe SiteBaseContext) |>.run config

@[implementedBy enableInitializersExecution]
private def enableInitializersExecutionWrapper : IO Unit := pure ()

def runInk (sourceFilePath : System.FilePath) : HtmlT IO Unit := do
  let contents ← IO.FS.readFile sourceFilePath
  let config := { 
    inputFilePath := sourceFilePath
    inputFileContents := contents
    lakeFile := none
    verbose := false
    prettifyOutput := true
    experimentalTypeInfo := true
    experimentalDocString := true
    experimentalSemanticType := true
  }
  enableInitializersExecutionWrapper
  if (← execAux config) != 0 then
    throw <| IO.userError s!"Analysis for \"{sourceFilePath}\" failed!"

end DocGen4.Process.LeanInk
