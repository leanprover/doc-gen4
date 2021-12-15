/-
Copyright (c) 2021 Henrik Böving. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Henrik Böving
-/
import Lean
import DocGen4.Process
import DocGen4.Output.Base
import DocGen4.Output.Index
import DocGen4.Output.Module
import DocGen4.Output.NotFound

namespace DocGen4

open Lean Std IO System Output

def htmlOutput (result : AnalyzerResult) : IO Unit := do
  -- TODO: parameterize this
  let config := { root := "/", result := result, currentName := none}
  let basePath := FilePath.mk "./build/doc/"
  let indexHtml := ReaderT.run index config 
  let notFoundHtml := ReaderT.run notFound config
  FS.createDirAll basePath
  FS.writeFile (basePath / "index.html") indexHtml.toString
  FS.writeFile (basePath / "style.css") styleCss
  FS.writeFile (basePath / "404.html") notFoundHtml.toString
  FS.writeFile (basePath / "nav.js") navJs
  for (module, content) in result.modules.toArray do
    let moduleHtml := ReaderT.run (moduleToHtml content) config
    let path := basePath / (nameToUrl module)
    FS.createDirAll $ nameToDirectory basePath module
    FS.writeFile path moduleHtml.toString

end DocGen4

