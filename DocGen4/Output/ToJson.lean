import Lean
import DocGen4.Process
import DocGen4.Output.Base
import Std.Data.RBMap

namespace DocGen4.Output

open Lean Std

structure JsonDeclaration where
  name : String
  doc : String
  docLink : String
  sourceLink : String
  deriving FromJson, ToJson

structure JsonInstance where
  name : String
  className : String
  deriving FromJson, ToJson

structure JsonModule where
  name : String
  declarations : List JsonDeclaration
  instances : Array JsonInstance
  imports : Array String
  deriving FromJson, ToJson

def DocInfo.toJson (module : Name) (info : Process.DocInfo) : HtmlM JsonDeclaration := do
  let name := info.getName.toString
  let doc := info.getDocString.getD ""
  let docLink ← declNameToLink info.getName
  let sourceLink ← getSourceUrl module info.getDeclarationRange
  pure { name, doc, docLink, sourceLink }

def Process.Module.toJson (module : Process.Module) : HtmlM Json := do
    let mut jsonDecls := []
    let mut instances := #[]
    let declInfo := Process.filterMapDocInfo module.members
    for decl in declInfo do
      jsonDecls := (←DocInfo.toJson module.name decl) :: jsonDecls
      if let .instanceInfo i := decl then
        instances := instances.push { name := i.name.toString, className := i.instClass.toString}
    let jsonMod : JsonModule :=  {
      name := module.name.toString,
      declarations := jsonDecls,
      instances := instances
      imports := module.imports.map Name.toString
    }
    pure $ ToJson.toJson jsonMod

end DocGen4.Output
