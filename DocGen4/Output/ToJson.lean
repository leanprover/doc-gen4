import Lean
import DocGen4.Process
import DocGen4.Output.Base
import DocGen4.Output.Module
import Lean.Data.RBMap

namespace DocGen4.Output

open Lean

structure JsonDeclarationInfo where
  name : String
  kind : String
  doc : String
  docLink : String
  sourceLink : String
  line : Nat
  deriving FromJson, ToJson

structure JsonDeclaration where
  info : JsonDeclarationInfo
  header : String
deriving FromJson, ToJson

structure JsonInstance where
  name : String
  className : String
  typeNames : Array String
  deriving FromJson, ToJson

structure JsonModule where
  name : String
  declarations : List JsonDeclaration
  instances : Array JsonInstance
  imports : Array String
  deriving FromJson, ToJson

structure JsonHeaderIndex where
  headers : List (String × String) := []

structure JsonIndexedDeclarationInfo where
  kind : String
  docLink : String
  deriving FromJson, ToJson

structure JsonIndex where
  declarations : List (String × JsonIndexedDeclarationInfo) := []
  instances : HashMap String (RBTree String Ord.compare) := .empty
  importedBy : HashMap String (Array String) := .empty
  modules : List (String × String) := []
  instancesFor : HashMap String (RBTree String Ord.compare) := .empty

instance : ToJson JsonHeaderIndex where
  toJson idx := Json.mkObj <| idx.headers.map (fun (k, v) => (k, toJson v))

instance : ToJson JsonIndex where
  toJson idx := Id.run do
    let jsonDecls := Json.mkObj <| idx.declarations.map (fun (k, v) => (k, toJson v))
    let jsonInstances := Json.mkObj <| idx.instances.toList.map (fun (k, v) => (k, toJson v.toArray))
    let jsonImportedBy := Json.mkObj <| idx.importedBy.toList.map (fun (k, v) => (k, toJson v))
    let jsonModules := Json.mkObj <| idx.modules.map (fun (k, v) => (k, toJson v))
    let jsonInstancesFor := Json.mkObj <| idx.instancesFor.toList.map (fun (k, v) => (k, toJson v.toArray))
    let finalJson := Json.mkObj [
      ("declarations", jsonDecls),
      ("instances", jsonInstances),
      ("importedBy", jsonImportedBy),
      ("modules", jsonModules),
      ("instancesFor", jsonInstancesFor)
    ]
    return finalJson

def JsonHeaderIndex.addModule (index : JsonHeaderIndex) (module : JsonModule) : JsonHeaderIndex :=
  let merge idx decl := { idx with headers := (decl.info.name, decl.header) :: idx.headers }
  module.declarations.foldl merge index

def JsonIndex.addModule (index : JsonIndex) (module : JsonModule) : BaseHtmlM JsonIndex := do
  let mut index := index
  let newModule := (module.name, ← moduleNameToLink (String.toName module.name))
  let newDecls := module.declarations.map (fun d => (d.info.name, {
    kind := d.info.kind,
    docLink := d.info.docLink,
  }))
  index := { index with
    modules := newModule :: index.modules
    declarations := newDecls ++ index.declarations
  }
  -- TODO: In theory one could sort instances and imports by name and batch the writes
  for inst in module.instances do
    let mut insts := index.instances.findD inst.className {}
    insts := insts.insert inst.name
    index := { index with instances := index.instances.insert inst.className insts }
    for typeName in inst.typeNames do
      let mut instsFor := index.instancesFor.findD typeName {}
      instsFor := instsFor.insert inst.name
      index := { index with instancesFor := index.instancesFor.insert typeName instsFor }

  for imp in module.imports do
    let mut impBy := index.importedBy.findD imp #[]
    impBy := impBy.push module.name
    index := { index with importedBy := index.importedBy.insert imp impBy }
  return index

def DocInfo.toJson (module : Name) (info : Process.DocInfo) : HtmlM JsonDeclaration := do
  let name := info.getName.toString
  let kind := info.getKind
  let doc := info.getDocString.getD ""
  let docLink ← declNameToLink info.getName
  let sourceLink ← getSourceUrl module info.getDeclarationRange
  let line := info.getDeclarationRange.pos.line
  let header := (← docInfoHeader info).toString
  let info := { name, kind, doc, docLink, sourceLink, line }
  return { info, header }

def Process.Module.toJson (module : Process.Module) : HtmlM Json := do
    let mut jsonDecls := []
    let mut instances := #[]
    let declInfo := Process.filterDocInfo module.members
    for decl in declInfo do
      jsonDecls := (← DocInfo.toJson module.name decl) :: jsonDecls
      if let .instanceInfo i := decl then
        instances := instances.push {
          name := i.name.toString,
          className := i.className.toString
          typeNames := i.typeNames.map Name.toString
        }
    let jsonMod : JsonModule :=  {
      name := module.name.toString,
      declarations := jsonDecls,
      instances,
      imports := module.imports.map Name.toString
    }
    return ToJson.toJson jsonMod

end DocGen4.Output
