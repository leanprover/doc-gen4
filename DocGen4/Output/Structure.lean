import DocGen4.Output.Arg
import DocGen4.Output.Template
import DocGen4.Output.DocString
import DocGen4.Process

namespace DocGen4
namespace Output

open scoped DocGen4.Jsx
open Lean

private def getShort : Name → String
  | .anonymous => ""
  | .num _ n => toString n
  | .str _ s => s

private def getShort' : Name → Name
  | .anonymous => .anonymous
  | .num _ n => .num .anonymous n
  | .str _ s => .str .anonymous s

/--
Render a single field consisting of its documentation, its name and its type as HTML.
-/
def fieldToHtml (structName : Name) (f : Process.FieldInfo) : HtmlM Html := do
  let shortName := getShort f.name
  let args ← f.args.mapM argToHtml
  if f.isDirect then
    let name := f.name.toString
    let doc : Array HTML ←
      if let some doc := f.doc then
        let renderedDoc ← docStringToHtml doc name
        pure #[<div class="structure_field_doc">[renderedDoc]</div>]
      else
        pure #[]
    pure
      <li id={name} class="structure_field">
        <div class="structure_field_info">{shortName} [args] {" : "} [← renderedCodeToHtml f.type]</div>
        [doc]
      </li>
  else
    -- In some cases, multiple inheritance leads to the generation of projection functions for
    -- inherited fields. In these cases, the generated projection should be a valid link target.
    -- However, just checking for the field name as a suffix of the structure name is not
    -- sufficient, because a user could also define that function, leading to duplicate ids or
    -- incorrect link targets. The solution is to check whether the name matches the pattern, _and_
    -- whether it's defined in the source code range of the structure (the structure elaborator
    -- associates them with the entry in the `extends` clause).
    let projName := structName ++ getShort' f.name
    let inner :=
      <div class="structure_field_info">
        <a href={← declNameToLink f.name}>{shortName}</a> [args] {" : "} [← renderedCodeToHtml f.type]
      </div>
    if (← getResult).containedNames[structName]?.any (·.contains projName) then
      let name := projName.toString
      pure
        <li id={name} class="structure_field inherited_field">
          {inner}
        </li>
    else
      pure
        <li class="structure_field inherited_field">
          {inner}
        </li>

/--
Render all information about a structure as HTML.
-/
def structureToHtml (i : Process.StructureInfo) : HtmlM (Array Html) := do
  let structureHtml ← do
    if Name.isSuffixOf `mk i.ctor.name then
      pure
        <ul class="structure_fields" id={i.ctor.name.toString}>
          [← i.fieldInfo.mapM (fieldToHtml i.name)]
        </ul>
    else
      let ctorShortName := i.ctor.name.componentsRev.head!.toString
      pure
        <ul class="structure_ext">
          <li id={i.ctor.name.toString} class="structure_ext_ctor">{s!"{ctorShortName} "} :: (</li>
          <ul class="structure_ext_fields">
            [← i.fieldInfo.mapM (fieldToHtml i.name)]
          </ul>
          <li class="structure_ext_ctor">)</li>
        </ul>
  return #[structureHtml]

end Output
end DocGen4
