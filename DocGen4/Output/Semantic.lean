import DocGen4.Output.Template
import DocGen4.Output.DocString
import Lean.Data.Xml

open Lean Xml

namespace DocGen4
namespace Output

instance : ToString $ Array Element where
  toString xs := xs.map toString |>.foldl String.append ""

instance : Coe Element Content where
  coe e := Content.Element e

-- TODO: syntax metaprogramming and basic semantic data

def semanticXml (i : DocInfo) : HtmlM $ Array Element := do
  pure #[
    Element.Element
      "rdf:RDF"
        (Std.RBMap.fromList [
          ("xmlns:rdf", "http://www.w3.org/1999/02/22-rdf-syntax-ns#"),
          ("xmlns:docgen4", s!"{←getRoot}semactic/docgen4.xml#")
        ] _)
        #[
          Element.Element
          "rdf:Description" 
          (Std.RBMap.fromList [
            ("rdf:about", s!"{←getRoot}semactic/{i.getName.hash}.xml#")
          ] _)
         #[]
        ]
  ]

def schemaXml : HtmlM $ Array Element := do
  pure #[
    Element.Element
      "rdf:RDF"
        (Std.RBMap.fromList [
          ("xmlns:rdf", "http://www.w3.org/1999/02/22-rdf-syntax-ns#"),
          ("xmlns:docgen4", s!"{←getRoot}semactic/docgen4.xml#")
        ] _)
        #[
          Element.Element
          "docgen4:hasInstance" 
          Std.RBMap.empty
          #[
            Element.Element
            "rdfs:type" 
            Std.RBMap.empty
            #[Content.Character "rdf:Property"]
          ]
        ]
  ]

end Output
end DocGen4
