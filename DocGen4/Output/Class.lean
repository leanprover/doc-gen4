import DocGen4.Output.Template
import DocGen4.Output.Structure

namespace DocGen4
namespace Output

open scoped DocGen4.Jsx

def classToHtml (i : ClassInfo) : HtmlM (Array Html) := do
  structureToHtml i.toStructureInfo

end Output
end DocGen4
