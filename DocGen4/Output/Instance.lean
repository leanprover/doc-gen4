import DocGen4.Output.Template
import DocGen4.Output.Definition

namespace DocGen4
namespace Output

def instanceToHtml (i : InstanceInfo) : HtmlM (Array Html) := definitionToHtml i

end Output
end DocGen4
