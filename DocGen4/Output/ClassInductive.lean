import DocGen4.Output.Template
import DocGen4.Output.Class
import DocGen4.Output.Inductive
import DocGen4.Process


namespace DocGen4
namespace Output

def classInductiveToHtml (i : Process.ClassInductiveInfo) : HtmlM (Array Html) := do
  inductiveToHtml i

end Output
end DocGen4
