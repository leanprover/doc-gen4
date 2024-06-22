import DocGen4.Output.Template
import DocGen4.Output.Class
import DocGen4.Output.Inductive
import DocGen4.Process


namespace DocGen4
namespace Output

def classInductiveToHtml (i : Process.ClassInductiveInfo) (backrefs : Array BackrefItem) :
    HtmlM (Array Html × Array BackrefItem) := do
  inductiveToHtml i backrefs

end Output
end DocGen4
