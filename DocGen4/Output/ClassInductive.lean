import DocGen4.Output.Template
import DocGen4.Output.Class
import DocGen4.Output.Inductive


namespace DocGen4
namespace Output

def classInductiveToHtml (i : ClassInductiveInfo) : HtmlM (Array Html) := do
  (←inductiveToHtml i.toInductiveInfo).push (←classInstancesToHtml i.instances)

end Output
end DocGen4
