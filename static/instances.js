import { DeclarationDataCenter } from "./declaration-data.js";

annotateInstances();

async function annotateInstances() {
  const dataCenter = await DeclarationDataCenter.init();
  const instanceLists = [...(document.querySelectorAll(".instances-list"))];

  for (const instanceList of instanceLists) {
    const className = instanceList.id.slice("instances-list-".length);
    const instances = dataCenter.instancesForClass(className);
    var innerHTML = "";
    for(var instance of instances) {
      const instanceLink = dataCenter.declNameToLink(instance);
      innerHTML += `<li><a href="${SITE_ROOT}${instanceLink}">${instance}</a></li>`
    }
    instanceList.innerHTML = innerHTML;
  }
}
