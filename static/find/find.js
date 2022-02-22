/**
 * This module is used for the `/find` endpoint.
 */

import { SITE_ROOT } from "../site-root.js";
import { DeclarationDataCenter } from "../declaration-data.js";

async function findRedirect(pattern, isSource) {
  const dataCenter = await DeclarationDataCenter.init();
  try {
    let results = dataCenter.search(pattern);
    window.location.replace(isSource ? results[0].source : results[0].link);
  } catch {
    window.location.replace(`${SITE_ROOT}404.html`);
  }
}

let hash = window.location.hash.split("#");

if (hash.length == 0) {
  window.location.replace(`${SITE_ROOT}/404.html`);
}

if (hash[1].endsWith("/src")) {
  findRedirect(hash.replace("/src", ""), true);
} else {
  findRedirect(hash, false);
}
