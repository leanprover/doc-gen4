/**
 * This module is used for the `/find` endpoint.
 */

import { SITE_ROOT } from "../site-root.js";
import { DeclarationDataCenter } from "../declaration-data.js";

async function findRedirect(pattern, isSource) {
  const dataCenter = await DeclarationDataCenter.init();
  let results = dataCenter.search(pattern, true);
  if (results.length == 0) {
    // if there is no exact match, redirect to 404 page to use fuzzy match
    window.location.replace(`${SITE_ROOT}404.html#${pattern ?? ""}`);
  } else {
    // redirect to doc or source page.
    window.location.replace(isSource ? results[0].sourceLink : results[0].link);
  }
}

let hash = window.location.hash.replace("#", "");

if (hash.endsWith("/src")) {
  findRedirect(hash.replace("/src", ""), true);
} else {
  findRedirect(hash, false);
}
