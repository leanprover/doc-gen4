import { SITE_ROOT } from "../site-root.js";
import { declSearch } from "../search.js";

async function findRedirect(query, isSource) {
  return declSearch(query).then((results) => {
    window.location.replace(isSource? results[0].source : results[0].link);
  }).catch(() => {
    window.location.replace(`${SITE_ROOT}404.html`);
  });
}

let splits = window.location.href.split("#");

if (splits.length < 2) {
  window.location.replace(`${SITE_ROOT}/404.html`);
}

if (splits[1].endsWith("/src")) {
  findRedirect(splits[1].replace("/src", ""), true);
} else {
  findRedirect(splits[1], false);
}
