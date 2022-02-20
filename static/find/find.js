import { SITE_ROOT } from "../site-root.js";
import { declSearch } from "../search.js";

let splits = window.location.href.split("#");

if (splits.length < 2) {
  window.location.replace(`${SITE_ROOT}/404.html`);
}

declSearch(splits[1]).then((results) => {
  window.location.replace(results[0].link);
}).catch(() => {
  window.location.replace(`${SITE_ROOT}404.html`);
});