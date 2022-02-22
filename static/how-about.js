/**
 * This module implements the `howabout` functionality in the 404 page.
 */

import { DeclarationDataCenter } from "./declaration-data.js";

const HOW_ABOUT = document.querySelector("#howabout");

if (HOW_ABOUT) {
  HOW_ABOUT.innerText = "Please wait a second. I'll try to help you.";

  HOW_ABOUT.parentNode
    .insertBefore(document.createElement("pre"), HOW_ABOUT)
    .appendChild(document.createElement("code")).innerText =
    window.location.href.replace(/[/]/g, "/\u200b");

  // TODO: add how about functionality for similar page as well.
  const pattern = window.location.hash.replace("#", "");

  DeclarationDataCenter.init().then((dataCenter) => {
    let results = dataCenter.search(pattern);
    if (results.length > 0) {
      HOW_ABOUT.innerText = "How about one of these instead:";
      const ul = HOW_ABOUT.appendChild(document.createElement("ul"));
      for (const { decl, link } of results) {
        const li = ul.appendChild(document.createElement("li"));
        const a = li.appendChild(document.createElement("a"));
        a.href = link;
        a.appendChild(document.createElement("code")).innerText = decl;
      }
    } else {
      HOW_ABOUT.innerText = "Sorry, I cannot find any similar declarations. Check the link or use the module navigation to find what you want :P";
    }
  });
}
