/**
 * This module is used to handle user's interaction with the search form.
 */

import { DeclarationDataCenter } from "./declaration-data.js";

const SEARCH_FORM = document.querySelector("#search_form");
const SEARCH_INPUT = SEARCH_FORM.querySelector("input[name=q]");

// Create an `div#search_results` to hold all search results.
let sr = document.createElement("div");
sr.id = "search_results";
SEARCH_FORM.appendChild(sr);

/**
 * Attach `selected` class to the the selected search result.
 */
function handleSearchCursorUpDown(down) {
  const sel = sr.querySelector(`.selected`);
  if (sel) {
    sel.classList.remove("selected");
    const toSelect = down
      ? sel.nextSibling || sr.firstChild
      : sel.previousSibling || sr.lastChild;
    toSelect && toSelect.classList.add("selected");
  } else {
    const toSelect = down ? sr.firstChild : sr.lastChild;
    toSelect && toSelect.classList.add("selected");
  }
}

/**
 * Perform search (when enter is pressed).
 */
function handleSearchEnter() {
  const sel = sr.querySelector(`.selected`) || sr.firstChild;
  sel.click();
}

/**
 * Allow user to navigate search results with up/down arrow keys, and choose with enter.
 */
SEARCH_INPUT.addEventListener("keydown", (ev) => {
  switch (ev.key) {
    case "Down":
    case "ArrowDown":
      ev.preventDefault();
      handleSearchCursorUpDown(true);
      break;
    case "Up":
    case "ArrowUp":
      ev.preventDefault();
      handleSearchCursorUpDown(false);
      break;
    case "Enter":
      ev.preventDefault();
      handleSearchEnter();
      break;
  }
});

/**
 * Remove all children of a DOM node.
 */
function removeAllChildren(node) {
  while (node.firstChild) {
    node.removeChild(node.lastChild);
  }
}

/**
 * Handle user input and perform search.
 */
function handleSearch(dataCenter, err, ev) {
  const text = ev.target.value;

  // If no input clear all.
  if (!text) {
    sr.removeAttribute("state");
    removeAllChildren(sr);
    return;
  }

  // searching
  sr.setAttribute("state", "loading");

  if (dataCenter) {
    const result = dataCenter.search(text, false);

    // in case user has updated the input.
    if (ev.target.value != text) return;
  
    // update search results
    removeAllChildren(sr);
    for (const { name, docLink } of result) {
      const d = sr.appendChild(document.createElement("a"));
      d.innerText = name;
      d.title = name;
      d.href = SITE_ROOT + docLink;
    }
  }
  // handle error
  else {
    removeAllChildren(sr);
    const d = sr.appendChild(document.createElement("a"));
    d.innerText = `Cannot fetch data, please check your network connection.\n${err}`;
  }
  sr.setAttribute("state", "done");
}

DeclarationDataCenter.init()
  .then((dataCenter) => {
    // Search autocompletion.
    SEARCH_INPUT.addEventListener("input", ev => handleSearch(dataCenter, null, ev));
  })
  .catch(e => {
    SEARCH_INPUT.addEventListener("input", ev => handleSearch(null, e, ev));
  });
