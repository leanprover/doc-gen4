/**
 * This module is a wrapper that facilitates manipulating the declaration data.
 *
 * Please see {@link DeclarationDataCenter} for more information.
 */

import { SITE_ROOT } from "./site-root.js";

/**
 * The DeclarationDataCenter is used for declaration searching.
 *
 * For usage, see the {@link init} and {@link search} methods.
 */
export class DeclarationDataCenter {
  /**
   * The declaration data. Users should not interact directly with this field.
   *
   * *NOTE:* This is not made private to support legacy browsers.
   */
  declarationData = null;

  /**
   * Used to implement the singleton, in case we need to fetch data mutiple times in the same page.
   */
  static singleton = null;

  /**
   * Construct a DeclarationDataCenter with given data.
   *
   * Please use {@link DeclarationDataCenter.init} instead, which automates the data fetching process.
   * @param {*} declarationData
   */
  constructor(declarationData) {
    this.declarationData = declarationData;
  }

  /**
   * The actual constructor of DeclarationDataCenter
   * @returns {Promise<DeclarationDataCenter>}
   */
  static async init() {
    if (!DeclarationDataCenter.singleton) {
      const dataUrl = new URL(
        `${SITE_ROOT}declaration-data.bmp`,
        window.location
      );
      const response = await fetch(dataUrl);
      const json = await response.json();
      // the data is a map of name (original case) to declaration data.
      const data = new Map(
        json.map(({ name, doc, link, source: sourceLink }) => [
          name,
          {
            name,
            lowerName: name.toLowerCase(),
            lowerDoc: doc.toLowerCase(),
            link,
            sourceLink,
          },
        ])
      );
      DeclarationDataCenter.singleton = new DeclarationDataCenter(data);
    }
    return DeclarationDataCenter.singleton;
  }

  /**
   * Search for a declaration.
   * @returns {Array<any>}
   */
  search(pattern, strict = false) {
    if (!pattern) {
      return [];
    }
    if (strict) {
      let decl = this.declarationData.get(pattern);
      return decl ? [decl] : [];
    } else {
      return getMatches(this.declarationData, pattern);
    }
  }
}

function isSeparater(char) {
  return char === "." || char === "_";
}

// HACK: the fuzzy matching is quite hacky

function matchCaseSensitive(declName, lowerDeclName, pattern) {
  let i = 0,
    j = 0,
    err = 0,
    lastMatch = 0;
  while (i < declName.length && j < pattern.length) {
    if (pattern[j] === declName[i] || pattern[j] === lowerDeclName[i]) {
      err += (isSeparater(pattern[j]) ? 0.125 : 1) * (i - lastMatch);
      if (pattern[j] !== declName[i]) err += 0.5;
      lastMatch = i + 1;
      j++;
    } else if (isSeparater(declName[i])) {
      err += 0.125 * (i + 1 - lastMatch);
      lastMatch = i + 1;
    }
    i++;
  }
  err += 0.125 * (declName.length - lastMatch);
  if (j === pattern.length) {
    return err;
  }
}

function getMatches(declarations, pattern, maxResults = 30) {
  const lowerPats = pattern.toLowerCase().split(/\s/g);
  const patNoSpaces = pattern.replace(/\s/g, "");
  const results = [];
  for (const {
    name,
    lowerName,
    lowerDoc,
    link,
    sourceLink,
  } of declarations.values()) {
    let err = matchCaseSensitive(name, lowerName, patNoSpaces);
    // match all words as substrings of docstring
    if (
      err >= 3 &&
      pattern.length > 3 &&
      lowerPats.every((l) => lowerDoc.indexOf(l) != -1)
    ) {
      err = 3;
    }
    if (err !== undefined) {
      results.push({ name, err, lowerName, lowerDoc, link, sourceLink });
    }
  }
  return results.sort(({ err: a }, { err: b }) => a - b).slice(0, maxResults);
}
