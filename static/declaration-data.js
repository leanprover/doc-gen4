/**
 * This module is a wrapper that facilitates manipulating the declaration data.
 *
 * Please see {@link DeclarationDataCenter} for more information.
 */

const CACHE_DB_NAME = "declaration-data";
const CACHE_DB_VERSION = 1;
const CACHE_DB_KEY = "DECLARATIONS_KEY";

async function fetchModuleData(module) {
  const moduleDataUrl = new URL(
    `${SITE_ROOT}declaration-data-${module}.bmp`,
    window.location
  );
  const moduleData = await fetch(moduleDataUrl);
  const moduleDataJson = await moduleData.json();
  return moduleDataJson;
}

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
      const dataListUrl = new URL(
        `${SITE_ROOT}declaration-data.bmp`,
        window.location
      );

      // try to use cache first
      const data = await fetchCachedDeclarationData().catch(_e => null);
      if (data) {
        // if data is defined, use the cached one.
        DeclarationDataCenter.singleton = new DeclarationDataCenter(data);
      } else {
        // undefined. then fetch the data from the server.
        const dataListRes = await fetch(dataListUrl);
        const dataListJson = await dataListRes.json();

        // TODO: this is probably kind of inefficient
        const dataJsonUnflattened = await Promise.all(dataListJson.map(fetchModuleData));

        const dataJson = dataJsonUnflattened.flat();
        // the data is a map of name (original case) to declaration data.
        const data = new Map(
          dataJson.map(({ name, doc, docLink, sourceLink }) => [
            name,
            {
              name,
              lowerName: name.toLowerCase(),
              lowerDoc: doc.toLowerCase(),
              docLink,
              sourceLink,
            },
          ])
        );
        await cacheDeclarationData(data);
        DeclarationDataCenter.singleton = new DeclarationDataCenter(data);
      }
    }
    return DeclarationDataCenter.singleton;
  }

  /**
   * Search for a declaration.
   * @returns {Array<any>}
   */
  search(pattern, strict = true) {
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
    docLink,
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
      results.push({
        name,
        err,
        lowerName,
        lowerDoc,
        docLink,
        sourceLink,
      });
    }
  }
  return results.sort(({ err: a }, { err: b }) => a - b).slice(0, maxResults);
}

// TODO: refactor the indexedDB part to be more robust

/**
 * Get the indexedDB database, automatically initialized.
 * @returns {Promise<IDBDatabase>}
 */
async function getDeclarationDatabase() {
  return new Promise((resolve, reject) => {
    const request = indexedDB.open(CACHE_DB_NAME, CACHE_DB_VERSION);

    request.onerror = function (event) {
      reject(
        new Error(
          `fail to open indexedDB ${CACHE_DB_NAME} of version ${CACHE_DB_VERSION}`
        )
      );
    };
    request.onupgradeneeded = function (event) {
      let db = event.target.result;
      // We only need to store one object, so no key path or increment is needed.
      db.createObjectStore("declaration");
    };
    request.onsuccess = function (event) {
      resolve(event.target.result);
    };
  });
}

/**
 * Store data in indexedDB object store.
 * @param {Map<string, any>} data
 */
async function cacheDeclarationData(data) {
  let db = await getDeclarationDatabase();
  let store = db
    .transaction("declaration", "readwrite")
    .objectStore("declaration");
  return new Promise((resolve, reject) => {
    let clearRequest = store.clear();
    let addRequest = store.add(data, CACHE_DB_KEY);

    addRequest.onsuccess = function (event) {
      resolve();
    };
    addRequest.onerror = function (event) {
      reject(new Error(`fail to store declaration data`));
    };
    clearRequest.onerror = function (event) {
      reject(new Error("fail to clear object store"));
    };
  });
}

/**
 * Retrieve data from indexedDB database.
 * @returns {Promise<Map<string, any>|undefined>}
 */
async function fetchCachedDeclarationData() {
  let db = await getDeclarationDatabase();
  let store = db
    .transaction("declaration", "readonly")
    .objectStore("declaration");
  return new Promise((resolve, reject) => {
    let transactionRequest = store.get(CACHE_DB_KEY);
    transactionRequest.onsuccess = function (event) {
      resolve(event.result);
    };
    transactionRequest.onerror = function (event) {
      reject(new Error(`fail to store declaration data`));
    };
  });
}
