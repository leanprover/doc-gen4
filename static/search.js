import { SITE_ROOT } from "./site-root.js";

function isSep(c) {
    return c === '.' || c === '_';
}

function matchCaseSensitive(declName, lowerDeclName, pat) {
    let i = 0, j = 0, err = 0, lastMatch = 0
    while (i < declName.length && j < pat.length) {
        if (pat[j] === declName[i] || pat[j] === lowerDeclName[i]) {
            err += (isSep(pat[j]) ? 0.125 : 1) * (i - lastMatch);
            if (pat[j] !== declName[i]) err += 0.5;
            lastMatch = i + 1;
            j++;
        } else if (isSep(declName[i])) {
            err += 0.125 * (i + 1 - lastMatch);
            lastMatch = i + 1;
        }
        i++;
    }
    err += 0.125 * (declName.length - lastMatch);
    if (j === pat.length) {
        return err;
    }
}

export function loadDecls(searchableDataCnt) {
    return searchableDataCnt.map(({name, description, link}) => [name, name.toLowerCase(), description.toLowerCase(), link]);
}

export function getMatches(decls, pat, maxResults = 30) {
    const lowerPats = pat.toLowerCase().split(/\s/g);
    const patNoSpaces = pat.replace(/\s/g, '');
    const results = [];
    for (const [decl, lowerDecl, lowerDoc, link] of decls) {
        let err = matchCaseSensitive(decl, lowerDecl, patNoSpaces);

        // match all words as substrings of docstring
        if (!(err < 3) && pat.length > 3 && lowerPats.every(l => lowerDoc.indexOf(l) != -1)) {
            err = 3;
        }

        if (err !== undefined) {
            results.push({decl, err, link});
        }
    }
    return results.sort(({err: a}, {err: b}) => a - b).slice(0, maxResults);
}

const declURL = new URL(`${SITE_ROOT}searchable_data.bmp`, window.location);

export const getDecls = (() => {
  let decls;
  return () => {
    if (!decls) decls = new Promise((resolve, reject) => {
        const req = new XMLHttpRequest();
        req.responseType = 'json';
        req.addEventListener('load', () => resolve(loadDecls(req.response)));
        req.addEventListener('error', () => reject());
        req.open('GET', declURL);
        req.send();
      })
    return decls;
  }
})()

export const declSearch = async (q) => getMatches(await getDecls(), q);