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

function loadDecls(searchableDataCnt) {
    return searchableDataCnt.map(({name, description}) => [name, name.toLowerCase(), description.toLowerCase()])
}

function getMatches(decls, pat, maxResults = 30) {
    const lowerPats = pat.toLowerCase().split(/\s/g);
    const patNoSpaces = pat.replace(/\s/g, '');
    const results = [];
    for (const [decl, lowerDecl, lowerDoc] of decls) {
        let err = matchCaseSensitive(decl, lowerDecl, patNoSpaces);

        // match all words as substrings of docstring
        if (!(err < 3) && pat.length > 3 && lowerPats.every(l => lowerDoc.indexOf(l) != -1)) {
            err = 3;
        }

        if (err !== undefined) {
            results.push({decl, err});
        }
    }
    return results.sort(({err: a}, {err: b}) => a - b).slice(0, maxResults);
}

if (typeof process === 'object') { // NodeJS
    const data = loadDecls(JSON.parse(require('fs').readFileSync('searchable_data.bmp').toString()));
    console.log(getMatches(data, process.argv[2] || 'ltltle'));
}