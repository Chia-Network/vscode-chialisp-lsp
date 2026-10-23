import { test } from 'node:test';
import * as assert from 'node:assert/strict';
import * as fs from 'node:fs';
import * as path from 'node:path';
import { INITIAL, parseRawGrammar, Registry } from 'vscode-textmate';
import { loadWASM, OnigScanner, OnigString } from 'vscode-oniguruma';

const grammarPath = path.resolve(__dirname, '../../../syntaxes/chialisp.tmGrammar.json');
const grammar = (async () => {
    await loadWASM(fs.readFileSync(require.resolve('vscode-oniguruma/release/onig.wasm')).buffer as ArrayBuffer);
    const registry = new Registry({
        onigLib: Promise.resolve({
            createOnigScanner: patterns => new OnigScanner(patterns),
            createOnigString: value => new OnigString(value)
        }),
        loadGrammar: async () => parseRawGrammar(fs.readFileSync(grammarPath, 'utf8'), grammarPath)
    });
    return (await registry.loadGrammar('source.chialisp'))!;
})();

test('comment parentheses keep comment scope inside and outside forms', async () => {
    const g = await grammar;
    for (const line of ['; (unmatched )) comment', '(mod () ; (comment ))', '  ;; (( another comment']) {
        const tokens = g.tokenizeLine(line, INITIAL).tokens;
        const start = line.indexOf(';');
        assert.ok(tokens.filter(token => token.endIndex > start).every(token =>
            token.scopes.some(scope => scope.startsWith('comment'))));
        // VS Code uses this encoded token type to exclude comments from bracket coloring.
        const encoded = g.tokenizeLine2(line, INITIAL).tokens;
        for (let i = 0; i < encoded.length; i += 2) {
            if (encoded[i] >= start) {
                assert.equal((encoded[i + 1] >>> 8) & 3, 1);
            }
        }
    }
});

test('strings protect semicolons and parentheses from comment and bracket scopes', async () => {
    const g = await grammar;
    for (const quote of ['"', "'"]) {
        const line = `(mod () (list ${quote}a; (b)${quote} 1)) ; real (comment)`;
        const tokens = g.tokenizeLine(line, INITIAL).tokens;
        const start = line.indexOf(quote);
        const end = line.lastIndexOf(quote) + 1;
        const stringTokens = tokens.filter(token => token.startIndex < end && token.endIndex > start);
        assert.ok(stringTokens.every(token => token.scopes.some(scope => scope.startsWith('string'))));
        const number = tokens.find(token => token.startIndex <= line.indexOf('1') && token.endIndex > line.indexOf('1'))!;
        assert.ok(number.scopes.every(scope => !/^(comment|string)/.test(scope)));
    }
});


test('escaped quotes and multiline strings retain their lexical state', async () => {
    const g = await grammar;
    const first = g.tokenizeLine('(mod () "escaped \\" ; (still string)', INITIAL);
    assert.ok(first.tokens[first.tokens.length - 1].scopes.some(scope => scope.startsWith('string')));
    const second = g.tokenizeLine('continued )" 1) ; real comment (', first.ruleStack);
    assert.ok(second.tokens[0].scopes.some(scope => scope.startsWith('string')));
    assert.ok(second.tokens[second.tokens.length - 1].scopes.some(scope => scope.startsWith('comment')));
});
