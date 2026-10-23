import * as assert from 'node:assert/strict';
import {spawn} from 'node:child_process';
import {fileURLToPath} from 'node:url';
import {StdinReader} from '../src/stdin_reader.js';

function run(messages) {
    return new Promise((resolve, reject) => {
        const child = spawn(process.execPath, [fileURLToPath(new URL('../src/index.js', import.meta.url))]);
        const replies = [];
        let stderr = '';
        const reader = new StdinReader(body => replies.push(JSON.parse(body)));
        child.stdout.on('data', chunk => reader.processChunk({write: reject}, chunk));
        child.stderr.on('data', chunk => { stderr += chunk; });
        child.on('error', reject);
        child.on('exit', code => resolve({code, replies, stderr}));
        for (const message of messages) {
            const body = typeof message === 'string' ? message : JSON.stringify({jsonrpc: '2.0', ...message});
            const packet = Buffer.from(`Content-Length: ${Buffer.byteLength(body)}\r\n\r\n${body}`);
            // Include boundaries inside UTF-8 code points, not just JSON tokens.
            for (let i = 0; i < packet.length; i += 7) {
                child.stdin.write(packet.subarray(i, i + 7));
            }
        }
        child.stdin.end();
    });
}

describe('LSP stdio protocol', () => {
    it('frames Unicode JSON-RPC and completes the request lifecycle', async () => {
        const uri = 'file:///audit/unicode.clsp';
        const {code, replies, stderr} = await run([
            '{invalid',
            {id: 1, method: 'initialize', params: {processId: null, rootUri: 'file:///audit', capabilities: {}}},
            {method: 'textDocument/didOpen', params: {textDocument: {uri, languageId: 'chialisp', version: 1, text: '(mod (é😀) (list "é😀" é😀))'}}},
            {id: 2, method: 'textDocument/definition', params: {textDocument: {uri}, position: {line: 0, character: 23}}},
            {id: 3, method: 'unknown/test', params: {}},
            {id: 4, method: 'textDocument/completion', params: {}},
            {id: 5, method: 'shutdown', params: null},
            {method: 'exit'},
        ]);
        assert.equal(code, 0, stderr);
        assert.ok(replies.every(reply => reply.jsonrpc === '2.0'));
        assert.equal(replies.find(reply => reply.id === null).error.code, -32700);
        assert.deepEqual(replies.find(reply => reply.id === 2).result.range, {
            start: {line: 0, character: 6}, end: {line: 0, character: 9},
        });
        assert.equal(replies.find(reply => reply.id === 3).error.code, -32601);
        assert.equal(replies.find(reply => reply.id === 4).error.code, -32602);
        assert.equal(replies.find(reply => reply.id === 5).result, null);
    });
    it('exits unsuccessfully if shutdown was omitted', async () => {
        const result = await run([{method: 'exit'}]);
        assert.equal(result.code, 1);
    });
});
