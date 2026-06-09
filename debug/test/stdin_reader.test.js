const assert = require('assert');
const test = require('node:test');
const {StdinReader} = require('../src/stdin_reader');

const log = {write: () => {}};

function packet(payload) {
    return `Content-Length: ${Buffer.byteLength(payload, 'utf8')}\r\n\r\n${payload}`;
}

test('parses fragmented messages', () => {
    let received = 0;
    const reader = new StdinReader((message) => {
        const parsed = JSON.parse(message);
        assert.equal(parsed.test, received);
        received += 1;
    });

    reader.processChunk(log, packet('{"test":0}'));
    reader.processChunk(log, 'Content-Length: 10\r\n\r\n{"');
    reader.processChunk(log, 'test":1}Content-Length: 10\r\n\r\n{');
    reader.processChunk(log, '"test":2}');

    assert.equal(received, 3);
});

test('combines accumulated payload before delivery', () => {
    const messages = [];
    const reader = new StdinReader((message) => {
        messages.push(message);
    });

    const message = '{"command":"stepIn","arguments":{"threadId":1}}';
    const stream = packet(message);
    const payloadStart = stream.indexOf('\r\n\r\n') + 4;

    reader.processChunk(log, stream.slice(0, payloadStart + 7));
    reader.processChunk(log, stream.slice(payloadStart + 7));

    assert.deepEqual(messages, [message]);
});
