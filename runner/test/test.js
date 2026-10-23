import * as assert from 'assert';
import { StdinReader } from '../src/stdin_reader.js';

const log = {'write': () => {}};

describe('stdin_parser', () => {
    it('decodes UTF-8 after collecting a fragmented byte payload', () => {
        const payload = JSON.stringify({text: 'é 😀 日本語'});
        const frame = Buffer.from(`Content-Length: ${Buffer.byteLength(payload)}\r\n\r\n${payload}`);
        const received = [];
        const reader = new StdinReader(packet => received.push(JSON.parse(packet)));
        for (const byte of frame) {
            reader.processChunk(log, Buffer.from([byte]));
        }
        assert.deepEqual(received, [{text: 'é 😀 日本語'}]);
    });
    it('should parse fragmented data', () => {
        let received = 0;
        const reader = new StdinReader((packet) => {
            const parsed = JSON.parse(packet);
            assert.equal(parsed.test, received);
            received += 1;
        });
        reader.processChunk(log, "Content-Length: 10\r\n\r\n{\"test\":0}");
        assert.equal(received, 1);
        reader.processChunk(log, "Content-Length: 10\r\n\r\n{\"");
        reader.processChunk(log, "test\":1}Content-Length: 10\r\n\r\n{");
        assert.equal(received, 2);
        reader.processChunk(log, "\"test\":2}");
        assert.equal(received, 3);
    });
});
