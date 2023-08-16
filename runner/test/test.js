import * as assert from 'assert';
import { StdinReader } from '../src/stdin_reader.js';

const log = {'write': () => {}};

describe('stdin_parser', () => {
    it('should parse fragmented data', () => {
        let received = 0;
        const reader = new StdinReader((packet) => {
            let parsed = JSON.parse(packet);
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
