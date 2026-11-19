'use strict';

// Wasm export names are fixed snake_case identifiers.
/* eslint-disable @typescript-eslint/naming-convention */

const assert = require('node:assert/strict');
const {mock, test} = require('node:test');
const {attachStubSocket} = require('./gdb_stub_service');

function fakeSocket() {
    const handlers = {};
    return {
        destroyed: false,
        handlers,
        destroy() {
            this.destroyed = true;
        },
        on(name, handler) {
            handlers[name] = handler;
        },
        write() {},
    };
}

test('stub create failure closes the listening server', () => {
    mock.timers.enable({apis: ['setTimeout']});
    try {
        const socket = fakeSocket();
        let closed = 0;
        const exits = [];
        attachStubSocket(socket, {
            create_arm_gdb_stub() {
                throw new Error('create failed');
            },
            destroy_arm_gdb_stub() {},
        }, {elf: Buffer.alloc(0), symbolsJson: '{}'}, {
            close() {
                closed += 1;
            },
        }, (code) => exits.push(code));

        assert.equal(socket.destroyed, true);
        assert.equal(closed, 1);
        mock.timers.tick(200);
        assert.deepEqual(exits, [1]);
    } finally {
        mock.timers.reset();
    }
});

test('stub data failure closes the listening server', () => {
    mock.timers.enable({apis: ['setTimeout']});
    try {
        const socket = fakeSocket();
        let closed = 0;
        attachStubSocket(socket, {
            create_arm_gdb_stub() {
                return 7;
            },
            arm_gdb_stub_incoming_data() {
                throw new Error('incoming failed');
            },
            destroy_arm_gdb_stub(id) {
                assert.equal(id, 7);
            },
        }, {elf: Buffer.alloc(0), symbolsJson: '{}'}, {
            close() {
                closed += 1;
            },
        }, () => {});

        socket.handlers.data(Buffer.from([0x03]));
        assert.equal(closed, 1);
    } finally {
        mock.timers.reset();
    }
});
