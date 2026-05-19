#!/usr/bin/env node
'use strict';

const fs = require('fs');
const net = require('net');
const path = require('path');

function parseArgs(argv) {
    const result = {};
    for (let i = 0; i < argv.length; i++) {
        const key = argv[i];
        if (!key.startsWith('--')) {
            throw new Error(`unexpected argument ${key}`);
        }
        const value = argv[i + 1];
        if (value === undefined || value.startsWith('--')) {
            throw new Error(`missing value for ${key}`);
        }
        result[key.slice(2)] = value;
        i += 1;
    }
    return result;
}

function loadWasmModule() {
    const candidates = [
        path.resolve(__dirname, '../../debug/build/clvm_tools_lsp.js'),
        path.resolve(__dirname, '../../wasm/pkg/clvm_tools_lsp.js'),
    ];

    for (const candidate of candidates) {
        if (fs.existsSync(candidate)) {
            return require(candidate);
        }
    }

    throw new Error(
        `Could not find clvm_tools_lsp wasm package. Tried: ${candidates.join(', ')}. ` +
        'Run `make` or `cd wasm && wasm-pack build --release --target nodejs` first.'
    );
}

function getRunArg(runArgsJson) {
    const runArgs = JSON.parse(runArgsJson);
    if (typeof runArgs === 'string') {
        return runArgs;
    }

    if (Array.isArray(runArgs) && typeof runArgs[0] === 'string') {
        return runArgs[0];
    }

    return '()';
}

function requiredArgs(args) {
    for (const required of ['workspace', 'program', 'run-args-json', 'elf-out', 'port']) {
        if (!args[required]) {
            throw new Error(`missing --${required}`);
        }
    }
}

function writeGeneratedElf(wasm, args) {
    const program = fs.readFileSync(args.program, 'utf8');
    const runArg = getRunArg(args['run-args-json']);
    const built = wasm.arm_gdb_build_program(args.program, program, runArg, args['elf-out']);
    const elf = Buffer.from(built.elf);

    fs.mkdirSync(path.dirname(args['elf-out']), { recursive: true });
    fs.writeFileSync(args['elf-out'], elf);

    return {
        elf,
        symbolsJson: built.symbolsJson,
    };
}

function destroyStub(wasm, stubId) {
    if (stubId !== undefined) {
        wasm.destroy_arm_gdb_stub(stubId);
    }
}

function startTcpBridge(wasm, args, built) {
    const port = Number.parseInt(args.port, 10);
    if (!Number.isInteger(port) || port <= 0 || port > 65535) {
        throw new Error(`invalid --port ${args.port}`);
    }

    const server = net.createServer((socket) => {
        let stubId;
        const writeToSocket = (bytes) => {
            if (!socket.destroyed) {
                socket.write(Buffer.from(bytes));
            }
        };

        try {
            stubId = wasm.create_arm_gdb_stub(built.elf, built.symbolsJson, writeToSocket);
        } catch (e) {
            console.error(e && e.stack ? e.stack : String(e));
            socket.destroy();
            return;
        }

        socket.on('data', (chunk) => {
            try {
                wasm.arm_gdb_stub_incoming_data(stubId, chunk);
            } catch (e) {
                console.error(e && e.stack ? e.stack : String(e));
                socket.destroy();
            }
        });

        socket.on('close', () => {
            destroyStub(wasm, stubId);
        });

        socket.on('error', (e) => {
            console.error(e && e.stack ? e.stack : String(e));
            destroyStub(wasm, stubId);
        });
    });

    server.on('error', (e) => {
        console.error(e && e.stack ? e.stack : String(e));
        process.exit(1);
    });

    server.listen(port, '127.0.0.1', () => {
        console.log(`CHIALISP_GDB_STUB_READY ${port}`);
    });

    process.on('SIGTERM', () => server.close(() => process.exit(0)));
    process.on('SIGINT', () => server.close(() => process.exit(0)));
}

function main() {
    const args = parseArgs(process.argv.slice(2));
    requiredArgs(args);

    process.chdir(args.workspace);

    const wasm = loadWasmModule();
    const built = writeGeneratedElf(wasm, args);
    startTcpBridge(wasm, args, built);
}

try {
    main();
} catch (e) {
    console.error(e && e.stack ? e.stack : String(e));
    process.exit(1);
}
