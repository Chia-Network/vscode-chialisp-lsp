#!/usr/bin/env node
'use strict';

const {spawn} = require('child_process');
const fs = require('fs');
const net = require('net');
const os = require('os');
const path = require('path');

const PROGRAM = [
    '(mod (X)',
    '  (defun F (X Y)',
    '    (if X',
    '      (F (- X 1) Y)',
    '      (+ X Y)))',
    '  (F X 3)',
    ')',
    '',
].join('\n');

function findExecutable(name) {
    const pathValue = process.env.PATH || '';
    for (const dir of pathValue.split(path.delimiter)) {
        if (!dir) {
            continue;
        }
        const candidate = path.join(dir, name);
        try {
            fs.accessSync(candidate, fs.constants.X_OK);
            return candidate;
        } catch (_e) {
            // Keep searching.
        }
    }
    return undefined;
}

function loadWasm() {
    const candidates = [
        path.resolve(__dirname, '../../wasm/pkg/clvm_tools_lsp.js'),
        path.resolve(__dirname, '../../debug/build/clvm_tools_lsp.js'),
    ];
    for (const candidate of candidates) {
        if (fs.existsSync(candidate)) {
            return require(candidate);
        }
    }
    throw new Error(`Could not find the node wasm package. Tried: ${candidates.join(', ')}`);
}

function assertFileReaderGuard(wasm) {
    let thrown = false;
    try {
        wasm.arm_gdb_build_program('prog.clsp', PROGRAM, '(5)', '[]', {}, 'out.elf');
    } catch (error) {
        thrown = true;
        const text = `${error && error.message ? error.message : error}`;
        if (!text.includes('file_reader must be a function')) {
            throw new Error(`non-function file_reader raised the wrong error: ${text}`);
        }
    }
    if (!thrown) {
        throw new Error('non-function file_reader returned a value instead of a JS error');
    }
}

function waitForReady(child) {
    return new Promise((resolve, reject) => {
        let stdout = '';
        const timeout = setTimeout(() => {
            reject(new Error(`timed out waiting for the stub.\n${stdout}`));
        }, 180000);
        child.stdout.on('data', (chunk) => {
            stdout += chunk.toString('utf8');
            const match = stdout.match(/CHIALISP_GDB_STUB_READY\s+(\d+)/);
            if (match) {
                clearTimeout(timeout);
                resolve(Number.parseInt(match[1], 10));
            }
        });
        child.stderr.on('data', (chunk) => {
            process.stderr.write(chunk);
        });
        child.once('error', (error) => {
            clearTimeout(timeout);
            reject(error);
        });
        child.once('exit', (code) => {
            clearTimeout(timeout);
            reject(new Error(`stub exited before becoming ready (${code})\n${stdout}`));
        });
    });
}

function anchorAddress(elf) {
    const view = new DataView(elf.buffer, elf.byteOffset, elf.byteLength);
    const sectionOffset = view.getUint32(32, true);
    const sectionSize = view.getUint16(46, true);
    const sectionCount = view.getUint16(48, true);
    const nameSection = view.getUint16(50, true);
    const nameOffset = view.getUint32(sectionOffset + nameSection * sectionSize + 16, true);
    const sectionName = (offset) => {
        let end = nameOffset + offset;
        while (elf[end] !== 0) {
            end += 1;
        }
        return elf.subarray(nameOffset + offset, end).toString('utf8');
    };

    for (let index = 0; index < sectionCount; index += 1) {
        const base = sectionOffset + index * sectionSize;
        if (sectionName(view.getUint32(base, true)) !== '.text.F') {
            continue;
        }
        const address = view.getUint32(base + 12, true);
        const offset = view.getUint32(base + 16, true);
        const size = view.getUint32(base + 20, true);
        const code = elf.subarray(offset, offset + size);
        const anchor = Buffer.from([0x00, 0x00, 0x80, 0xe2]);
        for (let cursor = 0; cursor + 4 <= Math.min(code.length, 48); cursor += 4) {
            if (code.subarray(cursor, cursor + 4).equals(anchor)) {
                return address + cursor;
            }
        }
        throw new Error('.text.F does not contain the function-entry NOP anchor');
    }
    throw new Error('ELF has no .text.F section');
}

function runGdb(gdb, commands) {
    return new Promise((resolve, reject) => {
        const child = spawn(gdb, ['-nx', '-batch', ...commands.flatMap((command) => ['-ex', command])], {
            stdio: ['ignore', 'pipe', 'pipe'],
        });
        let stdout = '';
        let stderr = '';
        const timeout = setTimeout(() => {
            child.kill('SIGKILL');
            reject(new Error(`gdb timed out\n${stdout}\n${stderr}`));
        }, 120000);
        child.stdout.on('data', (chunk) => {
            stdout += chunk.toString('utf8');
        });
        child.stderr.on('data', (chunk) => {
            stderr += chunk.toString('utf8');
        });
        child.once('error', (error) => {
            clearTimeout(timeout);
            reject(error);
        });
        child.once('exit', (code) => {
            clearTimeout(timeout);
            resolve({code, stdout, stderr});
        });
    });
}

function portOpen(port) {
    return new Promise((resolve) => {
        const socket = net.connect({host: '127.0.0.1', port}, () => {
            socket.end();
            resolve(true);
        });
        socket.on('error', () => resolve(false));
    });
}

function sleep(ms) {
    return new Promise((resolve) => setTimeout(resolve, ms));
}

async function main() {
    const gdb = findExecutable('gdb-multiarch');
    if (!gdb) {
        throw new Error('gdb-multiarch is not installed; the ARM GDB smoke test was not run');
    }

    const wasm = loadWasm();
    assertFileReaderGuard(wasm);

    const work = fs.mkdtempSync(path.join(os.tmpdir(), 'chialisp-arm-gdb-smoke-'));
    const program = path.join(work, 'prog.clsp');
    const elf = path.join(work, 'prog.arm.elf');
    const synthetic = `${elf}.clsp`;
    fs.writeFileSync(program, PROGRAM);
    const printer = path.resolve(__dirname, '../../wasm/resources/gdb_print_sexp.py');
    const stub = spawn(process.execPath, [
        path.join(__dirname, 'gdb_stub_service.js'),
        '--workspace', work,
        '--program', program,
        '--run-args-json', '["(5)"]',
        '--elf-out', elf,
        '--synthetic-source-out', synthetic,
        '--port', '0',
    ], {stdio: ['ignore', 'pipe', 'pipe']});

    let port;
    const stubExited = new Promise((resolve) => stub.once('exit', () => resolve(true)));
    try {
        port = await waitForReady(stub);
        const anchor = anchorAddress(fs.readFileSync(elf));
        const result = await runGdb(gdb, [
            'set pagination off',
            'set confirm off',
            `file ${elf}`,
            `dir ${work}`,
            `source ${printer}`,
            'info address F',
            `break *${anchor}`,
            `target remote 127.0.0.1:${port}`,
            'continue',
            'print $pc',
            'stepi',
            'print $pc',
            'info args',
            'disconnect',
            'quit',
        ]);
        const transcript = `${result.stdout}\n${result.stderr}`;
        fs.writeFileSync(path.join(work, 'gdb.txt'), transcript);
        if (result.code !== 0) {
            throw new Error(`gdb exited ${result.code}\n${transcript}`);
        }
        const hit = new RegExp(`Breakpoint\\s+1,\\s+0x0*${anchor.toString(16)}\\b`, 'i');
        if (!hit.test(transcript)) {
            throw new Error(`gdb did not stop at the function anchor ${anchor.toString(16)}\n${transcript}`);
        }
        const pcs = [...transcript.matchAll(/\$\d+\s*=\s*.*?(0x[0-9a-fA-F]+)/g)].map((match) => match[1]);
        if (pcs.length < 2 || pcs[0].toLowerCase() === pcs[1].toLowerCase()) {
            throw new Error(`stepi did not change $pc (${pcs.join(' -> ')})\n${transcript}`);
        }
        const lines = transcript.split('\n').map((line) => line.trim());
        const inspected = lines.find((line) => line.startsWith('ENV ='))
            || lines.find((line) => line.includes("b'"));
        if (!inspected) {
            throw new Error(`pretty-printer did not render a CLVM value\n${transcript}`);
        }
        process.stdout.write(`ARM_GDB_SMOKE_BREAKPOINT ${anchor.toString(16)}\n`);
        process.stdout.write(`ARM_GDB_SMOKE_STEP ${pcs[0]} -> ${pcs[1]}\n`);
        process.stdout.write(`ARM_GDB_SMOKE_INSPECT ${inspected.trim()}\n`);
    } finally {
        const natural = await Promise.race([stubExited, sleep(1500).then(() => false)]);
        if (!natural && stub.exitCode === null) {
            stub.kill('SIGTERM');
        }
    }

    const exited = await Promise.race([stubExited, sleep(2000).then(() => false)]);
    if (!exited && stub.exitCode === null) {
        stub.kill('SIGKILL');
        throw new Error('ARM GDB stub did not exit after GDB disconnected');
    }
    if (await portOpen(port)) {
        throw new Error(`stub port ${port} was still accepting connections`);
    }
    process.stdout.write('ARM_GDB_SMOKE_CLEANUP ok\n');
}

main().catch((error) => {
    process.stderr.write(`${error && error.stack ? error.stack : String(error)}\n`);
    process.exit(1);
});
