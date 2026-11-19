'use strict';

const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const test = require('node:test');

const launcher = require('./gdb_debug_launcher');

test('web-gdb-node fallback is pinned to a commit', () => {
    assert.match(launcher.webGdbNodeRev, /^[0-9a-f]{40}$/);
    const source = fs.readFileSync(path.join(__dirname, 'gdb_debug_launcher.js'), 'utf8');
    assert.equal(source.includes('20260525-test-mi-interp'), false);
    assert.match(source, /checkout', '--detach', WEB_GDB_NODE_REV/);
});

test('stub service does not write fixed debug files', () => {
    const source = fs.readFileSync(path.join(__dirname, 'gdb_stub_service.js'), 'utf8');
    assert.equal(source.includes('/tmp/gen.log'), false);
    assert.equal(source.includes('/tmp/gdb.txt'), false);
    assert.equal(source.includes('appendFileSync'), false);
});

test('local gdb rewrites guest paths and drops web-gdb import arguments', () => {
    const parsed = launcher.parseLaunchArgs([
        '/work/.chialisp-debug/web-gdb-node/src/runner.js',
        '--gdb-server',
        '127.0.0.1:0',
        '--chialisp-workspace',
        '/work',
        '--chialisp-program',
        '/work/prog.clsp',
        '--chialisp-run-args-json',
        '["(5)"]',
        '--chialisp-elf-out',
        '/work/prog.arm.elf',
        '--chialisp-synthetic-source-out',
        '/work/prog.arm.elf.clsp',
        '--chialisp-gdb-printer',
        '/ext/wasm/resources/gdb_print_sexp.py',
        '--workspace',
        '/work',
        '--import-file',
        '/work/prog.arm.elf',
        '--ex',
        'file /mnt/prog.arm.elf',
        '--ex',
        'source /mnt/gdb_print_sexp.py',
        '--ex',
        'dir /mnt',
        '--ex',
        'target remote /dev/ttyS1',
    ]);

    assert.equal(parsed.metadata.workspace, '/work');
    assert.equal(parsed.metadata.program, '/work/prog.clsp');
    assert.equal(parsed.metadata.runArgsJson, '["(5)"]');
    assert.deepEqual(parsed.interpreterArgs, ['--interpreter=mi']);

    const args = launcher.buildLocalGdbArgs(parsed, '127.0.0.1:4321');
    assert.equal(args.includes('--workspace'), false);
    assert.equal(args.includes('--import-file'), false);
    assert.equal(args.includes('/work/.chialisp-debug/web-gdb-node/src/runner.js'), false);
    assert.deepEqual(
        args.filter((arg, index) => args[index - 1] === '--ex'),
        [
            'file /work/prog.arm.elf',
            'source /ext/wasm/resources/gdb_print_sexp.py',
            'dir /work',
            'target remote 127.0.0.1:4321',
        ]
    );
    assert.equal(args.at(-1), '--interpreter=mi');
});
