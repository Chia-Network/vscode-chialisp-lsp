import {strict as assert} from 'node:assert';
import {test} from 'node:test';
import {buildMiDebuggerArgs, parseLaunchJson, quoteArg} from '../../armGdbLaunch';

function splitCommandLine(value: string): string[] {
    const args: string[] = [];
    let current = '';
    let quoted = false;
    let slashes = 0;
    for (const char of value) {
        if (quoted) {
            if (char === '\\') {
                slashes += 1;
                continue;
            }
            if (char === '"') {
                current += '\\'.repeat(Math.floor(slashes / 2));
                if (slashes % 2 === 1) {
                    current += '"';
                } else {
                    quoted = false;
                }
                slashes = 0;
                continue;
            }
            current += '\\'.repeat(slashes);
            slashes = 0;
            current += char;
            continue;
        }
        if (char === '"') {
            quoted = true;
            continue;
        }
        if (/\s/.test(char)) {
            if (current.length > 0) {
                args.push(current);
                current = '';
            }
            continue;
        }
        current += char;
    }
    if (current.length > 0) {
        args.push(current);
    }
    return args;
}

test('quoted workspace templates keep paths with spaces as one argument', () => {
    const workspace = '/home/person/My Programs';
    const expanded = buildMiDebuggerArgs({
        runnerPath: '${workspaceFolder}/.chialisp-debug/web-gdb-node/src/runner.js',
        workspacePath: '${workspaceFolder}',
        programPath: '${workspaceFolder}/puzzle.clsp',
        runArgsJson: '["(5)"]',
        elfPath: '${workspaceFolder}/.chialisp-debug/build/puzzle.arm.elf',
        syntheticSourcePath: '${workspaceFolder}/.chialisp-debug/build/puzzle.arm.elf.clsp',
        gdbPrinterPath: '/extensions/Chia Network/gdb_print_sexp.py',
        guestElfPath: '/mnt/puzzle.arm.elf',
        gdbPrinterGuestPath: '/mnt/gdb_print_sexp.py',
    }).split('${workspaceFolder}').join(workspace);
    const argv = splitCommandLine(expanded);

    assert.equal(quoteArg('${workspaceFolder}/puzzle.clsp').startsWith('"'), true);
    assert.ok(argv.includes(`${workspace}/puzzle.clsp`));
    assert.ok(argv.includes(`${workspace}/.chialisp-debug/build/puzzle.arm.elf`));
    assert.ok(argv.includes(`${workspace}/.chialisp-debug/build/puzzle.arm.elf.clsp`));
    assert.ok(argv.includes('/extensions/Chia Network/gdb_print_sexp.py'));
    assert.equal(argv.includes('/home/person/My'), false);
    assert.equal(argv.includes('Programs/puzzle.clsp'), false);
});

test('launch.json accepts trailing commas and inline comments', () => {
    const parsed = parseLaunchJson(`{
        // existing debugger
        "version": "0.2.0", // keep this
        "configurations": [
            /* retained */ {
                "name": "keep // inside",
                "program": "a,}",
            },
        ],
    }`);

    assert.equal(parsed.version, '0.2.0');
    assert.equal(parsed.configurations.length, 1);
    assert.equal(parsed.configurations[0].name, 'keep // inside');
    assert.equal(parsed.configurations[0].program, 'a,}');
});
