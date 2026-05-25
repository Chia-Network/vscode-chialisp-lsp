#!/usr/bin/env node
'use strict';

const childProcess = require('child_process');
const fs = require('fs');
const path = require('path');

const WEB_GDB_NODE_REPO = 'https://github.com/prozacchiwawa/web-gdb-node.git';
const WEB_GDB_NODE_BRANCH = '20260525-test-mi-interp';
const READY_PREFIX = 'CHIALISP_GDB_STUB_READY';

function hasExecutableAccess(filePath) {
    try {
        fs.accessSync(filePath, fs.constants.X_OK);
        return true;
    } catch (_e) {
        return false;
    }
}

function findExecutable(name) {
    const pathValue = process.env.PATH || '';
    for (const dir of pathValue.split(path.delimiter)) {
        if (!dir) {
            continue;
        }

        const candidate = path.join(dir, name);
        if (hasExecutableAccess(candidate)) {
            return candidate;
        }

        if (process.platform === 'win32') {
            const exeCandidate = `${candidate}.exe`;
            if (hasExecutableAccess(exeCandidate)) {
                return exeCandidate;
            }
        }
    }

    return undefined;
}

function consumeValue(argv, index, optionName) {
    const value = argv[index + 1];
    if (value === undefined) {
        throw new Error(`missing value for ${optionName}`);
    }

    return value;
}

function parseLaunchArgs(argv) {
    const parsed = {
        childArgs: [],
        interpreterArgs: [],
        metadata: {},
        runnerPath: undefined,
        gdbServer: '127.0.0.1:0',
    };

    for (let i = 0; i < argv.length; i++) {
        const arg = argv[i];

        if (!parsed.runnerPath && !arg.startsWith('--')) {
            parsed.runnerPath = arg;
            continue;
        }

        if (arg === '--gdb-server') {
            parsed.gdbServer = consumeValue(argv, i, arg);
            i += 1;
            continue;
        }

        if (arg.startsWith('--gdb-server=')) {
            parsed.gdbServer = arg.slice('--gdb-server='.length);
            continue;
        }

        if (arg === '--chialisp-workspace') {
            parsed.metadata.workspace = consumeValue(argv, i, arg);
            i += 1;
            continue;
        }

        if (arg.startsWith('--chialisp-workspace=')) {
            parsed.metadata.workspace = arg.slice('--chialisp-workspace='.length);
            continue;
        }

        if (arg === '--chialisp-program') {
            parsed.metadata.program = consumeValue(argv, i, arg);
            i += 1;
            continue;
        }

        if (arg.startsWith('--chialisp-program=')) {
            parsed.metadata.program = arg.slice('--chialisp-program='.length);
            continue;
        }

        if (arg === '--chialisp-run-args-json') {
            parsed.metadata.runArgsJson = consumeValue(argv, i, arg);
            i += 1;
            continue;
        }

        if (arg.startsWith('--chialisp-run-args-json=')) {
            parsed.metadata.runArgsJson = arg.slice('--chialisp-run-args-json='.length);
            continue;
        }

        if (arg === '--chialisp-elf-out') {
            parsed.metadata.elfOut = consumeValue(argv, i, arg);
            i += 1;
            continue;
        }

        if (arg.startsWith('--chialisp-elf-out=')) {
            parsed.metadata.elfOut = arg.slice('--chialisp-elf-out='.length);
            continue;
        }

        if (arg === '--chialisp-synthetic-source-out') {
            parsed.metadata.syntheticSourceOut = consumeValue(argv, i, arg);
            i += 1;
            continue;
        }

        if (arg.startsWith('--chialisp-synthetic-source-out=')) {
            parsed.metadata.syntheticSourceOut = arg.slice('--chialisp-synthetic-source-out='.length);
            continue;
        }

        if (arg === '--chialisp-gdb-printer') {
            parsed.metadata.gdbPrinter = consumeValue(argv, i, arg);
            i += 1;
            continue;
        }

        if (arg.startsWith('--chialisp-gdb-printer=')) {
            parsed.metadata.gdbPrinter = arg.slice('--chialisp-gdb-printer='.length);
            continue;
        }

        if (arg === '--interpreter=mi') {
            parsed.interpreterArgs.push(arg);
            continue;
        }

        if (arg === '--interpreter') {
            const value = consumeValue(argv, i, arg);
            parsed.interpreterArgs.push(arg, value);
            i += 1;
            continue;
        }

        parsed.childArgs.push(arg);
    }

    if (parsed.interpreterArgs.length === 0) {
        parsed.interpreterArgs.push('--interpreter=mi');
    }

    return parsed;
}

function requireMetadata(metadata) {
    const required = ['workspace', 'program', 'runArgsJson', 'elfOut', 'syntheticSourceOut'];
    for (const key of required) {
        if (!metadata[key]) {
            throw new Error(`missing --chialisp-${key.replace(/[A-Z]/g, (c) => `-${c.toLowerCase()}`)}`);
        }
    }
}

function parseGdbServer(value) {
    const separator = value.lastIndexOf(':');
    if (separator <= 0 || separator === value.length - 1) {
        throw new Error(`invalid --gdb-server ${value}`);
    }

    const host = value.slice(0, separator);
    const portText = value.slice(separator + 1);
    const port = Number.parseInt(portText, 10);
    if (!Number.isInteger(port) || port < 0 || port > 65535) {
        throw new Error(`invalid --gdb-server port ${portText}`);
    }

    return { host, port };
}

function runProcess(command, args, cwd) {
    return new Promise((resolve, reject) => {
        const child = childProcess.spawn(command, args, {
            cwd,
            env: process.env,
            stdio: ['ignore', 'pipe', 'pipe'],
        });
        let output = '';

        child.stdout.on('data', (chunk) => {
            output += chunk.toString('utf8');
        });

        child.stderr.on('data', (chunk) => {
            output += chunk.toString('utf8');
        });

        child.once('error', reject);
        child.once('exit', (code) => {
            if (code === 0) {
                resolve();
            } else {
                reject(new Error(`${command} ${args.join(' ')} exited with ${code}: ${output}`));
            }
        });
    });
}

function repoExists(repoPath) {
    return fs.existsSync(path.join(repoPath, '.git'));
}

async function ensureWebGdbRepo(runnerPath) {
    if (!runnerPath) {
        throw new Error('missing web-gdb-node runner path');
    }

    const repoPath = path.resolve(path.dirname(runnerPath), '..');
    const parentDir = path.dirname(repoPath);
    fs.mkdirSync(parentDir, { recursive: true });

    if (!repoExists(repoPath)) {
        await runProcess('git', ['clone', '--recurse-submodules', WEB_GDB_NODE_REPO, repoPath], parentDir);
        await runProcess('git', ['-C', repoPath, 'checkout', WEB_GDB_NODE_BRANCH], parentDir);
        return;
    }

    await runProcess('git', ['-C', repoPath, 'submodule', 'update', '--init', '--recursive'], parentDir);
}

function startStubService(metadata, requestedPort) {
    const servicePath = path.join(__dirname, 'gdb_stub_service.js');
    const args = [
        servicePath,
        '--workspace',
        metadata.workspace,
        '--program',
        metadata.program,
        '--run-args-json',
        metadata.runArgsJson,
        '--elf-out',
        metadata.elfOut,
        '--synthetic-source-out',
        metadata.syntheticSourceOut,
        '--port',
        requestedPort.toString(),
    ];

    const child = childProcess.spawn(process.execPath, args, {
        cwd: metadata.workspace,
        env: {
            ...process.env,
            ELECTRON_RUN_AS_NODE: '1',
            ATOM_SHELL_INTERNAL_RUN_AS_NODE: '1',
        },
        stdio: ['ignore', 'pipe', 'pipe'],
    });

    return new Promise((resolve, reject) => {
        let settled = false;
        let stdout = '';
        const timeout = setTimeout(() => {
            if (!settled) {
                settled = true;
                child.kill();
                reject(new Error('timed out waiting for the ARM GDB stub to become ready'));
            }
        }, 10 * 60 * 1000);

        const fail = (error) => {
            if (!settled) {
                settled = true;
                clearTimeout(timeout);
                reject(error);
            }
        };

        child.stdout.on('data', (chunk) => {
            stdout += chunk.toString('utf8');
            const match = stdout.match(new RegExp(`${READY_PREFIX}\\\\s+(\\\\d+)`));
            if (match && !settled) {
                settled = true;
                clearTimeout(timeout);
                resolve({ child, port: Number.parseInt(match[1], 10) });
            }
        });

        child.stderr.on('data', (chunk) => {
            process.stderr.write(chunk);
        });

        child.once('error', fail);
        child.once('exit', (code) => {
            if (!settled) {
                fail(new Error(`ARM GDB stub exited with ${code}`));
            }
        });
    });
}

function rewriteExCommand(command, metadata, endpoint) {
    if (command === 'target remote /dev/ttyS1') {
        return `target remote ${endpoint}`;
    }

    if (metadata.elfOut && command === `file /mnt/${path.basename(metadata.elfOut)}`) {
        return `file ${metadata.elfOut}`;
    }

    if (metadata.gdbPrinter && command === 'source /mnt/gdb_print_sexp.py') {
        return `source ${metadata.gdbPrinter}`;
    }

    if (metadata.workspace && command === 'dir /mnt') {
        return `dir ${metadata.workspace}`;
    }

    return command;
}

function buildLocalGdbArgs(parsed, endpoint) {
    const args = [];

    for (let i = 0; i < parsed.childArgs.length; i++) {
        const arg = parsed.childArgs[i];

        if (arg === '--workspace' || arg === '--import-file') {
            i += 1;
            continue;
        }

        if (arg.startsWith('--workspace=') || arg.startsWith('--import-file=')) {
            continue;
        }

        if (arg === '--ex') {
            const command = parsed.childArgs[i + 1];
            if (command !== undefined) {
                args.push(arg, rewriteExCommand(command, parsed.metadata, endpoint));
                i += 1;
            } else {
                args.push(arg);
            }
            continue;
        }

        if (arg.startsWith('--ex=')) {
            args.push('--ex', rewriteExCommand(arg.slice('--ex='.length), parsed.metadata, endpoint));
            continue;
        }

        args.push(arg);
    }

    args.push(...parsed.interpreterArgs);
    return args;
}

function buildWebGdbArgs(parsed, endpoint) {
    if (!parsed.runnerPath) {
        throw new Error('missing web-gdb-node runner path');
    }

    return [
        parsed.runnerPath,
        '--gdb-server',
        endpoint,
        ...parsed.childArgs,
        ...parsed.interpreterArgs,
    ];
}

function spawnDebugger(command, args, cwd) {
    return childProcess.spawn(command, args, {
        cwd,
        env: {
            ...process.env,
            ELECTRON_RUN_AS_NODE: '1',
            ATOM_SHELL_INTERNAL_RUN_AS_NODE: '1',
        },
        stdio: 'inherit',
    });
}

function waitForDebugger(child, stubChild) {
    return new Promise((resolve) => {
        child.once('exit', (code, signal) => {
            if (stubChild.exitCode === null && !stubChild.killed) {
                stubChild.kill();
            }

            if (signal) {
                resolve(1);
            } else {
                resolve(code === null ? 1 : code);
            }
        });

        child.once('error', (error) => {
            process.stderr.write(`${error && error.stack ? error.stack : String(error)}\n`);
            if (stubChild.exitCode === null && !stubChild.killed) {
                stubChild.kill();
            }
            resolve(1);
        });

        for (const signal of ['SIGINT', 'SIGTERM']) {
            process.once(signal, () => {
                if (child.exitCode === null && !child.killed) {
                    child.kill(signal);
                }
                if (stubChild.exitCode === null && !stubChild.killed) {
                    stubChild.kill(signal);
                }
            });
        }
    });
}

async function main() {
    const parsed = parseLaunchArgs(process.argv.slice(2));
    requireMetadata(parsed.metadata);

    const requestedServer = parseGdbServer(parsed.gdbServer);
    const localGdb = findExecutable('gdb-multiarch');
    if (!localGdb) {
        await ensureWebGdbRepo(parsed.runnerPath);
    }

    const stub = await startStubService(parsed.metadata, requestedServer.port);
    const endpoint = `${requestedServer.host}:${stub.port}`;
    const command = localGdb || process.execPath;
    const args = localGdb ? buildLocalGdbArgs(parsed, endpoint) : buildWebGdbArgs(parsed, endpoint);
    const debuggerProcess = spawnDebugger(command, args, parsed.metadata.workspace);
    process.exitCode = await waitForDebugger(debuggerProcess, stub.child);
}

main().catch((error) => {
    process.stderr.write(`${error && error.stack ? error.stack : String(error)}\n`);
    process.exit(1);
});
