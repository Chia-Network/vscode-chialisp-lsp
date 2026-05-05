const fs = require('fs');
const path = require('path');
const net = require('net');
const process = require('process');

const clvm_tools_rs = require('../build/clvm_tools_lsp');

function fail(message) {
    process.stderr.write(`${message}\n`);
    process.exit(1);
}

function parseAddress(address) {
    const splitIdx = address.lastIndexOf(':');
    if (splitIdx < 0) {
        return { host: '127.0.0.1', port: Number(address) };
    }

    return {
        host: address.substring(0, splitIdx) || '127.0.0.1',
        port: Number(address.substring(splitIdx + 1)),
    };
}

function parseArgs(argv) {
    const result = {
        workspace: process.cwd(),
        source: '',
        env: '()',
        gdbServer: '127.0.0.1:9001',
        output: '',
        syntheticOutput: '',
        include: [],
    };

    for (let i = 0; i < argv.length; i++) {
        const arg = argv[i];
        if (arg === '--workspace') {
            result.workspace = argv[++i];
        } else if (arg === '--source') {
            result.source = argv[++i];
        } else if (arg === '--env') {
            result.env = argv[++i];
        } else if (arg === '--gdb-server') {
            result.gdbServer = argv[++i];
        } else if (arg === '--output') {
            result.output = argv[++i];
        } else if (arg === '--synthetic-output') {
            result.syntheticOutput = argv[++i];
        } else if (arg === '--include') {
            result.include.push(argv[++i]);
        } else {
            fail(`Unknown argument: ${arg}`);
        }
    }

    if (!result.source) {
        fail('Missing required --source argument');
    }

    return result;
}

const options = parseArgs(process.argv.slice(2));
const workspaceFolder = path.resolve(options.workspace);
const sourcePath = path.resolve(workspaceFolder, options.source);
const includePaths = options.include.map((inc) => path.resolve(workspaceFolder, inc));

let sourceContent = '';
try {
    sourceContent = fs.readFileSync(sourcePath, 'utf8');
} catch (e) {
    fail(`Unable to read source file ${sourcePath}: ${e}`);
}

const relativeSource = path.relative(workspaceFolder, sourcePath);
const compileResult = clvm_tools_rs.compile_to_arm_elf(
    sourceContent,
    relativeSource,
    includePaths,
    options.env
);

if (compileResult && compileResult.error) {
    fail(`compile_to_arm_elf failed: ${compileResult.error}`);
}

const objectFile = compileResult.object_file;
if (!(objectFile instanceof Uint8Array)) {
    fail('compile_to_arm_elf did not return object_file Uint8Array');
}

const outputPath = options.output
    ? path.resolve(workspaceFolder, options.output)
    : path.resolve(workspaceFolder, '.chialisp-gdb', `${path.parse(sourcePath).name}.elf`);
const syntheticOutputPath = options.syntheticOutput
    ? path.resolve(workspaceFolder, options.syntheticOutput)
    : `${outputPath}.synthetic.clsp`;

fs.mkdirSync(path.dirname(outputPath), { recursive: true });
fs.writeFileSync(outputPath, Buffer.from(objectFile));
if (typeof compileResult.synthetic_source === 'string') {
    fs.writeFileSync(syntheticOutputPath, compileResult.synthetic_source);
}

const { host, port } = parseAddress(options.gdbServer);
if (!Number.isFinite(port) || port <= 0) {
    fail(`Invalid gdb server port in address ${options.gdbServer}`);
}

let liveSocket = null;
let liveStubId = undefined;

function destroyLiveStub() {
    if (liveStubId !== undefined) {
        try {
            clvm_tools_rs.destroy_gdb_stub(liveStubId);
        } catch (_e) {
            // Nothing to do.
        }
        liveStubId = undefined;
    }
}

const server = net.createServer((socket) => {
    if (liveSocket && !liveSocket.destroyed) {
        liveSocket.destroy();
    }
    destroyLiveStub();

    liveSocket = socket;
    const stubCreated = clvm_tools_rs.create_gdb_stub(objectFile, compileResult.symbols, (data) => {
        if (!socket.destroyed) {
            socket.write(Buffer.from(data));
        }
    });

    if (stubCreated && stubCreated.error) {
        socket.destroy();
        fail(`create_gdb_stub failed: ${stubCreated.error}`);
    }

    liveStubId = stubCreated;

    socket.on('data', (data) => {
        if (liveStubId === undefined) {
            return;
        }

        const response = clvm_tools_rs.gdb_stub_incoming_data(liveStubId, new Uint8Array(data));
        if (response && response.error) {
            process.stderr.write(`gdb_stub_incoming_data error: ${response.error}\n`);
        }
    });

    socket.on('close', () => {
        destroyLiveStub();
    });
});

server.on('error', (e) => {
    fail(`gdb stub server error: ${e}`);
});

server.listen(port, host, () => {
    process.stdout.write(`READY ${host}:${port}\n`);
    process.stdout.write(`ELF ${outputPath}\n`);
    process.stdout.write(`SYNTHETIC ${syntheticOutputPath}\n`);
});

function shutdown(exitCode) {
    destroyLiveStub();
    if (liveSocket && !liveSocket.destroyed) {
        liveSocket.destroy();
    }
    server.close(() => process.exit(exitCode));
}

process.on('SIGINT', () => shutdown(0));
process.on('SIGTERM', () => shutdown(0));
