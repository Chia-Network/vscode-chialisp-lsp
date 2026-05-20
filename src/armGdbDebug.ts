import * as vscode from 'vscode';
import * as fs from 'fs';
import * as net from 'net';
import * as path from 'path';
import { spawn, ChildProcessWithoutNullStreams } from 'child_process';

import { extensionName, publisher } from './constants';
import { log } from './logger';

const WEB_GDB_NODE_REPO = 'https://github.com/prozacchiwawa/web-gdb-node.git';
const DEBUG_WORK_DIR = '.chialisp-debug';

interface ChialispJson {
    run_args?: string[] | string; // eslint-disable-line @typescript-eslint/naming-convention
    [key: string]: unknown;
}

interface ArmGdbContext {
    folder: vscode.WorkspaceFolder;
    programPath: string;
    workspaceRoot: string;
    workDir: string;
    webGdbDir: string;
    buildDir: string;
    elfPath: string;
    syntheticSourcePath: string;
    nodeWrapperPath: string;
    port: number;
    runArgs: string[];
}

let runningStub: ChildProcessWithoutNullStreams | undefined;
let outputChannel: vscode.OutputChannel | undefined;

function getOutputChannel(): vscode.OutputChannel {
    if (!outputChannel) {
        outputChannel = vscode.window.createOutputChannel('Chialisp ARM GDB');
    }

    return outputChannel;
}

function quoteArg(value: string): string {
    return `"${value.replace(/(["\\$`])/g, '\\$1')}"`;
}

function quoteSh(value: string): string {
    return `'${value.replace(/'/g, `'\\''`)}'`;
}

function toWorkspaceSlashPath(workspaceRoot: string, filePath: string): string {
    return path.relative(workspaceRoot, filePath).split(path.sep).join('/');
}

function executableExists(filePath: string): boolean {
    try {
        fs.accessSync(filePath, fs.constants.X_OK);
        return true;
    } catch (_e) {
        return false;
    }
}

function repoExists(repoPath: string): boolean {
    return fs.existsSync(path.join(repoPath, '.git'));
}

function normalizeRunArgs(value: unknown): string[] | undefined {
    if (typeof value === 'string') {
        return [value];
    }

    if (Array.isArray(value) && value.every((item) => typeof item === 'string')) {
        return value as string[];
    }

    return undefined;
}

function parseJsonWithLineComments(content: string): any {
    return JSON.parse(content.replace(/^\s*\/\/.*$/gm, ''));
}

function runProcess(command: string, args: string[], cwd: string): Promise<void> {
    return new Promise((resolve, reject) => {
        const child = spawn(command, args, {
            cwd,
            env: process.env,
        });
        let output = '';

        child.stdout.on('data', (chunk: Buffer) => {
            const text = chunk.toString('utf8');
            output += text;
            getOutputChannel().append(text);
        });

        child.stderr.on('data', (chunk: Buffer) => {
            const text = chunk.toString('utf8');
            output += text;
            getOutputChannel().append(text);
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

async function ensureRepo(repoPath: string, repoUrl: string, parentDir: string): Promise<void> {
    if (!repoExists(repoPath)) {
        await runProcess('git', ['clone', '--recurse-submodules', repoUrl, repoPath], parentDir);
        return;
    }

    await runProcess('git', ['-C', repoPath, 'submodule', 'update', '--init', '--recursive'], parentDir);
}

async function writeNodeWrapper(workDir: string): Promise<string> {
    const wrapperPath = path.join(workDir, process.platform === 'win32' ? 'vscode-node.cmd' : 'vscode-node');
    const executable = process.execPath;

    if (process.platform === 'win32') {
        const content = [
            '@echo off',
            'set ELECTRON_RUN_AS_NODE=1',
            'set ATOM_SHELL_INTERNAL_RUN_AS_NODE=1',
            `"${executable}" %*`,
            '',
        ].join('\r\n');
        await fs.promises.writeFile(wrapperPath, content, 'utf8');
    } else {
        const content = [
            '#!/bin/sh',
            'export ELECTRON_RUN_AS_NODE=1',
            'export ATOM_SHELL_INTERNAL_RUN_AS_NODE=1',
            `exec ${quoteSh(executable)} "$@"`,
            '',
        ].join('\n');
        await fs.promises.writeFile(wrapperPath, content, { encoding: 'utf8', mode: 0o755 });
        await fs.promises.chmod(wrapperPath, 0o755);
    }

    return wrapperPath;
}

async function readChialispJson(jsonPath: string): Promise<ChialispJson> {
    try {
        const content = await fs.promises.readFile(jsonPath, 'utf8');
        return JSON.parse(content) as ChialispJson;
    } catch (e: any) {
        if (e && e.code === 'ENOENT') {
            return {};
        }

        throw e;
    }
}

async function getRunArgs(workspaceRoot: string): Promise<string[] | undefined> {
    const jsonPath = path.join(workspaceRoot, 'chialisp.json');
    const chialispJson = await readChialispJson(jsonPath);
    const existing = normalizeRunArgs(chialispJson.run_args);
    if (existing && existing.length > 0) {
        return existing;
    }

    const input = await vscode.window.showInputBox({
        prompt: 'CLVM data to use as arguments for this ARM GDB debug run',
        placeHolder: '(99 103)',
        value: '()',
        ignoreFocusOut: true,
    });

    if (input === undefined) {
        return undefined;
    }

    chialispJson.run_args = [input];
    await fs.promises.writeFile(jsonPath, JSON.stringify(chialispJson, null, 4) + '\n', 'utf8');
    return [input];
}

function reservePort(): Promise<number> {
    return new Promise((resolve, reject) => {
        const server = net.createServer();
        server.once('error', reject);
        server.listen(0, '127.0.0.1', () => {
            const address = server.address();
            if (!address || typeof address === 'string') {
                server.close(() => reject(new Error('could not reserve local TCP port')));
                return;
            }

            const port = address.port;
            server.close(() => resolve(port));
        });
    });
}

async function prepareContext(folder: vscode.WorkspaceFolder, programPath: string): Promise<ArmGdbContext | undefined> {
    const workspaceRoot = folder.uri.fsPath;
    const runArgs = await getRunArgs(workspaceRoot);
    if (!runArgs) {
        return undefined;
    }

    const workDir = path.join(workspaceRoot, DEBUG_WORK_DIR);
    const webGdbDir = path.join(workDir, 'web-gdb-node');
    const buildDir = path.join(workDir, 'build');
    const safeBaseName = path.basename(programPath).replace(/[^a-zA-Z0-9_.-]/g, '_');
    const elfPath = path.join(buildDir, `${safeBaseName}.arm.elf`);
    const syntheticSourcePath = `${elfPath}.clsp`;
    const port = await reservePort();

    await fs.promises.mkdir(buildDir, { recursive: true });
    const nodeWrapperPath = await writeNodeWrapper(workDir);
    await ensureRepo(webGdbDir, WEB_GDB_NODE_REPO, workDir);

    return {
        folder,
        programPath,
        workspaceRoot,
        workDir,
        webGdbDir,
        buildDir,
        elfPath,
        syntheticSourcePath,
        nodeWrapperPath,
        port,
        runArgs,
    };
}

function startStubService(context: vscode.ExtensionContext, armContext: ArmGdbContext): Promise<void> {
    if (runningStub) {
        runningStub.kill();
        runningStub = undefined;
    }

    const servicePath = path.join(context.extensionPath, 'scripts', 'arm-gdb', 'gdb_stub_service.js');
    const args = [
        servicePath,
        '--workspace',
        armContext.workspaceRoot,
        '--program',
        armContext.programPath,
        '--run-args-json',
        JSON.stringify(armContext.runArgs),
        '--elf-out',
        armContext.elfPath,
        '--synthetic-source-out',
        armContext.syntheticSourcePath,
        '--port',
        armContext.port.toString(),
    ];

    return new Promise((resolve, reject) => {
        const child = spawn(process.execPath, args, {
            cwd: armContext.workspaceRoot,
            env: {
                ...process.env,
                // Electron uses these exact environment variable names to run as Node.
                // eslint-disable-next-line @typescript-eslint/naming-convention
                ELECTRON_RUN_AS_NODE: '1',
                // eslint-disable-next-line @typescript-eslint/naming-convention
                ATOM_SHELL_INTERNAL_RUN_AS_NODE: '1',
            },
        });
        runningStub = child;

        let settled = false;
        const timeout = setTimeout(() => {
            if (!settled) {
                settled = true;
                reject(new Error('timed out waiting for the ARM GDB stub to become ready'));
            }
        }, 10 * 60 * 1000);

        const finishReady = () => {
            if (!settled) {
                settled = true;
                clearTimeout(timeout);
                resolve();
            }
        };

        const fail = (message: string) => {
            if (!settled) {
                settled = true;
                clearTimeout(timeout);
                reject(new Error(message));
            }
        };

        child.stdout.on('data', (chunk: Buffer) => {
            const text = chunk.toString('utf8');
            getOutputChannel().append(text);
            if (text.includes('CHIALISP_GDB_STUB_READY')) {
                finishReady();
            }
        });

        child.stderr.on('data', (chunk: Buffer) => {
            getOutputChannel().append(chunk.toString('utf8'));
        });

        child.once('error', (err) => fail(err.message));
        child.once('exit', (code) => {
            runningStub = undefined;
            if (code !== 0) {
                fail(`ARM GDB stub exited with ${code}`);
            }
        });
    });
}

async function writeLaunchJson(armContext: ArmGdbContext): Promise<vscode.DebugConfiguration> {
    const vscodeDir = path.join(armContext.workspaceRoot, '.vscode');
    const launchPath = path.join(vscodeDir, 'launch.json');
    const elfRel = toWorkspaceSlashPath(armContext.workspaceRoot, armContext.elfPath);
    const syntheticSourceRel = toWorkspaceSlashPath(armContext.workspaceRoot, armContext.syntheticSourcePath);
    const programRel = toWorkspaceSlashPath(armContext.workspaceRoot, armContext.programPath);
    const workspaceVar = '${workspaceFolder}';
    const elfWorkspacePath = `${workspaceVar}/${elfRel}`;
    const syntheticSourceWorkspacePath = `${workspaceVar}/${syntheticSourceRel}`;
    const guestElfPath = `/mnt/${elfRel}`;
    const runnerPath = `${workspaceVar}/${toWorkspaceSlashPath(armContext.workspaceRoot, path.join(armContext.webGdbDir, 'src', 'runner.js'))}`;
    const nodeWrapperPath = `${workspaceVar}/${toWorkspaceSlashPath(armContext.workspaceRoot, armContext.nodeWrapperPath)}`;
    const name = `Chialisp ARM GDB: ${path.basename(armContext.programPath)}`;
    const miDebuggerArgs = [
        quoteArg(runnerPath),
        '--gdb-server',
        `localhost:${armContext.port}`,
        '--workspace',
        quoteArg(workspaceVar),
        '--import-file',
        quoteArg(elfWorkspacePath),
        '--import-file',
        quoteArg(syntheticSourceWorkspacePath),
        '--interpreter=mi',
    ].join(' ');

    const configuration: vscode.DebugConfiguration = {
        name,
        type: 'cppdbg',
        request: 'launch',
        program: elfWorkspacePath,
        cwd: workspaceVar,
        // cppdbg requires this exact launch.json spelling.
        // eslint-disable-next-line @typescript-eslint/naming-convention
        MIMode: 'gdb',
        miDebuggerPath: nodeWrapperPath,
        miDebuggerArgs,
        stopAtEntry: true,
        externalConsole: false,
        customLaunchSetupCommands: [
            { text: '-gdb-set architecture arm', description: 'Select ARM architecture' },
            { text: `-file-exec-and-symbols ${guestElfPath}`, description: 'Load generated Chialisp ARM ELF symbols' },
            { text: '-target-select remote /dev/ttyS1', description: 'Connect to web-gdb-node serial GDB bridge' },
        ],
        launchCompleteCommand: 'None',
        chialispProgram: `${workspaceVar}/${programRel}`,
        chialispRunArgs: armContext.runArgs,
    };

    let launchJson: any = {
        version: '0.2.0',
        configurations: [],
    };

    try {
        launchJson = parseJsonWithLineComments(await fs.promises.readFile(launchPath, 'utf8'));
    } catch (e: any) {
        if (!e || e.code !== 'ENOENT') {
            throw e;
        }
    }

    if (!Array.isArray(launchJson.configurations)) {
        launchJson.configurations = [];
    }

    launchJson.configurations = launchJson.configurations.filter((item: any) => item && item.name !== name);
    launchJson.configurations.push(configuration);

    await fs.promises.mkdir(vscodeDir, { recursive: true });
    await fs.promises.writeFile(launchPath, JSON.stringify(launchJson, null, 4) + '\n', 'utf8');
    return configuration;
}

export function armGdbDebugActivate(context: vscode.ExtensionContext) {
    context.subscriptions.push({
        dispose: () => {
            if (runningStub) {
                runningStub.kill();
                runningStub = undefined;
            }
        },
    });

    context.subscriptions.push(
        vscode.commands.registerCommand(`${extensionName}.prepareArmGdbDebug`, async (uri?: vscode.Uri) => {
            const selectedUri = uri ?? vscode.window.activeTextEditor?.document.uri;
            if (!selectedUri) {
                throw new Error('could not get uri for active text editor');
            }

            const folder = vscode.workspace.getWorkspaceFolder(selectedUri);
            if (!folder) {
                throw new Error('No workspace root for open files');
            }

            const selfExtension = vscode.extensions.getExtension(`${publisher}.${extensionName}`);
            if (!selfExtension) {
                throw new Error('no extension matches our id');
            }

            const channel = getOutputChannel();
            channel.show(true);

            const configuration = await vscode.window.withProgress({
                location: vscode.ProgressLocation.Notification,
                title: 'Preparing Chialisp ARM GDB debug launch',
                cancellable: false,
            }, async (progress) => {
                progress.report({ message: 'Downloading web-gdb-node' });
                const armContext = await prepareContext(folder, selectedUri.fsPath);
                if (!armContext) {
                    return undefined;
                }

                progress.report({ message: 'Building ELF and starting GDB stub' });
                await startStubService(context, armContext);

                progress.report({ message: 'Writing .vscode/launch.json' });
                return writeLaunchJson(armContext);
            });

            if (configuration) {
                void vscode.window.showInformationMessage(
                    `Wrote ${configuration.name} and started the ARM GDB stub. Start that launch configuration to enter gdb-multiarch.`
                );
            }
        })
    );
}
