import * as vscode from 'vscode';
import * as fs from 'fs';
import * as path from 'path';

import { extensionName, publisher } from './constants';

const DEBUG_WORK_DIR = '.chialisp-debug';
const GDB_SEXP_PRINTER_RESOURCE_PATH = path.join('wasm', 'resources', 'gdb_print_sexp.py');
const GDB_SEXP_PRINTER_GUEST_PATH = '/mnt/gdb_print_sexp.py';
const GDB_DEBUG_LAUNCHER_PATH = path.join('scripts', 'arm-gdb', 'gdb_debug_launcher.js');

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
    runArgs: string[];
}

let outputChannel: vscode.OutputChannel | undefined;

function getOutputChannel(): vscode.OutputChannel {
    if (!outputChannel) {
        outputChannel = vscode.window.createOutputChannel('Chialisp ARM GDB');
    }

    return outputChannel;
}

function quoteArg(value: string): string {
    if (value.length > 0 && !/[\s"]/.test(value)) {
        return value;
    }

    let result = '"';
    let backslashes = 0;
    for (const char of value) {
        if (char === '\\') {
            backslashes += 1;
        } else if (char === '"') {
            result += '\\'.repeat(backslashes * 2 + 1);
            result += char;
            backslashes = 0;
        } else {
            result += '\\'.repeat(backslashes);
            result += char;
            backslashes = 0;
        }
    }

    result += '\\'.repeat(backslashes * 2);
    result += '"';
    return result;
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

async function writeNodeWrapper(workDir: string, extensionPath: string): Promise<string> {
    const wrapperPath = path.join(workDir, process.platform === 'win32' ? 'vscode-node.cmd' : 'vscode-node');
    const executable = process.execPath;
    const launcherPath = path.join(extensionPath, GDB_DEBUG_LAUNCHER_PATH);

    if (process.platform === 'win32') {
        const content = [
            '@echo off',
            'setlocal EnableExtensions EnableDelayedExpansion',
            'set ELECTRON_RUN_AS_NODE=1',
            'set ATOM_SHELL_INTERNAL_RUN_AS_NODE=1',
            `set "NODE_EXECUTABLE=${executable}"`,
            `set "CHIALISP_GDB_LAUNCHER=${launcherPath}"`,
            '"%NODE_EXECUTABLE%" "%CHIALISP_GDB_LAUNCHER%" %*',
            'exit /b %ERRORLEVEL%',
            '',
        ].join(String.fromCharCode(13, 10));
        await fs.promises.writeFile(wrapperPath, content, 'utf8');
    } else {
        const content = [
            '#!/bin/sh',
            'export ELECTRON_RUN_AS_NODE=1',
            'export ATOM_SHELL_INTERNAL_RUN_AS_NODE=1',
            `exec ${quoteSh(executable)} ${quoteSh(launcherPath)} "$@"`,
            '',
        ].join(String.fromCharCode(10));
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

async function prepareContext(folder: vscode.WorkspaceFolder, programPath: string, extensionPath: string): Promise<ArmGdbContext | undefined> {
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
    await fs.promises.mkdir(buildDir, { recursive: true });
    // cppdbg can validate the program path before invoking the wrapper; the launcher overwrites these.
    await Promise.all([
        fs.promises.open(elfPath, 'a').then((file) => file.close()),
        fs.promises.open(syntheticSourcePath, 'a').then((file) => file.close()),
    ]);
    const nodeWrapperPath = await writeNodeWrapper(workDir, extensionPath);

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
        runArgs,
    };
}

async function writeLaunchJson(armContext: ArmGdbContext, extensionPath: string): Promise<vscode.DebugConfiguration> {
    const vscodeDir = path.join(armContext.workspaceRoot, '.vscode');
    const launchPath = path.join(vscodeDir, 'launch.json');
    const elfRel = toWorkspaceSlashPath(armContext.workspaceRoot, armContext.elfPath);
    const syntheticSourceRel = toWorkspaceSlashPath(armContext.workspaceRoot, armContext.syntheticSourcePath);
    const programRel = toWorkspaceSlashPath(armContext.workspaceRoot, armContext.programPath);
    const gdbSexpPrinterResourcePath = path.join(extensionPath, GDB_SEXP_PRINTER_RESOURCE_PATH);
    const workspaceVar = '${workspaceFolder}';
    const elfWorkspacePath = `${workspaceVar}/${elfRel}`;
    const syntheticSourceWorkspacePath = `${workspaceVar}/${syntheticSourceRel}`;
    const guestElfPath = `/mnt/${path.basename(armContext.elfPath)}`;
    const runnerPath = `${workspaceVar}/${toWorkspaceSlashPath(armContext.workspaceRoot, path.join(armContext.webGdbDir, 'src', 'runner.js'))}`;
    const nodeWrapperPath = `${workspaceVar}/${toWorkspaceSlashPath(armContext.workspaceRoot, armContext.nodeWrapperPath)}`;
    const name = `Chialisp ARM GDB: ${path.basename(armContext.programPath)}`;
    const miDebuggerArgsParts = [
        quoteArg(runnerPath),
        '--gdb-server',
        '127.0.0.1:0',
        '--chialisp-workspace',
        quoteArg(workspaceVar),
        '--chialisp-program',
        quoteArg(`${workspaceVar}/${programRel}`),
        '--chialisp-run-args-json',
        quoteArg(JSON.stringify(armContext.runArgs)),
        '--chialisp-elf-out',
        quoteArg(elfWorkspacePath),
        '--chialisp-synthetic-source-out',
        quoteArg(syntheticSourceWorkspacePath),
        '--chialisp-gdb-printer',
        quoteArg(gdbSexpPrinterResourcePath),
        '--workspace',
        quoteArg(workspaceVar),
        '--import-file',
        quoteArg(elfWorkspacePath),
        '--import-file',
        quoteArg(syntheticSourceWorkspacePath),
        '--import-file',
        quoteArg(gdbSexpPrinterResourcePath),
    ];
    const gdbExCommands = [
        `file ${guestElfPath}`,
        `source ${GDB_SEXP_PRINTER_GUEST_PATH}`,
        'dir /mnt',
        'target remote /dev/ttyS1',
    ];
    for (const command of gdbExCommands) {
        miDebuggerArgsParts.push('--ex', quoteArg(command));
    }
    const miDebuggerArgs = miDebuggerArgsParts.join(' ');

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
        // GDB starts inside web-gdb-node with --ex commands, so cppdbg should not send a second launch sequence.
        customLaunchSetupCommands: [],
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
                progress.report({ message: 'Preparing debug launch files' });
                const armContext = await prepareContext(folder, selectedUri.fsPath, context.extensionPath);
                if (!armContext) {
                    return undefined;
                }

                progress.report({ message: 'Writing .vscode/launch.json' });
                return writeLaunchJson(armContext, context.extensionPath);
            });

            if (configuration) {
                void vscode.window.showInformationMessage(
                    `Wrote ${configuration.name}. Start that launch configuration to build the ARM debug target and attach GDB.`
                );
            }
        })
    );
}
