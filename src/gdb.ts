import * as vscode from 'vscode';
import * as path from 'path';
import * as fs from 'fs';
import { ChildProcessWithoutNullStreams, spawn, spawnSync } from 'child_process';
import { TextEncoder } from 'util';

import { extensionName, ourExtension } from './constants';
import { log } from './logger';

type BackendPreference = 'auto' | 'local' | 'web';

interface SetupCommand {
    text: string;
    description?: string;
    ignoreFailures?: boolean;
}

interface ChialispGdbSection {
    enabled?: boolean;
    backend?: string;
    gdbServer?: string;
    prettyPrinter?: string;
    webGdbRunner?: string;
    source?: string;
    includePaths?: string[];
    env?: string;
    syntheticOutput?: string;
}

interface ChialispGdbConfiguration extends vscode.DebugConfiguration {
    chialisp?: ChialispGdbSection;
    args?: string[];
    setupCommands?: SetupCommand[];
    miDebuggerPath?: string;
    miDebuggerArgs?: string;
    miDebuggerServerAddress?: string;
    MIMode?: string;
    program?: string;
    cwd?: string;
    __chialispRunnerId?: string;
}

const templateDebugName = "Chialisp GDB";
const wasmRunnerReadyTimeoutMs = 15000;
const activeRunnerProcesses = new Map<string, ChildProcessWithoutNullStreams>();

interface WasmRunnerResult {
    process: ChildProcessWithoutNullStreams;
    serverAddress: string;
    elfPath: string;
    syntheticPath: string;
}

function normalizeBackend(input: string | undefined): BackendPreference {
    if (input === 'local' || input === 'web') {
        return input;
    }
    return 'auto';
}

function shellQuote(arg: string): string {
    if (arg.match(/^[a-zA-Z0-9_./:=-]+$/)) {
        return arg;
    }

    return `"${arg.replace(/\\/g, '\\\\').replace(/"/g, '\\"')}"`;
}

function resolveWorkspacePlaceholders(value: string, workspaceFolder: string, filePath?: string): string {
    let rendered = value.replace(/\$\{workspaceFolder\}/g, workspaceFolder);
    if (filePath) {
        rendered = rendered.replace(/\$\{file\}/g, filePath);
        rendered = rendered.replace(/\$\{fileBasenameNoExtension\}/g, path.parse(filePath).name);
    }
    return rendered;
}

function canExecute(commandName: string): boolean {
    try {
        const result = spawnSync(commandName, ['--version'], { timeout: 2000, encoding: 'utf8' });
        return !result.error && result.status === 0;
    } catch (_e) {
        return false;
    }
}

function getTemplateConfiguration(): ChialispGdbConfiguration {
    return {
        name: templateDebugName,
        type: 'cppdbg',
        request: 'launch',
        MIMode: 'gdb',
        cwd: '${workspaceFolder}',
        program: '${workspaceFolder}/.chialisp-gdb/${fileBasenameNoExtension}.elf',
        stopAtEntry: true,
        externalConsole: false,
        args: ['()'],
        setupCommands: [{ text: '-enable-pretty-printing' }],
        chialisp: {
            enabled: true,
            backend: 'auto',
            gdbServer: 'localhost:9001',
            prettyPrinter: '${workspaceFolder}/support/gdb_print_sexp.py',
            webGdbRunner: '${workspaceFolder}/node_modules/web-gdb-node/src/runner.js',
            source: '${file}',
            includePaths: [],
            env: '()',
        },
    };
}

async function ensureTemplateLaunchFile(workspaceFolder: vscode.WorkspaceFolder): Promise<void> {
    const vscodeFolderUri = vscode.Uri.joinPath(workspaceFolder.uri, '.vscode');
    const launchUri = vscode.Uri.joinPath(vscodeFolderUri, 'launch.json');

    try {
        await vscode.workspace.fs.stat(launchUri);
        return;
    } catch (_e) {
        // launch.json does not exist yet.
    }

    const selected = await vscode.window.showInformationMessage(
        "No .vscode/launch.json found. Create a Chialisp GDB template?",
        "Create Template",
        "Not Now"
    );
    if (selected !== "Create Template") {
        return;
    }

    await vscode.workspace.fs.createDirectory(vscodeFolderUri);
    const launchObject = {
        version: '0.2.0',
        configurations: [getTemplateConfiguration()],
    };

    await vscode.workspace.fs.writeFile(
        launchUri,
        new TextEncoder().encode(`${JSON.stringify(launchObject, null, 4)}\n`)
    );
    const document = await vscode.workspace.openTextDocument(launchUri);
    await vscode.window.showTextDocument(document);
}

function getActiveFile(resource?: vscode.Uri): vscode.Uri | undefined {
    if (resource) {
        return resource;
    }
    return vscode.window.activeTextEditor?.document.uri;
}

function resolvePathForWorkspace(workspaceFolder: string, value: string, filePath?: string): string {
    const rendered = resolveWorkspacePlaceholders(value, workspaceFolder, filePath);
    return path.isAbsolute(rendered) ? rendered : path.resolve(workspaceFolder, rendered);
}

function readChialispJsonIncludePaths(workspaceFolder: string): string[] {
    const configPath = path.join(workspaceFolder, 'chialisp.json');
    if (!fs.existsSync(configPath)) {
        return [];
    }

    try {
        const json = JSON.parse(fs.readFileSync(configPath, 'utf8'));
        if (!json || !Array.isArray(json.include_paths)) {
            return [];
        }

        return json.include_paths
            .filter((entry: unknown) => typeof entry === 'string')
            .map((entry: string) => resolvePathForWorkspace(workspaceFolder, entry));
    } catch (e) {
        log.error(`Failed to parse ${configPath}: ${e}`);
        return [];
    }
}

function getConfiguredIncludePaths(
    gdbConfig: ChialispGdbConfiguration,
    workspaceFolder: string,
    sourcePath: string,
): string[] {
    const launchConfigPaths = (gdbConfig.chialisp?.includePaths ?? [])
        .filter((entry) => typeof entry === 'string')
        .map((entry) => resolvePathForWorkspace(workspaceFolder, entry, sourcePath));
    const workspaceIncludePaths = readChialispJsonIncludePaths(workspaceFolder);
    return [...new Set([...launchConfigPaths, ...workspaceIncludePaths])];
}

function getCompileEnv(gdbConfig: ChialispGdbConfiguration, extensionConfig: vscode.WorkspaceConfiguration): string {
    const configEnv = gdbConfig.chialisp?.env;
    if (typeof configEnv === 'string' && configEnv.trim().length > 0) {
        return configEnv;
    }
    if (Array.isArray(gdbConfig.args) && gdbConfig.args.length > 0 && typeof gdbConfig.args[0] === 'string') {
        return gdbConfig.args[0];
    }
    return extensionConfig.get<string>('gdb.defaultEnv', '()');
}

function resolveDebugSourcePath(
    gdbConfig: ChialispGdbConfiguration,
    workspaceFolder: string,
): string | undefined {
    const configured = gdbConfig.chialisp?.source;
    const activeUri = vscode.window.activeTextEditor?.document.uri;
    const activeFile = activeUri?.fsPath;
    if (configured) {
        return resolvePathForWorkspace(workspaceFolder, configured, activeFile);
    }

    if (activeUri) {
        return activeUri.fsPath;
    }

    return undefined;
}

function resolveWasmRunnerScriptPath(
    workspaceFolder: string,
    extensionConfig: vscode.WorkspaceConfiguration,
): string {
    const configuredRunnerPath = extensionConfig.get<string>('gdb.wasmRunnerPath', '').trim();
    if (configuredRunnerPath.length > 0) {
        return resolveWorkspacePlaceholders(configuredRunnerPath, workspaceFolder);
    }

    const extension = vscode.extensions.getExtension(ourExtension);
    if (!extension) {
        throw new Error(`Extension ${ourExtension} is not available`);
    }
    return path.join(extension.extensionPath, 'debug', 'build', 'gdb_runner.js');
}

function createRunnerId(): string {
    return `${Date.now().toString(16)}-${Math.random().toString(16).slice(2, 10)}`;
}

function disposeRunner(runnerId: string | undefined): void {
    if (!runnerId) {
        return;
    }
    const running = activeRunnerProcesses.get(runnerId);
    if (!running) {
        return;
    }
    activeRunnerProcesses.delete(runnerId);
    try {
        running.kill('SIGTERM');
    } catch (_e) {
        // Best effort cleanup.
    }
}

async function startWasmGdbRunner(
    workspaceFolder: string,
    sourcePath: string,
    includePaths: string[],
    compileEnv: string,
    gdbServerAddress: string,
    elfOutputPath: string,
    syntheticOutputPath: string,
    extensionConfig: vscode.WorkspaceConfiguration
): Promise<WasmRunnerResult> {
    const wasmRunnerScript = resolveWasmRunnerScriptPath(workspaceFolder, extensionConfig);
    if (!fs.existsSync(wasmRunnerScript)) {
        throw new Error(`Wasm gdb runner script not found at ${wasmRunnerScript}`);
    }

    const nodePath = extensionConfig.get<string>('gdb.wasmRunnerNodePath', 'node');
    const runnerArgs: string[] = [
        wasmRunnerScript,
        '--workspace', workspaceFolder,
        '--source', sourcePath,
        '--env', compileEnv,
        '--gdb-server', gdbServerAddress,
        '--output', elfOutputPath,
        '--synthetic-output', syntheticOutputPath,
    ];
    for (const includePath of includePaths) {
        runnerArgs.push('--include', includePath);
    }

    return new Promise((resolve, reject) => {
        const childProcess = spawn(nodePath, runnerArgs, {
            cwd: workspaceFolder,
            stdio: ['pipe', 'pipe', 'pipe'],
        }) as ChildProcessWithoutNullStreams;

        let resolved = false;
        let stdoutBuffer = '';
        let stderrBuffer = '';
        let readyAddress = '';
        let elfPath = '';
        let syntheticPath = '';

        const timeout = setTimeout(() => {
            if (resolved) {
                return;
            }
            try {
                childProcess.kill('SIGTERM');
            } catch (_e) {
                // Nothing to do.
            }
            reject(new Error(`Timed out waiting for wasm gdb runner readiness. stderr:\n${stderrBuffer}`));
        }, wasmRunnerReadyTimeoutMs);

        const processLine = (line: string) => {
            const trimmed = line.trim();
            if (trimmed.length === 0) {
                return;
            }
            if (trimmed.startsWith('READY ')) {
                readyAddress = trimmed.substring(6);
            } else if (trimmed.startsWith('ELF ')) {
                elfPath = trimmed.substring(4);
            } else if (trimmed.startsWith('SYNTHETIC ')) {
                syntheticPath = trimmed.substring(10);
            } else {
                log.info(`wasm gdb runner: ${trimmed}`);
            }

            if (!resolved && readyAddress && elfPath) {
                resolved = true;
                clearTimeout(timeout);
                resolve({
                    process: childProcess,
                    serverAddress: readyAddress,
                    elfPath,
                    syntheticPath,
                });
            }
        };

        childProcess.stdout.on('data', (chunk) => {
            stdoutBuffer += chunk.toString('utf8');
            const lines = stdoutBuffer.split(/\r?\n/);
            stdoutBuffer = lines.pop() ?? '';
            for (const line of lines) {
                processLine(line);
            }
        });

        childProcess.stderr.on('data', (chunk) => {
            stderrBuffer += chunk.toString('utf8');
        });

        childProcess.once('error', (error) => {
            if (resolved) {
                return;
            }
            clearTimeout(timeout);
            reject(error);
        });

        childProcess.once('exit', (code) => {
            if (resolved) {
                return;
            }
            clearTimeout(timeout);
            reject(new Error(`wasm gdb runner exited early with code ${code}. stderr:\n${stderrBuffer}`));
        });
    });
}

function sourceCommandForPrettyPrinter(prettyPrinterPath: string): SetupCommand {
    return {
        text: `source ${shellQuote(prettyPrinterPath)}`,
        description: 'Load chialisp SExp pretty-printer',
        ignoreFailures: true,
    };
}

function ensureSetupCommand(config: ChialispGdbConfiguration, command: SetupCommand): void {
    if (!config.setupCommands) {
        config.setupCommands = [];
    }

    const found = config.setupCommands.some((c) => c.text === command.text);
    if (!found) {
        config.setupCommands.push(command);
    }
}

function resolveWebRunnerPath(
    workspaceFolder: string,
    chialispConfig: ChialispGdbSection,
    extensionConfig: vscode.WorkspaceConfiguration
): string {
    const configuredValue = chialispConfig.webGdbRunner
        ?? extensionConfig.get<string>('gdb.webGdbRunnerPath', '${workspaceFolder}/node_modules/web-gdb-node/src/runner.js');
    return resolveWorkspacePlaceholders(configuredValue, workspaceFolder);
}

function chooseBackend(
    workspaceFolder: string,
    chialispConfig: ChialispGdbSection,
    extensionConfig: vscode.WorkspaceConfiguration
): BackendPreference | undefined {
    const preferredBackend = normalizeBackend(
        chialispConfig.backend ?? extensionConfig.get<string>('gdb.backend', 'auto')
    );
    const localDebuggerPath = extensionConfig.get<string>('gdb.localDebuggerPath', 'gdb-multiarch');
    const hasLocalGdb = canExecute(localDebuggerPath);

    if (preferredBackend === 'local') {
        if (!hasLocalGdb) {
            void vscode.window.showErrorMessage(
                `Configured debugger '${localDebuggerPath}' was not found. ` +
                `Install gdb-multiarch or set '${extensionName}.gdb.backend' to 'web'.`
            );
            return undefined;
        }
        return 'local';
    }

    if (preferredBackend === 'web') {
        return 'web';
    }

    if (hasLocalGdb) {
        return 'local';
    }

    const webRunnerPath = resolveWebRunnerPath(workspaceFolder, chialispConfig, extensionConfig);
    if (!fs.existsSync(webRunnerPath)) {
        void vscode.window.showErrorMessage(
            `Neither local '${localDebuggerPath}' nor web-gdb-node runner were found. ` +
            `Install gdb-multiarch or install web-gdb-node in this workspace.`
        );
        return undefined;
    }
    return 'web';
}

function readLaunchConfigEntry(
    workspaceFolder: vscode.WorkspaceFolder,
    configName: string
): ChialispGdbConfiguration | undefined {
    const launchConfig = vscode.workspace.getConfiguration('launch', workspaceFolder.uri);
    const launchEntries = launchConfig.get<any[]>('configurations', []);
    for (const entry of launchEntries) {
        if (entry && typeof entry.name === 'string' && entry.name === configName) {
            return entry as ChialispGdbConfiguration;
        }
    }
    return undefined;
}

function findWorkspaceLaunchConfigNames(workspaceFolder: vscode.WorkspaceFolder): string[] {
    const launchConfig = vscode.workspace.getConfiguration('launch', workspaceFolder.uri);
    const launchEntries = launchConfig.get<any[]>('configurations', []);
    const names: string[] = [];
    for (const entry of launchEntries) {
        if (entry && entry.chialisp && entry.chialisp.enabled && typeof entry.name === 'string') {
            names.push(entry.name);
        }
    }
    return names;
}

export function gdbActivate(context: vscode.ExtensionContext): void {
    const provider: vscode.DebugConfigurationProvider = {
        resolveDebugConfigurationWithSubstitutedVariables: async (
            folder,
            config,
            _token
        ): Promise<vscode.DebugConfiguration | undefined> => {
            const gdbConfig = config as ChialispGdbConfiguration;
            if (!gdbConfig.chialisp?.enabled) {
                return gdbConfig;
            }

            const workspaceFolder = folder?.uri.fsPath
                ?? gdbConfig.cwd
                ?? vscode.workspace.workspaceFolders?.[0]?.uri.fsPath;
            if (!workspaceFolder) {
                void vscode.window.showErrorMessage("Could not resolve workspace folder for Chialisp GDB config");
                return undefined;
            }

            const extensionConfig = vscode.workspace.getConfiguration(
                extensionName,
                folder?.uri ?? vscode.Uri.file(workspaceFolder)
            );
            const backend = chooseBackend(workspaceFolder, gdbConfig.chialisp, extensionConfig);
            if (!backend) {
                return undefined;
            }

            const sourcePath = resolveDebugSourcePath(gdbConfig, workspaceFolder);
            if (!sourcePath) {
                void vscode.window.showErrorMessage("No source file found for Chialisp GDB session");
                return undefined;
            }

            const gdbServerAddress = gdbConfig.chialisp.gdbServer
                ?? extensionConfig.get<string>('gdb.serverAddress', 'localhost:9001');
            const prettyPrinterTemplate = gdbConfig.chialisp.prettyPrinter
                ?? extensionConfig.get<string>('gdb.prettyPrinterPath', '${workspaceFolder}/support/gdb_print_sexp.py');
            const prettyPrinterPath = resolveWorkspacePlaceholders(prettyPrinterTemplate, workspaceFolder);
            const compileEnv = getCompileEnv(gdbConfig, extensionConfig);
            const desiredElfOutput = gdbConfig.program
                ? resolvePathForWorkspace(workspaceFolder, gdbConfig.program, sourcePath)
                : path.join(workspaceFolder, '.chialisp-gdb', `${path.parse(sourcePath).name}.elf`);
            const desiredSyntheticOutput = gdbConfig.chialisp?.syntheticOutput
                ? resolvePathForWorkspace(workspaceFolder, gdbConfig.chialisp.syntheticOutput, sourcePath)
                : `${desiredElfOutput}.synthetic.clsp`;
            const includePaths = getConfiguredIncludePaths(gdbConfig, workspaceFolder, sourcePath);

            let wasmRunner: WasmRunnerResult;
            try {
                wasmRunner = await startWasmGdbRunner(
                    workspaceFolder,
                    sourcePath,
                    includePaths,
                    compileEnv,
                    gdbServerAddress,
                    desiredElfOutput,
                    desiredSyntheticOutput,
                    extensionConfig
                );
            } catch (e) {
                void vscode.window.showErrorMessage(`Failed to start Chialisp wasm gdb runner: ${e}`);
                return undefined;
            }

            const runnerId = createRunnerId();
            activeRunnerProcesses.set(runnerId, wasmRunner.process);
            gdbConfig.__chialispRunnerId = runnerId;

            ensureSetupCommand(gdbConfig, { text: '-enable-pretty-printing' });
            ensureSetupCommand(gdbConfig, sourceCommandForPrettyPrinter(prettyPrinterPath));

            gdbConfig.type = 'cppdbg';
            gdbConfig.request = gdbConfig.request ?? 'launch';
            gdbConfig.MIMode = 'gdb';
            gdbConfig.cwd = gdbConfig.cwd ?? workspaceFolder;
            gdbConfig.program = wasmRunner.elfPath;

            if (backend === 'local') {
                const localDebuggerPath = extensionConfig.get<string>('gdb.localDebuggerPath', 'gdb-multiarch');
                gdbConfig.miDebuggerPath = localDebuggerPath;
                gdbConfig.miDebuggerServerAddress = wasmRunner.serverAddress;
            } else {
                const webRunnerPath = resolveWebRunnerPath(workspaceFolder, gdbConfig.chialisp, extensionConfig);
                if (!fs.existsSync(webRunnerPath)) {
                    disposeRunner(runnerId);
                    void vscode.window.showErrorMessage(
                        `web-gdb-node runner not found at ${webRunnerPath}. ` +
                        "Install web-gdb-node or switch backend to local."
                    );
                    return undefined;
                }

                const webNodePath = extensionConfig.get<string>('gdb.webNodePath', 'node');
                const extraArgs = extensionConfig.get<string[]>('gdb.webGdbExtraArgs', []);
                const webArgs = [
                    webRunnerPath,
                    '--gdb-server',
                    wasmRunner.serverAddress,
                    '--workspace',
                    workspaceFolder,
                    '--ex',
                    'target remote /dev/ttyS1',
                    ...extraArgs,
                ];
                gdbConfig.miDebuggerPath = webNodePath;
                gdbConfig.miDebuggerArgs = webArgs.map(shellQuote).join(' ');
                delete gdbConfig.miDebuggerServerAddress;
            }

            return gdbConfig;
        }
    };

    context.subscriptions.push(vscode.debug.registerDebugConfigurationProvider('cppdbg', provider));
    context.subscriptions.push(vscode.debug.onDidTerminateDebugSession((session) => {
        const config = session.configuration as ChialispGdbConfiguration;
        disposeRunner(config.__chialispRunnerId);
    }));

    context.subscriptions.push(
        vscode.commands.registerCommand(`${extensionName}.gdbDebug`, async (resource?: vscode.Uri) => {
            const sourceUri = getActiveFile(resource);
            if (!sourceUri) {
                void vscode.window.showErrorMessage("Open a Chialisp source file to start GDB debugging");
                return;
            }

            const workspaceFolder = vscode.workspace.getWorkspaceFolder(sourceUri)
                ?? vscode.workspace.workspaceFolders?.[0];
            if (!workspaceFolder) {
                void vscode.window.showErrorMessage("No workspace folder is open");
                return;
            }

            await ensureTemplateLaunchFile(workspaceFolder);
            await vscode.window.showTextDocument(sourceUri);
            const launchConfigNames = findWorkspaceLaunchConfigNames(workspaceFolder);
            let launchConfigName = templateDebugName;
            if (launchConfigNames.length === 1) {
                launchConfigName = launchConfigNames[0];
            } else if (launchConfigNames.length > 1) {
                const selected = await vscode.window.showQuickPick(
                    launchConfigNames,
                    { placeHolder: 'Select Chialisp launch configuration to debug' }
                );
                if (!selected) {
                    return;
                }
                launchConfigName = selected;
            }
            const started = await vscode.debug.startDebugging(workspaceFolder, launchConfigName);
            if (!started) {
                void vscode.window.showErrorMessage("Could not start Chialisp GDB debugging session");
            }
        })
    );

    log.info("chialisp gdb prototype activated");
}

export function gdbDeactivate(): void {
    for (const runnerId of activeRunnerProcesses.keys()) {
        disposeRunner(runnerId);
    }
}
