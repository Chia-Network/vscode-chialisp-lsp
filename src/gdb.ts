import * as vscode from 'vscode';
import * as path from 'path';
import * as fs from 'fs';
import { spawnSync } from 'child_process';
import { TextEncoder } from 'util';

import { extensionName } from './constants';
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
}

interface ChialispGdbConfiguration extends vscode.DebugConfiguration {
    chialisp?: ChialispGdbSection;
    setupCommands?: SetupCommand[];
    miDebuggerPath?: string;
    miDebuggerArgs?: string;
    miDebuggerServerAddress?: string;
    MIMode?: string;
    program?: string;
    cwd?: string;
}

const templateDebugName = "Chialisp GDB";
const armtxTerminalName = "Chialisp armtx";

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
        setupCommands: [{ text: '-enable-pretty-printing' }],
        chialisp: {
            enabled: true,
            backend: 'auto',
            gdbServer: 'localhost:9001',
            prettyPrinter: '${workspaceFolder}/support/gdb_print_sexp.py',
            webGdbRunner: '${workspaceFolder}/node_modules/web-gdb-node/src/runner.js',
            source: '${file}',
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

async function runArmtxIfEnabled(workspaceFolder: vscode.WorkspaceFolder, sourceFile?: string): Promise<void> {
    const config = vscode.workspace.getConfiguration(extensionName, workspaceFolder.uri);
    const autoStartArmtx = config.get<boolean>('gdb.autoStartArmtx', true);
    if (!autoStartArmtx || !sourceFile) {
        return;
    }

    const armtxCommandTemplate = config.get<string>('gdb.armtxCommand', 'armtx "${file}"');
    const renderedCommand = resolveWorkspacePlaceholders(
        armtxCommandTemplate,
        workspaceFolder.uri.fsPath,
        sourceFile
    );

    let terminal = vscode.window.terminals.find((t) => t.name === armtxTerminalName);
    if (!terminal) {
        terminal = vscode.window.createTerminal({
            name: armtxTerminalName,
            cwd: workspaceFolder.uri.fsPath,
        });
    }
    terminal.show(true);
    terminal.sendText(renderedCommand, true);

    // Give armtx a short window to start gdbserver before attaching.
    await new Promise((resolve) => setTimeout(resolve, 800));
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
    workspaceFolder: vscode.WorkspaceFolder | undefined,
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

    if (workspaceFolder) {
        const webRunnerPath = resolveWebRunnerPath(workspaceFolder.uri.fsPath, chialispConfig, extensionConfig);
        if (!fs.existsSync(webRunnerPath)) {
            void vscode.window.showErrorMessage(
                `Neither local '${localDebuggerPath}' nor web-gdb-node runner were found. ` +
                `Install gdb-multiarch or install web-gdb-node in this workspace.`
            );
            return undefined;
        }
    }
    return 'web';
}

function createRuntimeConfiguration(workspaceFolder: vscode.WorkspaceFolder, sourceUri: vscode.Uri): ChialispGdbConfiguration {
    const config = getTemplateConfiguration();
    config.cwd = workspaceFolder.uri.fsPath;
    config.program = path.join(
        workspaceFolder.uri.fsPath,
        '.chialisp-gdb',
        `${path.parse(sourceUri.fsPath).name}.elf`
    );
    config.chialisp = {
        ...config.chialisp,
        source: sourceUri.fsPath,
    };
    return config;
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
            const backend = chooseBackend(folder, gdbConfig.chialisp, extensionConfig);
            if (!backend) {
                return undefined;
            }

            const gdbServerAddress = gdbConfig.chialisp.gdbServer
                ?? extensionConfig.get<string>('gdb.serverAddress', 'localhost:9001');
            const prettyPrinterTemplate = gdbConfig.chialisp.prettyPrinter
                ?? extensionConfig.get<string>('gdb.prettyPrinterPath', '${workspaceFolder}/support/gdb_print_sexp.py');
            const prettyPrinterPath = resolveWorkspacePlaceholders(prettyPrinterTemplate, workspaceFolder);

            ensureSetupCommand(gdbConfig, { text: '-enable-pretty-printing' });
            ensureSetupCommand(gdbConfig, sourceCommandForPrettyPrinter(prettyPrinterPath));

            gdbConfig.type = 'cppdbg';
            gdbConfig.request = gdbConfig.request ?? 'launch';
            gdbConfig.MIMode = 'gdb';
            gdbConfig.cwd = gdbConfig.cwd ?? workspaceFolder;

            if (backend === 'local') {
                const localDebuggerPath = extensionConfig.get<string>('gdb.localDebuggerPath', 'gdb-multiarch');
                gdbConfig.miDebuggerPath = localDebuggerPath;
                gdbConfig.miDebuggerServerAddress = gdbServerAddress;
            } else {
                const webRunnerPath = resolveWebRunnerPath(workspaceFolder, gdbConfig.chialisp, extensionConfig);
                if (!fs.existsSync(webRunnerPath)) {
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
                    gdbServerAddress,
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
            await runArmtxIfEnabled(workspaceFolder, sourceUri.fsPath);

            const runtimeConfig = createRuntimeConfiguration(workspaceFolder, sourceUri);
            const started = await vscode.debug.startDebugging(workspaceFolder, runtimeConfig);
            if (!started) {
                void vscode.window.showErrorMessage("Could not start Chialisp GDB debugging session");
            }
        })
    );

    log.info("chialisp gdb prototype activated");
}

export function gdbDeactivate(): void {
}
