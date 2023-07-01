import * as vscode from 'vscode';
import * as fs from 'fs';
import * as os from 'os';
import * as path from 'path';
import * as net from 'net';
import {Stream, Writable} from 'stream';
import {ChildProcess, fork, ForkOptions} from 'child_process';

import {publisher, extensionName} from './constants';
import {log, stringify} from './logger';
import {EmptyDisposable, isspace, guessFileType} from './utils';

// Thanks: https://github.com/mfine15/vscode-languageclient/blob/082cf5541e3568111c69710fbac189f467acf552/src/utils/electron.ts
// The debug extesion adapter doesn't support a simple child_process runner as
// language server does, so I'm copying the language server's system here.
function makeRandomHexString(length: number): string {
	  let chars = ['0', '1', '2', '3', '4', '5', '6', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'];
	  let result = '';
	  for (let i = 0; i < length; i++) {
		    let idx = Math.floor(chars.length * Math.random());
		    result += chars[idx];
	  }
	  return result;
}

export function generatePipeName(): string {
	  let randomName = 'vscode-lang-' + makeRandomHexString(40);
	  if (process.platform === 'win32') {
		    return '\\\\.\\pipe\\' + randomName + '-sock';
	  }

	  // Mac/Unix: use socket file
	  return path.join(os.tmpdir(), randomName + '.sock');
}

interface RunningSubprocess {
    running: ChildProcess | undefined,
    stdin: Stream | undefined,
    stdout: Stream | undefined,
    pipe: any | undefined
};

let runningSubprocess: RunningSubprocess = {
    running: undefined,
    stdin: undefined,
    stdout: undefined,
    pipe: undefined
};

type ResolveFunction = (v: any) => void;
type RejectFunction = (v: any) => void;

function startSubprocess(
    resolve: ResolveFunction,
    reject: RejectFunction,
    modulePath: string,
    workspaceFolder: string
) {
    if (runningSubprocess.running) {
        resolve(runningSubprocess);
        return;
    }

    try {
        let options: ForkOptions = {
            stdio: 'pipe',
            env: {
                ELECTRON_RUN_AS_NODE: '1',
                ATOM_SHELL_INTERNAL_RUN_AS_NODE: '1',
                WORKSPACE_FOLDER: workspaceFolder,
                ...process.env
            },
            cwd: workspaceFolder
        };

        let newProcess = fork(modulePath, [], options);
        runningSubprocess.running = newProcess;
        newProcess.stdout?.pause();

        // Uncomment to surface output in the debug display in vscode.
        // newProcess.stderr?.addListener('data', (b) => {
        //     log.info(`stderr> ${b.toString('utf8')}`);
        // });

        // Handle exit.
        newProcess.once('error', (err) => {
            log.info(`newProcess error ${err}`);
            if (runningSubprocess.running) {
                runningSubprocess.running = undefined;
                reject(err);
            }
        });
        newProcess.once('exit', () => {
            log.info('debug process terminated');
            if (runningSubprocess.running) {
                runningSubprocess.running = undefined;
                // Nothing to do.
            }
        });

        resolve(runningSubprocess);
    } catch (e) {
        reject(e);
    }
}

// Start debug process with stdio connected to the given named pipe.
async function forkToPipe(pipeName: string, modulePath: string, workspaceFolder: string) {
    return new Promise((resolve, reject) => {
        let pipeServer: net.Server = net.createServer((targetPipe) => {
            // Connect the process up.
            let newProcess = runningSubprocess.running;
            if (newProcess && newProcess.stdin && newProcess.stdout) {
                newProcess.stdout.addListener('data', (b) => {
                    targetPipe.write(b);
                });
                newProcess.stdout.resume();
                targetPipe.on('data', (b) => {
                    newProcess?.stdin?.write(b);
                });
                targetPipe.on('close', () => {
                    newProcess?.kill();
                    runningSubprocess.running = undefined;
                    runningSubprocess.pipe = undefined;
                    runningSubprocess.stdin = undefined;
                    runningSubprocess.stdout = undefined;
                });
                newProcess.once('exit', () => {
                    log.info(`debug subprocess exited`);
                    targetPipe.end();
                });
            } else {
                log.info(`debug subprocess terminated early`);
                targetPipe.end();
            }
        });
        pipeServer.listen(pipeName);
        log.info('starting process');
        startSubprocess(resolve, reject, modulePath, workspaceFolder);
    });
}

export function debuggerActivate(context: vscode.ExtensionContext) {
    const config = vscode.workspace.getConfiguration(extensionName);

    const selfExtension = vscode.extensions.getExtension(`${publisher}.${extensionName}`);
    const extensionPath = selfExtension?.extensionPath;
    if (!extensionPath) {
        throw new Error("no extension matches our id");
    }

    const modulePath = path.join(extensionPath, "debug/build/runner.js");

    const debugAdapter: vscode.DebugAdapterDescriptorFactory = {
        createDebugAdapterDescriptor: (session: vscode.DebugSession, executable: vscode.DebugAdapterExecutable | undefined) => {
            if (process.env.CHIALISP_DBG) {
                return new vscode.DebugAdapterExecutable(process.env.CHIALISP_DBG);
            } else {
                let namedPipeName = generatePipeName();
                return forkToPipe(namedPipeName, modulePath, session.workspaceFolder ? session.workspaceFolder.uri.fsPath : ".").then(() => {
                    return new vscode.DebugAdapterNamedPipeServer(namedPipeName);
                });
            }
        }
    };
    var adapterRegistration: vscode.Disposable = new EmptyDisposable();
    try {
        adapterRegistration = vscode.debug.registerDebugAdapterDescriptorFactory("dev-chialisp", debugAdapter);
    } catch (e) {
        log.info(`adapter registration throws ${e}`);
        throw e;
    }

    context.subscriptions.push(adapterRegistration);

    var configProvider: vscode.Disposable = vscode.debug.registerDebugConfigurationProvider("dev-chialisp", {
        provideDebugConfigurations: async (folder, token) => {
            return [{
                name: "chialisp",
                type: "chialisp",
                request: "launch",
                program: "${workspaceFolder}/program.clvm.hex",
                stopOnEntry: false,
                yieldSteps: 4096,
                onlyDebugGlob: "<${workspaceFolder}/**/*>"
            }];
        },
        resolveDebugConfiguration: async (folder, config, token) => {
            if (!config.type) {
                return {
                    name: "${fileBasename}",
                    type: "chialisp",
                    request: "launch",
                    program: "${file}"
                };
            }
            return config;
        },
        resolveDebugConfigurationWithSubstitutedVariables: async (folder, debugConfiguration, token) => {
            // We can do the rest of our checking server side.
            return debugConfiguration;
        }
    });

    context.subscriptions.push(
        vscode.commands.registerCommand(
            `${extensionName}.startDebug`,
            async () => {
                let textEditor = vscode.window.activeTextEditor;
                let uri = vscode.window.activeTextEditor?.document.uri;
                if (!uri) {
                    throw new Error("could not get uri for active text editor");
                }
                let folder = vscode.workspace.getWorkspaceFolder(uri);
                if (!folder) {
                    throw new Error(
                        "No workspace root for open files"
                    );
                }

                const options = {
                    name: uri.fsPath,
                    type: extensionName,
                    request: "launch",
                    stopOnEntry: true,
                    yieldSteps: 4096,
                };

                return vscode.debug.startDebugging(folder, options);
            }
        )
    );
}

export function debuggerDeactivate() {
}
