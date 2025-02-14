import * as vscode from 'vscode';
import * as path from 'path';
import * as os from 'os';
import {ourExtension} from './constants';

import {
    ExecutableOptions,
    LanguageClient,
    LanguageClientOptions,
    Logger,
    RevealOutputChannelOn,
    ServerOptions,
    StaticFeature,
    TransportKind,
} from 'vscode-languageclient/node';
import { TextDecoder, TextEncoder } from 'util';

var usingLangClient: LanguageClient|undefined = undefined;

export class WorkaroundFeature implements StaticFeature {
    fillClientCapabilities(capabilities: any): void {
        capabilities.workspace.workspaceEdit = { documentChanges: true };
    }
    register() {
    }
    unreigster() {
    }
    initialize(): void {
    }
    dispose(): void {
    }
    getState(): any {
        return {};
    }
}

async function activateServer(context: vscode.ExtensionContext) {
    const workspaceClientInstanceId = 'chialisp';
    const outputChannel: vscode.OutputChannel = vscode.window.createOutputChannel(workspaceClientInstanceId);
    var ourExtensionPath = vscode.extensions.getExtension(ourExtension)?.extensionPath;

    if (!ourExtensionPath) {
        // XXX Report error
        return;
    }

    var serverExecutable = 'node';
    var serverArgs: string[] = [ourExtensionPath + "/runner/src/runner.js"];
    var debugArgs = serverArgs;

    if (process.env.CHIALISP_LSP) {
        serverExecutable = process.env.CHIALISP_LSP;
        serverArgs = [];
        debugArgs = [];
    }

    const serverOptions: ServerOptions = {
        run: {module: path.join(ourExtensionPath, "runner/build/runner.js")},
        debug: {module: path.join(ourExtensionPath, "runner/build/runner.js")}
    };

    const fileToExaminePattern = '**/*';
    const clientOptions: LanguageClientOptions = {
        // This constrains when the lsp receives a notification that a file should be looked at.
        documentSelector: [{scheme: 'file', language: 'chialisp'}],
        // We can use a configuration setting to indicate workspace level
        // include dirs.
        synchronize: {
            configurationSection: 'chialisp',
            fileEvents: vscode.workspace.createFileSystemWatcher('**/*')
        },

        diagnosticCollectionName: workspaceClientInstanceId,
        revealOutputChannelOn: RevealOutputChannelOn.Never,
        outputChannel,
        outputChannelName: workspaceClientInstanceId,
        middleware: {
            // We'll have to learn what goes here
        },
        workspaceFolder: undefined,
        initializationOptions: {
            customData: 'here it is'
        }
    };

    const langClient = new LanguageClient('chialisp', workspaceClientInstanceId, serverOptions, clientOptions);
    langClient.registerFeature(new WorkaroundFeature());
    langClient.registerProposedFeatures();
    langClient.start();

    // Share with deactivate
    usingLangClient = langClient;
}

export function languageActivate(context: vscode.ExtensionContext) {
    // As a way of allowing the extension to be used without editing chialisp.json
    // which was requested, this flow is used:
    //
    // The extension flags missing includes and includes a code action which
    // invokes chialisp.locateIncludePath when selected from the hotfix list.
    //
    // When selected, it brings up a file chooser to find chialisp include files.
    //
    // The directory containing the selected file is added to a chialisp.json in
    // the root of the workspace, where we look for chialisp.json.  When this
    // file appears in the set of files modified in workspace to the extension,
    // it's also re-read in there.
    vscode.commands.registerCommand("chialisp.locateIncludePath", function(value) {
        console.log('locateIncludePath', JSON.stringify(value));
        // Open a file selector to find the included file.
        vscode.window.showOpenDialog({
            filters: {
                // This is part of the user interface
                // eslint-disable-next-line @typescript-eslint/naming-convention
                "Chialisp Include Files": ["clinc","clib","clvm","clsp"]
            }
        }).then((uriList) => {
            if (!uriList || uriList.length === 0) {
                return;
            }

            // Find out which uris are in the workspace
            const u = uriList[0];
            const workspace = vscode.workspace.getWorkspaceFolder(u);
            if (workspace) {
                return {uri: u, workspace, relative: vscode.workspace.asRelativePath(u)};
            } else {
                return {uri: u};
            }
        }).then((useFile) => {
            if (!useFile) {
                return;
            }

            const updateChialispJson = (chialispJson: any, newFile: vscode.Uri) => {
                let targetDirectory = path.dirname(newFile.fsPath);
                let relativePath = vscode.workspace.asRelativePath(newFile);

                if (relativePath) {
                    if (!path.isAbsolute(relativePath)) {
                        targetDirectory = "./" + path.dirname(relativePath);
                    }
                }

                // Try to enforce correct structure.
                if (chialispJson.include_paths === undefined || !chialispJson.include_paths.length) {
                    chialispJson.include_paths = [];
                }

                // Try not to duplicate.
                for (var i = 0; i < chialispJson.include_paths; i++) {
                    if (targetDirectory === chialispJson.include_paths[i]) {
                        return;
                    }
                }

                // We have a new include path to put in.
                chialispJson.include_paths.push(targetDirectory);

                return chialispJson;
            };

            const dec = new TextDecoder("utf-8");
            const enc = new TextEncoder();

            var chialispJsonUri: vscode.Uri | undefined;
            var treatAndWriteBackChialispJson: (oldChialispJson: any) => Thenable<void>;

            function treatAndWriteFile(chialispJsonUri: vscode.Uri, useFileUri: vscode.Uri) {
                return (oldChialispJson: any) => {
                    const newChialispJson = updateChialispJson(oldChialispJson, useFileUri);
                    return vscode.workspace.fs.writeFile(chialispJsonUri, enc.encode(JSON.stringify(newChialispJson)));
                };
            };

            let activeTextUri = vscode.window.activeTextEditor?.document.uri;
            let workspaceUri = useFile.workspace ? useFile.workspace.uri : activeTextUri ? vscode.workspace.getWorkspaceFolder(activeTextUri)?.uri : undefined;
            if (workspaceUri) {
                chialispJsonUri = vscode.Uri.joinPath(workspaceUri, "chialisp.json");
                treatAndWriteBackChialispJson = treatAndWriteFile(chialispJsonUri, useFile.uri);
            } else {
                vscode.window.showErrorMessage("Could not identify workspace folder root");
                return;
            }

            return vscode.workspace.fs.readFile(chialispJsonUri).then((filedata) => {
                try {
                    const oldChialispJson = JSON.parse(dec.decode(filedata));
                    return treatAndWriteBackChialispJson(oldChialispJson);
                } catch (e) {
                    vscode.window.showErrorMessage("Could not update existing chialisp.json");
                }
            }, (e: any) => {
                try {
                    const oldChialispJson = {include_paths:[]};
                    return treatAndWriteBackChialispJson(oldChialispJson);
                } catch (e) {
                    vscode.window.showErrorMessage("Could not formulate chialisp.json");
                }
            });
        }).then(() => {
            let editor = vscode.window.activeTextEditor;
            if (editor) {
                return editor.edit((builder) => {
                    builder.insert(new vscode.Position(0, 0), " ");
                }).then(() => {
                    return editor?.edit((builder) => {
                        builder.delete(new vscode.Range(new vscode.Position(0, 0), new vscode.Position(0, 1)));
                    });
                });
            }
        }, (e) => {
            vscode.window.showErrorMessage(`Could not write chialisp.json: ${e}`);
        });
    }, context);

    activateServer(context);
}

export function languageDeactivate() {
    // Stop service
    if (usingLangClient) {
        usingLangClient.stop();
    }
}
