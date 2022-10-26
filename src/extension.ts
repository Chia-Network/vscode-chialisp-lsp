'use strict';
// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import * as vscode from 'vscode';
import * as path from 'path';
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

const ourExtension = "ChiaNetwork.chialisp";
var usingLangClient: LanguageClient|undefined = undefined;

export class WorkaroundFeature implements StaticFeature {
    fillClientCapabilities(capabilities: any): void {
        capabilities.workspace.workspaceEdit = { documentChanges: true };
    }
    initialize(): void {
    }
    dispose(): void {
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
    var serverArgs: string[] = [ourExtensionPath + "/runner/build/runner.js"];
    var debugArgs = serverArgs;

    if (process.env.CHIALISP_LSP) {
        serverExecutable = process.env.CHIALISP_LSP;
        serverArgs = [];
        debugArgs = [];
    }

    // Register associations
    const exeOptions: ExecutableOptions = {
        cwd: ourExtensionPath,
        env: { ... process.env }
    };

    const serverOptions: ServerOptions = {
        run: {command: serverExecutable, transport: TransportKind.stdio, args: serverArgs, options: exeOptions},
        debug: {command: serverExecutable, transport: TransportKind.stdio, args: debugArgs, options: exeOptions}
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

// this method is called when your extension is activated
// your extension is activated the very first time the c`ommand is executed
export async function activate(context: vscode.ExtensionContext) {
    activateServer(context);
}

// this method is called when your extension is deactivated
export async function deactivate() {
    // Stop service
    if (usingLangClient) {
        await usingLangClient.stop();
    }
}
