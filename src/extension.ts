'use strict';
// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import * as vscode from 'vscode';
import * as path from 'path';
import * as fsBase from 'fs';
import * as os from 'os';

const fs = fsBase.promises;

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
import { languageActivate, languageDeactivate } from './lsp';
import { debuggerActivate, debuggerDeactivate } from './dbg';

// this method is called when your extension is activated
// your extension is activated the very first time the c`ommand is executed
export async function activate(context: vscode.ExtensionContext) {
    // Taken from vscode-cpptools...
    // Register a protocol handler to serve localized versions of the schema for c_cpp_properties.json
    class SchemaProvider implements vscode.TextDocumentContentProvider {
        public async provideTextDocumentContent(uri: vscode.Uri): Promise<string> {
            console.assert(uri.path[0] === '/', "A preceding slash is expected on schema uri path");
            const fileName: string = uri.path.substring(1);
            let extension = vscode.extensions.getExtension('ChiaNetwork.chialisp');
            if (extension) {
                let targetPath = path.join(extension.extensionUri.fsPath, fileName);
                let fileBuffer = await fs.readFile(targetPath);
                return fileBuffer.toString('utf8');
            }

            return new Promise((resolve, reject) => {
                reject(new Error("requested file doesn't seem to exist"));
            });
        }
    }

    vscode.workspace.registerTextDocumentContentProvider('chialisp-schema', new SchemaProvider());
    languageActivate(context);
    debuggerActivate(context);
}

// this method is called when your extension is deactivated
export async function deactivate() {
    languageDeactivate();
    debuggerDeactivate();
}
