'use strict';
// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import * as vscode from 'vscode';
import * as path from 'path';
import * as os from 'os';

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
    languageActivate(context);
    debuggerActivate(context);
}

// this method is called when your extension is deactivated
export async function deactivate() {
    languageDeactivate();
    debuggerDeactivate();
}
