import * as vscode from 'vscode'
import {extensionName} from './constants'

export const logger = vscode.window.createOutputChannel(extensionName);
export const logWithLevel = (level: any, text: string) => {
    logger.appendLine(`${level}: ${text}`);
    logger.show();
};
export const stringify = (json: any) => { return JSON.stringify(json); };
export const log = {
    info: (text: string) => logWithLevel("info", text),
    error: (text: string) => logWithLevel("error", text)
};
