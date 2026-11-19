// Paths are quoted even when the template has no spaces. VS Code substitutes
// ${workspaceFolder} after launch.json is written, and cppdbg then splits
// miDebuggerArgs. A check against the unsubstituted text would leave
// "My Programs/puzzle.clsp" as two arguments.

export function quoteArg(value: string): string {
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

export interface ArmGdbLaunchPaths {
    runnerPath: string;
    workspacePath: string;
    programPath: string;
    runArgsJson: string;
    elfPath: string;
    syntheticSourcePath: string;
    gdbPrinterPath: string;
    guestElfPath: string;
    gdbPrinterGuestPath: string;
}

export function buildMiDebuggerArgs(paths: ArmGdbLaunchPaths): string {
    const parts = [
        quoteArg(paths.runnerPath),
        '--gdb-server',
        '127.0.0.1:0',
        '--chialisp-workspace',
        quoteArg(paths.workspacePath),
        '--chialisp-program',
        quoteArg(paths.programPath),
        '--chialisp-run-args-json',
        quoteArg(paths.runArgsJson),
        '--chialisp-elf-out',
        quoteArg(paths.elfPath),
        '--chialisp-synthetic-source-out',
        quoteArg(paths.syntheticSourcePath),
        '--chialisp-gdb-printer',
        quoteArg(paths.gdbPrinterPath),
        '--workspace',
        quoteArg(paths.workspacePath),
        '--import-file',
        quoteArg(paths.elfPath),
        '--import-file',
        quoteArg(paths.syntheticSourcePath),
        '--import-file',
        quoteArg(paths.gdbPrinterPath),
    ];
    const gdbExCommands = [
        `file ${paths.guestElfPath}`,
        `source ${paths.gdbPrinterGuestPath}`,
        'dir /mnt',
        'target remote /dev/ttyS1',
    ];
    for (const command of gdbExCommands) {
        parts.push('--ex', quoteArg(command));
    }
    return parts.join(' ');
}

function stripJsonComments(content: string): string {
    let result = '';
    let inString = false;
    let escaped = false;
    for (let index = 0; index < content.length; index += 1) {
        const char = content[index];
        const next = content[index + 1];
        if (inString) {
            result += char;
            if (escaped) {
                escaped = false;
            } else if (char === '\\') {
                escaped = true;
            } else if (char === '"') {
                inString = false;
            }
            continue;
        }
        if (char === '"') {
            inString = true;
            result += char;
            continue;
        }
        if (char === '/' && next === '/') {
            index += 1;
            while (index + 1 < content.length && content[index + 1] !== '\n') {
                index += 1;
            }
            continue;
        }
        if (char === '/' && next === '*') {
            index += 2;
            while (index < content.length && !(content[index] === '*' && content[index + 1] === '/')) {
                index += 1;
            }
            index += 1;
            continue;
        }
        result += char;
    }
    return result;
}

function stripTrailingCommas(content: string): string {
    let result = '';
    let inString = false;
    let escaped = false;
    for (let index = 0; index < content.length; index += 1) {
        const char = content[index];
        if (inString) {
            result += char;
            if (escaped) {
                escaped = false;
            } else if (char === '\\') {
                escaped = true;
            } else if (char === '"') {
                inString = false;
            }
            continue;
        }
        if (char === '"') {
            inString = true;
            result += char;
            continue;
        }
        if (char === ',') {
            let look = index + 1;
            while (look < content.length && /\s/.test(content[look])) {
                look += 1;
            }
            if (content[look] === '}' || content[look] === ']') {
                continue;
            }
        }
        result += char;
    }
    return result;
}

export function parseLaunchJson(content: string): any {
    return JSON.parse(stripTrailingCommas(stripJsonComments(content)));
}
