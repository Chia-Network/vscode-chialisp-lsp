import * as clvm_tools_rs from '../../wasm/pkg/clvm_tools_lsp.js';
import * as process from 'process';
import * as fs from 'fs';
import { StdinReader } from './stdin_reader.js';

const emptyWriteLog = (line) => { };
const log = {
	write: emptyWriteLog
};

const lspId = clvm_tools_rs.create_lsp_service(function(name) {
    try {
        return fs.readFileSync(name, 'utf8');
    } catch(e) {
        log.write('read file failed: ' + e + '\n');
        return null;
    }
}, function(e) {
    log.write('stderr> ' + e + '\n');
});

let shutdownRequested = false;

function sendMessage(value) {
    const message = JSON.stringify({jsonrpc: '2.0', ...value});
    process.stdout.write(`Content-Length: ${Buffer.byteLength(message, 'utf8')}\r\n\r\n${message}`);
}

function processCommand(cmd) {
    if (cmd.method === 'workspace/didChangeConfiguration') {
        if (cmd.params.settings && cmd.params.settings && cmd.params.settings.chialisp) {
            let stderrPath = cmd.params.settings.chialisp.stderrLogPath;
            if (typeof stderrPath === 'string' && stderrPath !== '') {
		            if (!stderrPath.startsWith('/')) {
			              stderrPath = '/tmp/' + stderrPath;
		            }
                    log.write = (line) => {
                    try {
                        fs.appendFileSync(stderrPath, line + '\n');
                    } catch (e) {
                        ; // Can't do much.
                    }
                };
            } else {
                log.write = emptyWriteLog;
            }
        }
    }
    log.write(JSON.stringify(cmd));
}

const stdinReader = new StdinReader((m) => {
    let command;
    try {
        // Check for various commands we need to interpret at this layer.
        command = JSON.parse(m);
        processCommand(command);
    } catch (e) {
        log.write('local exn: ' + e + '\n' + e.stack + '\n');
        sendMessage({id: null, error: {code: -32700, message: 'Invalid JSON'}});
        return;
    }
    if (command.method === 'exit') {
        clvm_tools_rs.destroy_lsp_service(lspId);
        process.stdout.write('', () => process.exit(shutdownRequested ? 0 : 1));
        return;
    }

    // Process the message in text form, sending it to the lsp.
    let messages = [];
    try {
        messages = clvm_tools_rs.lsp_service_handle_msg(lspId, m);
    } catch (e) {
        log.write('exn: ' + e + '\n' + e.stack + '\n');
        if (command.id !== undefined) {
            sendMessage({id: command.id, error: {code: -32603, message: 'Language service failed'}});
        }
    }

    for (let i = 0; i < messages.length; i++) {
        const innerMs = JSON.parse(messages[i]);
        for (let j = 0; j < innerMs.length; j++) {
            const reply = innerMs[j];
            if (command.method === 'shutdown' && reply.id === command.id && !reply.error) {
                shutdownRequested = true;
            }
            sendMessage(reply);
        }
    }
});

process.stdin.on('data', function(chunk) {
    stdinReader.processChunk(log, chunk);
});

process.stdin.on('end', function() {
    clvm_tools_rs.destroy_lsp_service(lspId);
    process.stdout.write('', () => { process.exit(0); });
});
