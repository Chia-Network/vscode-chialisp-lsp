import * as clvm_tools_rs from '../../wasm/pkg/clvm_tools_lsp.js';
import * as process from 'process';
import * as fs from 'fs';
import { StdinReader } from './stdin_reader.js';

// clean 1:1 8-bit encoding.
process.stdin.setEncoding('binary');

const emptyWriteLog = (line) => { };
const log = {
	write: emptyWriteLog
};

let lspId = clvm_tools_rs.create_lsp_service(function(name) {
    try {
        return fs.readFileSync(name, 'utf8');
    } catch(e) {
        log.write('read file failed: ' + e + '\n');
        return null;
    }
}, function(e) {
    log.write('stderr> ' + e + '\n');
});

function processCommand(cmd) {
    if (cmd.method === 'workspace/didChangeConfiguration') {
        if (cmd.params.settings && cmd.params.settings && cmd.params.settings.chialisp) {
            let stderrPath = cmd.params.settings.chialisp.stderrLogPath;
            if (stderrPath !== '') {
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
    try {
        // Check for various commands we need to interpret at this layer.
        processCommand(JSON.parse(m));
    } catch (e) {
        log.write('local exn: ' + e + '\n' + e.stack + '\n');
    }

    // Process the message in text form, sending it to the lsp.
    let messages = [];
    try {
        messages = clvm_tools_rs.lsp_service_handle_msg(lspId, m);
    } catch (e) {
        log.write('exn: ' + e + '\n' + e.stack + '\n');
    }

    for (let i = 0; i < messages.length; i++) {
        const innerMs = JSON.parse(messages[i]);
        for (let j = 0; j < innerMs.length; j++) {
            const message = JSON.stringify(innerMs[j]);
            const messageLength = Buffer.byteLength(message, 'utf8');
            process.stdout.write(`Content-Length: ${messageLength}\r\n\r\n`);
            process.stdout.write(message);
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
