import * as clvm_tools_rs from '../../wasm/pkg/clvm_tools_lsp.js';
import * as process from 'process';
import * as fs from 'fs';
import { StdinReader } from './stdin_reader.js';

// clean 1:1 8-bit encoding.
process.stdin.setEncoding('binary');

let emptyWriteLog = (line) => { };
var log = {
	write: emptyWriteLog
};

let lsp_id = clvm_tools_rs.create_lsp_service(function(name) {
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
        messages = clvm_tools_rs.lsp_service_handle_msg(lsp_id, m);
    } catch (e) {
        log.write('exn: ' + e + '\n' + e.stack + '\n');
    }

    for (var i = 0; i < messages.length; i++) {
        let inner_ms = JSON.parse(messages[i]);
        for (var j = 0; j < inner_ms.length; j++) {
            let message = JSON.stringify(inner_ms[j]);
            const message_length = Buffer.byteLength(message, 'utf8');
            process.stdout.write(`Content-Length: ${message_length}\r\n\r\n`);
            process.stdout.write(message);
        }
    }
});

process.stdin.on('data', function(chunk) {
    stdinReader.processChunk(log, chunk);
});

process.stdin.on('end', function() {
    clvm_tools_rs.destroy_lsp_service(lsp_id);
    process.stdout.write('', () => { process.exit(0); });
});
