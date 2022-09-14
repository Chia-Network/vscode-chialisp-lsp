let clvm_tools_rs = require('../build/clvm_tools_rs.js');
let process = require('process');
let fs = require('fs');

// clean 1:1 8-bit encoding.
process.stdin.setEncoding('binary');

const START_HEADER = 0;
const EOL_RETURN = 1;
const FIRST_EOL = 2;
const MAYBE_SECOND_EOL = 3;
const MESSAGE_READ = 4;

let stdin_reader = {};
stdin_reader.mode = START_HEADER;
stdin_reader.message_header = '';
stdin_reader.message_payload = '';
stdin_reader.remaining_bytes = 0;

process.stdin.on('data', function(chunk) {
    for (var i = 0; i < chunk.length; ) {
        let ch = chunk[i];
        let do_inc = 1;
        if (stdin_reader.mode == START_HEADER) {
            if (ch == '\r' || ch == '\n') {
                let line = stdin_reader.message_header.split(':');
                stdin_reader.message_header = '';
                if (line[0].match(/[Cc]ontent-[Ll]ength/) && line.length > 1) {
                    stdin_reader.remaining_bytes = parseInt(line[1].trim());
                }
                if (ch == '\r') {
                    stdin_reader.mode = EOL_RETURN;
                } else {
                    stdin_reader.mode = FIRST_EOL;
                }
            } else {
                stdin_reader.message_header += ch;
            }
        } else if (stdin_reader.mode == EOL_RETURN) {
            if (ch == '\n') {
                stdin_reader.mode = FIRST_EOL;
            }
        } else if (stdin_reader.mode == FIRST_EOL) {
            if (ch == '\r') {
                stdin_reader.mode = MAYBE_SECOND_EOL;
            } else if (ch == '\n') {
                stdin_reader.mode = MESSAGE_READ;
            } else {
                stdin_reader.mode = START_HEADER;
                stdin_reader.message_header += ch;
            }
        } else if (stdin_reader.mode == MAYBE_SECOND_EOL) {
            if (ch == '\n') {
                stdin_reader.mode = MESSAGE_READ;
            }
        } else { // MESSAGE_READ
            do_inc = 0;
            if (chunk.length >= stdin_reader.remaining_bytes) {
                let message = chunk.substr(i, stdin_reader.remaining_bytes);
                i += stdin_reader.remaining_bytes;
                stdin_reader.remaining_bytes = 0;
                stdin_reader.mode = START_HEADER;
                try {
                    if (stdin_reader.deliver_msg) {
                        stdin_reader.deliver_msg(message);
                    }
                } catch (e) {
                    process.stderr.write('exception ' + e + '\n');
                }
            } else {
                stdin_reader.message_payload += chunk.substr(i);
                let can_use_bytes = chunk.length - i;
                stdin_reader.remaining_bytes -= can_use_bytes;
                i += can_use_bytes; // end
            }
        }

        i += do_inc;
    }
});

let lsp_id = clvm_tools_rs.create_lsp_service(function(name) {
    try {
        return fs.readFileSync(name, 'utf8');
    } catch(e) {
        process.stderr.write('read file failed: ' + e + '\n');
        return null;
    }
}, function(e) {
    process.stderr.write('stderr> ' + e + '\n');
});

process.stdin.on('end', function() {
    clvm_tools_rs.destroy_lsp_service(lsp_id);
    process.exit(0);
});

let awaiting_init_msg = true;
stdin_reader.deliver_msg = function(m) {
    let messages = [];
    try {
        messages = clvm_tools_rs.lsp_service_handle_msg(lsp_id, m);
    } catch (e) {
        process.stderr.write('exn: ' + e + '\n');
    }

    for (var i = 0; i < messages.length; i++) {
        let inner_ms = JSON.parse(messages[i]);
        for (var j = 0; j < inner_ms.length; j++) {
            let message = JSON.stringify(inner_ms[j]);
            process.stdout.write('Content-Length: ' + message.length + '\r\n\r\n');
            process.stdout.write(message);
        }
    }
};
