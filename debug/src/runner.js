let clvm_tools_rs = require('../build/clvm_tools_lsp');
let process = require('process');
let path = require('path');
let fs = require('fs');

// clean 1:1 8-bit encoding.
process.stdin.setEncoding('binary');

const START_HEADER = 0;
const EOL_RETURN = 1;
const FIRST_EOL = 2;
const MAYBE_SECOND_EOL = 3;
const MESSAGE_READ = 4;

var workspaceFolder = process.env.WORKSPACE_FOLDER ? process.env.WORKSPACE_FOLDER : ".";

let emptyWriteLog = (line) => { };
var log = {
	write: emptyWriteLog
};

function tryOpenFile(name,tryDir) {
    let nameWithPath = (tryDir === null) ? name : path.join(tryDir,name);
    try {
        return fs.readFileSync(nameWithPath, 'utf8');
    } catch(e) {
        return null;
    }
}

let dbg_id = clvm_tools_rs.create_dbg_service(function(name) {
    const directoryTryList = [workspaceFolder, ".", null];
    for (let i = 0; i < directoryTryList.length; i++) {
        const fileContent = tryOpenFile(name, directoryTryList[i]);
        if (fileContent !== null) {
            return fileContent;
        }
        log.write(`read ${name} in ${directoryTryList[i]} failed`);
    }
    return null;
}, function(e) {
    log.write('stderr> ' + e + '\n');
});

var stepper_tick = undefined;
let stdin_reader = {};

// We don't have threads in wasm so we depend on a ticker at this layer to cause
// the debugger to auto-step, keeping control coming from one consistent source.
//
// This also allows us to process pause and stop messages from the consumer
// without needing complexity at this level.
function startStepper() {
    if (stepper_tick !== undefined) {
        return;
    }

    stepper_tick = setInterval(() => {
        stdin_reader.deliver_msg(JSON.stringify({
            seq:0,
            type:"request",
            command:"stepIn",
            arguments:{"threadId":-1}
        }));
    }, 0);
}

// Recognize that we've got a stepper interval set and ensure that it gets removed.
// Without that interval ticker, the debugger will idle.
function stopStepper() {
    if (stepper_tick === undefined) {
        return;
    }

    clearInterval(stepper_tick);
    stepper_tick = undefined;
}

stdin_reader.mode = START_HEADER;
stdin_reader.message_header = '';
stdin_reader.message_payload = '';
stdin_reader.remaining_bytes = 0;
// When a full message is captured and parsed, it's delivered here.  The API
// entry point receives the message in json format.
//
// We recognize the debug service indicating that it wants the runner to start
// automatically stepping and events that cause it to stop.
stdin_reader.deliver_msg = function(m) {
    let messages = [];
    try {
        log.write(`input msg ${m}`);
        messages = clvm_tools_rs.dbg_service_handle_msg(dbg_id, m);
    } catch (e) {
        log.write('exn: ' + e + '\n');
    }

    for (var i = 0; i < messages.length; i++) {
        let inner_ms = JSON.parse(messages[i]);
        for (var j = 0; j < inner_ms.length; j++) {
            let json = inner_ms[j];
            let message = JSON.stringify(json);
            if (typeof json.message === 'string' && json.message.startsWith('run')) {
                // We return a message if we want to start stepping.
                startStepper();
            } else if (json.type === 'event' && json.event === 'stopped') {
                stopStepper();
            }
            log.write(`output message ${message}`);
            process.stdout.write('Content-Length: ' + message.length + '\r\n\r\n');
            process.stdout.write(message);
        }
    }
};

function processCommand(cmd) {
    log.write(`process command ${JSON.stringify(cmd)}`);
    if (cmd.method === 'workspace/didChangeConfiguration') {
        if (cmd.params.settings && cmd.params.settings && cmd.params.settings.chialisp) {
            let stderrPath = cmd.params.settings.chialisp.stderrLogPath;
            if (stderrPath !== '') {
                log.write = (line) => {
                    fs.appendFileSync(stderrPath, line + '\n');
                };
            } else {
                log.write = emptyWriteLog;
            }
        }
    }
    log.write(JSON.stringify(cmd));
}

// Fairly complicated but the goal here is to implement a state machine that
// parses the http-header-like message protocol used by the debug adapter and
// lsp.  Ultimately, the goal is to call processCommand 
process.stdin.on('data', function(chunk) {
    for (var i = 0; i < chunk.length; ) {
        let ch = chunk[i];
        let do_inc = 1;
        if (stdin_reader.mode === START_HEADER) {
            if (ch === '\r' || ch === '\n') {
                let line = stdin_reader.message_header.split(':');
                stdin_reader.message_header = '';
                if (line[0].match(/[Cc]ontent-[Ll]ength/) && line.length > 1) {
                    stdin_reader.remaining_bytes = parseInt(line[1].trim());
                }
                if (ch === '\r') {
                    stdin_reader.mode = EOL_RETURN;
                } else {
                    stdin_reader.mode = FIRST_EOL;
                }
            } else {
                stdin_reader.message_header += ch;
            }
        } else if (stdin_reader.mode == EOL_RETURN) {
            if (ch === '\n') {
                stdin_reader.mode = FIRST_EOL;
            }
        } else if (stdin_reader.mode == FIRST_EOL) {
            if (ch === '\r') {
                stdin_reader.mode = MAYBE_SECOND_EOL;
            } else if (ch === '\n') {
                stdin_reader.mode = MESSAGE_READ;
            } else {
                stdin_reader.mode = START_HEADER;
                stdin_reader.message_header += ch;
            }
        } else if (stdin_reader.mode == MAYBE_SECOND_EOL) {
            if (ch === '\n') {
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
                        processCommand(JSON.parse(message));
                        stdin_reader.deliver_msg(message);
                    }
                } catch (e) {
                    log.write('exception ' + e);
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

process.stdin.on('end', function() {
    log.write('end');
    clvm_tools_rs.destroy_dbg_service(dbg_id);
    process.stdout.write('', () => { process.exit(0); });
});
