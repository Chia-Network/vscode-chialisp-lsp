let clvm_tools_rs = require('../build/clvm_tools_lsp');
let process = require('process');
let path = require('path');
let fs = require('fs');
let {StdinReader} = require('./stdin_reader');

// clean 1:1 8-bit encoding.
process.stdin.setEncoding('binary');

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
        deliverMessage(JSON.stringify({
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

// When a full message is captured and parsed, it's delivered here.  The API
// entry point receives the message in json format.
//
// We recognize the debug service indicating that it wants the runner to start
// automatically stepping and events that cause it to stop.
function deliverMessage(m) {
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
}

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

function deliverStdinMessage(m) {
    processCommand(JSON.parse(m));
    deliverMessage(m);
}

const stdinReader = new StdinReader(deliverStdinMessage);

process.stdin.on('data', function(chunk) {
    stdinReader.processChunk(log, chunk);
});

process.stdin.on('end', function() {
    log.write('end');
    clvm_tools_rs.destroy_dbg_service(dbg_id);
    process.stdout.write('', () => { process.exit(0); });
});
