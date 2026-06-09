const START_HEADER = 0;
const EOL_RETURN = 1;
const FIRST_EOL = 2;
const MAYBE_SECOND_EOL = 3;
const MESSAGE_READ = 4;

class StdinReader {
    constructor(deliverMsg) {
        this.mode = START_HEADER;
        this.messageHeader = '';
        this.messagePayload = '';
        this.remainingBytes = 0;
        this.deliverMsg = deliverMsg;
    }

    // Process a DAP/LSP-style Content-Length stream. Chunks can split headers,
    // payloads, or contain multiple messages.
    processChunk(log, chunk) {
        for (let i = 0; i < chunk.length; ) {
            let ch = chunk[i];
            let doInc = 1;
            if (this.mode === START_HEADER) {
                if (ch === '\r' || ch === '\n') {
                    let line = this.messageHeader.split(':');
                    this.messageHeader = '';
                    if (line[0].match(/[Cc]ontent-[Ll]ength/) && line.length > 1) {
                        this.remainingBytes = parseInt(line[1].trim());
                    }
                    if (ch === '\r') {
                        this.mode = EOL_RETURN;
                    } else {
                        this.mode = FIRST_EOL;
                    }
                } else {
                    this.messageHeader += ch;
                }
            } else if (this.mode === EOL_RETURN) {
                if (ch === '\n') {
                    this.mode = FIRST_EOL;
                }
            } else if (this.mode === FIRST_EOL) {
                if (ch === '\r') {
                    this.mode = MAYBE_SECOND_EOL;
                } else if (ch === '\n') {
                    this.mode = MESSAGE_READ;
                } else {
                    this.mode = START_HEADER;
                    this.messageHeader += ch;
                }
            } else if (this.mode === MAYBE_SECOND_EOL) {
                if (ch === '\n') {
                    this.mode = MESSAGE_READ;
                }
            } else {
                doInc = 0;
                if (chunk.length - i >= this.remainingBytes) {
                    let message = this.messagePayload + chunk.substr(i, this.remainingBytes);
                    this.messagePayload = '';
                    i += this.remainingBytes;
                    this.remainingBytes = 0;
                    this.mode = START_HEADER;
                    try {
                        if (this.deliverMsg) {
                            this.deliverMsg(message);
                        }
                    } catch (e) {
                        log.write('exception ' + e);
                    }
                } else {
                    this.messagePayload += chunk.substr(i);
                    let canUseBytes = chunk.length - i;
                    this.remainingBytes -= canUseBytes;
                    i += canUseBytes;
                }
            }

            i += doInc;
        }
    }
}

module.exports = {StdinReader};
