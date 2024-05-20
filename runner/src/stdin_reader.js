const START_HEADER = 0;
const EOL_RETURN = 1;
const FIRST_EOL = 2;
const MAYBE_SECOND_EOL = 3;
const MESSAGE_READ = 4;

export class StdinReader {
    constructor(deliverMsg) {
        this.mode = START_HEADER;
        this.message_header = '';
        this.message_payload = '';
        this.remaining_bytes = 0;
        this.deliverMsg = deliverMsg;
    }

    // Process a chunk coming in from stdin.  It may be misaligned due to
    // os level buffer limits so we need to be prepared to deal with fragments.
    processChunk(log, chunk) {
        for (let i = 0; i < chunk.length; ) {
            let ch = chunk[i];
            let doInc = 1;
            if (this.mode === START_HEADER) {
                if (ch === '\r' || ch === '\n') {
                    let line = this.message_header.split(':');
                    this.message_header = '';
                    if (line[0].match(/[Cc]ontent-[Ll]ength/) && line.length > 1) {
                        this.remaining_bytes = parseInt(line[1].trim());
                    }
                    if (ch === '\r') {
                        this.mode = EOL_RETURN;
                    } else {
                        this.mode = FIRST_EOL;
                    }
                } else {
                    this.message_header += ch;
                }
            } else if (this.mode === EOL_RETURN) {
                if (ch === '\n') {
                    this.mode = FIRST_EOL;
                }
            } else if (this.mode == FIRST_EOL) {
                if (ch === '\r') {
                    this.mode = MAYBE_SECOND_EOL;
                } else if (ch === '\n') {
                    this.mode = MESSAGE_READ;
                } else {
                    this.mode = START_HEADER;
                    this.message_header += ch;
                }
            } else if (this.mode === MAYBE_SECOND_EOL) {
                if (ch === '\n') {
                    this.mode = MESSAGE_READ;
                }
            } else { // MESSAGE_READ
                doInc = 0;
                if (chunk.length - i >= this.remaining_bytes) {
                    let message = this.message_payload + chunk.substr(i, this.remaining_bytes);
                    this.message_payload = '';
                    i += this.remaining_bytes;
                    this.remaining_bytes = 0;
                    this.mode = START_HEADER;
                    try {
                        if (this.deliverMsg) {
                            this.deliverMsg(message);
                        }
                    } catch (e) {
                        log.write('exception ' + e);
                    }
                } else {
                    this.message_payload += chunk.substr(i);
                    let canUseBytes = chunk.length - i;
                    this.remaining_bytes -= canUseBytes;
                    i += canUseBytes; // end
                }
            }

            i += doInc;
        }
    }
};
