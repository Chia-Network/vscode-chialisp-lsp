import * as fs from 'fs';

export class EmptyDisposable {
    dispose() { }
}

export function isspace(ch: string) {
    return (ch === ' ' || ch === '\t' || ch === '\r' || ch === '\n');
}

// Determine if it's an eligible clvm file, hex file, etc.
export const guessFileType = async (path: string) => {
    let fileContent = (await fs.promises.readFile(path)).toString('utf8');
    let kind: any = {};

    if (fileContent.length === 0) {
        return undefined;
    } else {
        // Find the first nonblank
        for (var i = 0; i < fileContent.length; i++) {
            let ch = fileContent[i];

            if (isspace(ch)) {
                continue;
            }

            if (ch === '(') {
                kind = {'chialisp':true, 'clvm':true};
                break;
            }

            if (ch.match(/[0-9a-fA-F]/)) {
                kind = {'hex':true};
                break;
            }
        }

        let gotWord = '';

        for (i += 1; i < fileContent.length; i++) {
            let ch = fileContent[i];

            // Read only the first word after a non-blank.
            if (isspace(ch) || ch === '(') {
                if (gotWord === 'mod') {
                    kind['clvm'] = false;
                    break;
                } else if (gotWord !== '') {
                    kind['chialisp'] = false;
                    break;
                } else {
                    continue;
                }
            }

            gotWord += ch;
        }
    }

    let kindKeys = Object.keys(kind);
    for (var k = 0; k < kindKeys.length; k++) {
        let key = kindKeys[k];
        if (kind[key]) {
            return key;
        }
    }

    // Explicitly no matches.
    return {};
};
