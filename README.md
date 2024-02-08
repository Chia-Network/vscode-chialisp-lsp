# chialisp-lsp-client README

A rust based chialisp language server client delivered with a rust client compiled
to webassembly.  The rust client uses much of the same code as chia's chialisp
compiler, but tries to be more lenient and faster to use incrementally.

## Features

- Highlighting functions, arguments
- Go to definition (if in the same file)
- Completion
- Include files (when the filesystem is accessible)
- Debug adapter

## Requirements

- A 64-bit version of any OS supported by both Chia and Visual Studio Code (Windows, MacOS, and Debian-based Linux distributions are all supported)

- Microsoft Visual Studio Code
  - This is the only editor that is currently supported
  - If you would like to add support for your favorite editor or IDE, PRs are welcome!

## Required to build:

    npm install -g vsce
    cargo install wasm-pack
    rustup target add wasm32-unknown-unknown

## Testing

From the root directory of the repo, run:

    ./test/test.sh

## Extension Settings

For best results, put a chialisp.json file in your workspace root that lists the
directories you'll be including other files from.

{
    "include_paths": ["/home/person/dev/chia-blockchain/chia/wallet/puzzles"]
}

## Known Issues

Currently, the language server is in beta. There are no specific known bugs at this point, but we expect continued updates as bugs are encountered, leading up to a full release.

## Release Notes

### 0.0.1 (2022/09/28)

- Public beta release
- Includes syntax highlighting, go to definition, and auto-completion

---
