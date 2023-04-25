# README

A rust based Chialisp language server client delivered with a rust client compiled
to webassembly (wasm).  This extension uses the modern Chialisp compiler from `clvm_tool_rs` (https://github.com/Chia-Network/clvm_tools_rs).

To use as a Visual Studio Code extension, simply install the extension from the extension marketplace
https://marketplace.visualstudio.com/items?itemName=ChiaNetwork.chialisp

## Features

- Highlighting functions, arguments
- Go to definition (if in the same file)
- Completion
- Include files (when the filesystem is accessible)
- Debugging support via the Debug Adapter Protocol

## Requirements

- A 64-bit version of any OS supported by both Chia and Visual Studio Code (Windows, MacOS, and Debian-based Linux distributions are all supported)

- Microsoft Visual Studio Code
  - This is the only editor that is currently supported
  - If you would like to add support for your favorite editor or IDE, PRs are welcome!

## Extension Settings

For best results, put a chialisp.json file in your workspace root that lists the
directories you'll be including other files from.

{
    "include_paths": ["/home/person/dev/chia-blockchain/chia/wallet/puzzles"]
}

## Manual Building Steps:

This is not required to use the extension, as it is available fully compiled and ready to use directly from the vscode extension marketplace. However, if you wish to make code changes or want to manually build this extension, you can follow the below steps:

    npm install -g vsce
    cargo install wasm-pack
    rustup target add wasm32-unknown-unknown


---
