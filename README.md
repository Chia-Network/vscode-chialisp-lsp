# Chialisp for Visual Studio Code

[![License](https://img.shields.io/github/license/Chia-Network/vscode-chialisp-lsp?logo=Github)](LICENSE)
[![Build](https://github.com/Chia-Network/vscode-chialisp-lsp/actions/workflows/test.yml/badge.svg)](https://github.com/Chia-Network/vscode-chialisp-lsp/actions/workflows/test.yml)

Language support and debugging for Chialisp, powered by the modern
[Chialisp compiler](https://github.com/Chia-Network/chialisp) compiled from Rust to WebAssembly.

## Installation

Install [Chialisp from the Visual Studio Marketplace](https://marketplace.visualstudio.com/items?itemName=ChiaNetwork.chialisp).
The extension is ready to use; no source build or Rust installation is needed.

## Features

- Syntax highlighting for functions and arguments
- Go to definition
- Completion
- Include-file and module support
- Debugging via the Debug Adapter Protocol

## Requirements

- Visual Studio Code 1.85 or later
- A 64-bit operating system supported by both Chia and Visual Studio Code, including Windows, macOS, and Debian-based Linux distributions

Visual Studio Code is currently the only supported editor. Contributions to support other editors are welcome.

## Extension Settings

For best results, put a `chialisp.json` file in your workspace root that lists the
directories containing your include files:

```json
{
    "include_paths": ["/home/person/dev/chia-blockchain/chia/wallet/puzzles"]
}
```

## Building from Source

These steps are for contributors who want to modify or build the extension.
Install Node.js 24, npm, Rust, and Make, then run from the repository root:

```sh
cargo install --version 0.13.1 wasm-pack
rustup target add wasm32-unknown-unknown
npm ci
./node_modules/.bin/vsce package --no-yarn
```

Packaging builds the compiler, language server, and debugger and creates a `.vsix`
file. In VS Code, run **Extensions: Install from VSIX...** to install it.

### Testing

After building the `.vsix`, run the browser integration tests from the repository root:

```sh
DETECT_CHROMEDRIVER_VERSION=true ./test/test.sh
```

The tests require Docker and Chrome, and start code-server on port 8080. On Linux,
run under a display server, such as `xvfb-run -a`, as shown in the
[Test workflow](https://github.com/Chia-Network/vscode-chialisp-lsp/blob/main/.github/workflows/test.yml).
