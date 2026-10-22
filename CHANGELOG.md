# Changelog

Notable changes to the Chialisp extension, grouped by released version.

## [1.2.6] - 2026-07-28

### Changed

- Updated build and test dependencies and Marketplace publishing authentication.

[Full release notes](https://github.com/Chia-Network/vscode-chialisp-lsp/releases/tag/1.2.6)

## [1.2.5] - 2026-07-01

### Added

- Support for module-style Chialisp, including imports, exported aliases, go to definition, and a quick fix for locating missing modules.

### Fixed

- Parentheses inside comments are no longer colored as code.
- Live diagnostics for imported functions and name resolution for aliased exports.
- Debugger input framing and continue responses.
- Handling of hex files as program data.

### Changed

- Updated the Chialisp compiler and dependency versions.

[Full release notes](https://github.com/Chia-Network/vscode-chialisp-lsp/releases/tag/1.2.5)

## [1.2.4] - 2025-04-01

### Added

- Validation and descriptions for Chialisp debug launch configurations.
- Support for loading debugger symbols from launch configurations.

### Fixed

- Improved debugger argument retrieval and support for separate symbols and free functions in classic Chialisp.

### Changed

- Updated the Chialisp compiler, build tooling, and dependencies.

[Full release notes](https://github.com/Chia-Network/vscode-chialisp-lsp/releases/tag/1.2.4)

## [1.2.2] - 2024-08-05

### Changed

- Updated dependencies, including semver, word-wrap, and braces.

[Full release notes](https://github.com/Chia-Network/vscode-chialisp-lsp/releases/tag/1.2.2)

## [1.2.1] - 2024-05-17

### Added

- Language support for `assign` bindings, lambda expressions, and dynamically computed rest arguments.

### Fixed

- First-line comment handling.
- False errors for `defconstant` values outside the language server's supported subset.
- Protocol message parsing when input arrives in chunks.

### Changed

- Raised the minimum supported VS Code version to 1.85.
- Updated the Chialisp compiler and language client dependencies.

[Full release notes](https://github.com/Chia-Network/vscode-chialisp-lsp/releases/tag/1.2.1)

## [1.0.0] - 2023-04-20

### Added

- Support for debugging Chialisp programs using the Debug Adapter Protocol.

### Changed

- Updated clvm_tools_rs to 0.1.31 for new compiler features and debug support.

## [0.2.0] - 2023-01-23

### Added

- Syntax highlighting.
- Go to definition.
- Auto-completion.
- Configurable stderr logging for diagnostics.

[1.2.6]: https://github.com/Chia-Network/vscode-chialisp-lsp/compare/1.2.5...1.2.6
[1.2.5]: https://github.com/Chia-Network/vscode-chialisp-lsp/compare/1.2.4...1.2.5
[1.2.4]: https://github.com/Chia-Network/vscode-chialisp-lsp/compare/1.2.2...1.2.4
[1.2.2]: https://github.com/Chia-Network/vscode-chialisp-lsp/compare/1.2.1...1.2.2
[1.2.1]: https://github.com/Chia-Network/vscode-chialisp-lsp/compare/1.0.0...1.2.1
[1.0.0]: https://github.com/Chia-Network/vscode-chialisp-lsp/releases/tag/1.0.0
[0.2.0]: https://github.com/Chia-Network/vscode-chialisp-lsp/releases/tag/0.2.0
