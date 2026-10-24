# Changelog

Changes in released versions of the Chialisp extension.

## [1.2.7] - 2026-10-23

### Fixed

- Fixed false errors in modules using macros, exports, and generated helpers.
- Fixed completion crashes and missing suggestions in nested bindings.
- Preserved unsaved changes in imported files and refreshed diagnostics when dependencies change.
- Reported include and import cycles without crashing.
- Fixed highlighting and navigation positions around Unicode characters.
- Fixed language server message handling and shutdown.
- Fixed highlighting around strings and parentheses in comments.
- Preserved existing settings when adding include paths and limited include-path fixes to missing dependencies.

### Changed

- Updated the Marketplace README and changelog.
- Updated dependencies and build tools.

## [1.2.6] - 2026-07-28

### Changed

- Updated build and test dependencies.
- Switched Marketplace publishing to OpenID Connect authentication.

[Full release notes](https://github.com/Chia-Network/vscode-chialisp-lsp/releases/tag/1.2.6)

## [1.2.5] - 2026-07-01

### Added

- Support for Chialisp modules, including imports, exported aliases, and go to definition.
- A quick fix to locate missing modules.

### Fixed

- Parentheses inside comments are no longer colored as code.
- Fixed live diagnostics for imported functions and name resolution for aliased exports.
- Fixed debugger message parsing and responses to continue commands.
- Fixed the handling of hex files as program data.

### Changed

- Updated the Chialisp compiler and dependencies.

[Full release notes](https://github.com/Chia-Network/vscode-chialisp-lsp/releases/tag/1.2.5)

## [1.2.4] - 2025-04-01

### Added

- Validation and help text for Chialisp debug launch configurations.
- Support for loading debugger symbols from launch configurations.

### Fixed

- Fixed debugger argument retrieval.
- Improved debugging of classic Chialisp programs that use separate symbol files or free functions.

### Changed

- Updated the Chialisp compiler, build tools, and dependencies.

[Full release notes](https://github.com/Chia-Network/vscode-chialisp-lsp/releases/tag/1.2.4)

## [1.2.2] - 2024-08-05

### Changed

- Updated dependencies, including semver, word-wrap, and braces.

[Full release notes](https://github.com/Chia-Network/vscode-chialisp-lsp/releases/tag/1.2.2)

## [1.2.1] - 2024-05-17

### Added

- Language support for `assign` bindings, lambda expressions, and dynamically computed rest arguments.

### Fixed

- Fixed comment handling on the first line of a file.
- Stopped reporting errors for valid `defconstant` values the language server cannot evaluate.
- Fixed protocol message parsing when input arrives in chunks.

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
- Code completion.
- Configurable stderr logging for diagnostics.

[1.2.7]: https://github.com/Chia-Network/vscode-chialisp-lsp/compare/1.2.6...1.2.7
[1.2.6]: https://github.com/Chia-Network/vscode-chialisp-lsp/compare/1.2.5...1.2.6
[1.2.5]: https://github.com/Chia-Network/vscode-chialisp-lsp/compare/1.2.4...1.2.5
[1.2.4]: https://github.com/Chia-Network/vscode-chialisp-lsp/compare/1.2.2...1.2.4
[1.2.2]: https://github.com/Chia-Network/vscode-chialisp-lsp/compare/1.2.1...1.2.2
[1.2.1]: https://github.com/Chia-Network/vscode-chialisp-lsp/compare/1.0.0...1.2.1
[1.0.0]: https://github.com/Chia-Network/vscode-chialisp-lsp/releases/tag/1.0.0
[0.2.0]: https://github.com/Chia-Network/vscode-chialisp-lsp/releases/tag/0.2.0
