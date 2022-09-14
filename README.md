# chialisp-lsp-client README

A rust based chialisp language server client delivered with a rust client compiled
to webassembly.  The rust client uses much of the same code as chia's chialisp
compiler, but tries to be more lenient and faster to use incrementally.

## Features

- Highlighting functions, arguments
- Go to definition (if in the same file)
- Completion
- Include files (when the filesystem is accessible)

## Requirements

## Extension Settings

For best results, put a chialisp.json file in your workspace root that lists the
directories you'll be including other files from.

{
    "include_paths": ["/home/person/dev/chia-blockchain/chia/wallet/puzzles"]
}

## Known Issues

Calling out known issues can help limit users opening duplicate issues against your extension.

This is experimental and may need tuning and bug fixing at this point, but should
be of assistance.

## Release Notes

Users appreciate release notes as you update your extension.

### 1.0.0

Initial alpha release.

---
