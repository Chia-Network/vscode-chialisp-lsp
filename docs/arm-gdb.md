# ARM GDB debugging

The extension can compile a Chialisp program to an ARM ELF and debug it with GDB.
`Chialisp: Prepare ARM GDB Chialisp Debug` (`chialisp.prepareArmGdbDebug`) writes a
`cppdbg` launch configuration. Starting that configuration builds the ELF, starts
the GDB stub, and connects GDB.

## Requirements

- The extension build, which includes the wasm package (`make`, or
  `cd wasm && wasm-pack build --release --target nodejs`).
- GDB with ARM support. On Debian and Ubuntu this is the `gdb-multiarch` package.
  macOS and Windows were not exercised by the automated session described below.
- Rust 1.85 or newer when building from source. The pinned `clvm_arm_jit` crates
  use Rust edition 2024. The extension CI toolchain is current stable Rust.

When `gdb-multiarch` is on `PATH`, the launcher uses it and does not download
anything at debug time. When it is absent, the launcher clones
`https://github.com/prozacchiwawa/web-gdb-node.git` at commit
`f78f8ed2cdecf8825958ff73c07ac90be92cf7e4`, checks out that commit detached, and
initializes its submodules. That fallback is a network fetch at debug time.
Install `gdb-multiarch` to avoid it.

## Usage

1. Open a `.clsp` or `.clvm` file in a workspace.
2. Run **Chialisp: Prepare ARM GDB Chialisp Debug**.
3. If `chialisp.json` has no `run_args`, enter the CLVM environment value
   (for example `(5)` or `()`). The command stores it as a one-element array.
4. Start the new **Chialisp ARM GDB** launch configuration.

The command prepares:

- `.chialisp-debug/`, ignored by git, including a small Node wrapper that runs
  the launcher with the editor's JavaScript runtime.
- `.vscode/launch.json`, updated in place. Existing configurations with other
  names are left as they are.
- On launch, `.chialisp-debug/build/<program>.arm.elf` and a sibling
  `<program>.arm.elf.clsp` synthetic source file.

`run_args` is an array of strings in `chialisp.json`. The ARM debug run uses
the first element as the CLVM argument environment. Include directories in
`include_paths` are passed to the compiler.

The launch commands load `wasm/resources/gdb_print_sexp.py`. GDB then prints
values of the generated `sexp` / `word *` types as CLVM S-expressions.
The generated code for a helper such as `F` is in a `.text.F` section. That
section opens with `push {fp, lr}` and contains a NOP anchor (`add r0, r0, #0`)
before later lines in the function. The `defun` keyword line itself may not
have a code address. Expression lines in the original file do.

Symbol-table keys are content hashes. A value may be a source location or a
helper name such as `F`. This generator pin also emits a DWARF subprogram
named `F`, but that subprogram's address range is not the `.text.F` prologue.
`break F` can therefore stop somewhere other than the prologue. The smoke test
breaks at the `.text.F` anchor, which is the named function's entry instruction.

## Dependency pin

`wasm/third_party/clvm_arm_jit` is a snapshot of revision
`eb397028ec00d4604a508eb8eac64a12d6cb07b7`. That revision contains the
named-function NOP anchor
(`60425c4de68b39eab8b762ffa85c0f1977e1ff6a`) and matches the generator and stub
calls this extension makes: the environment is compiled into the ELF, and
`ElfObject` returns synthetic source. The snapshot's own manifests request
`target-lexicon` 0.13, the copy already used by this crate. `wasm/third_party/faerie`
is faerie 0.16.0 with `indexmap` 2, `scroll` 0.12, and `goblin` 0.8 so the
build does not add `syn` 1.0.109 or `indexmap` 1.9.3.

Tag `v0.1.0` (`3da06491cadff7e5307e9226410b116e9938cf05`) is not a compatible
replacement. It moves the environment to stub creation, changes `Program::new`,
and no longer returns synthetic source. `chialisp` stays on 0.4.4. `clvmr` is
requested as 0.16.0 and resolves to the 0.16.4 required by the pinned JIT.

`wasm-opt` stays enabled. The smoke test below runs against
`wasm-pack build --release`, which optimizes the wasm package. The ARM ELF is
produced by that package at runtime.

## Tests

From the repository root, with Rust 1.85+ and the wasm32 target:

```sh
cd wasm && cargo test elf
node --test scripts/arm-gdb/launcher.test.js
cd wasm && wasm-pack build --release --target nodejs && cd ..
node scripts/arm-gdb/smoke_test.js
```

`cargo test elf` builds an ELF in a temporary directory. It checks that
`.text.F` starts with the function prologue, that the NOP anchor is in that
prologue, that a statement row covers the entry, and that a later line follows
the anchor. It also checks `ArmGdbCompilerOpts` file reads for an absolute
path, an include path, and a missing include. A non-function `file_reader` is
rejected by the wasm export. `smoke_test.js` asserts that JS error, then
connects `gdb-multiarch` to the stub, continues to the `.text.F` anchor, steps
one instruction, and checks that `info args` shows a pretty-printed CLVM value.

The smoke test exits with an error when `gdb-multiarch` or the wasm package is
missing. It does not skip quietly.
