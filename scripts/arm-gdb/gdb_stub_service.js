#!/usr/bin/env node
'use strict';

const fs = require('fs');
const path = require('path');
const { spawn } = require('child_process');

function parseArgs(argv) {
    const result = {};
    for (let i = 0; i < argv.length; i++) {
        const key = argv[i];
        if (!key.startsWith('--')) {
            throw new Error(`unexpected argument ${key}`);
        }
        const value = argv[i + 1];
        if (value === undefined || value.startsWith('--')) {
            throw new Error(`missing value for ${key}`);
        }
        result[key.slice(2)] = value;
        i += 1;
    }
    return result;
}

function tomlString(value) {
    return JSON.stringify(value);
}

function writeFileIfChanged(filePath, content) {
    try {
        if (fs.readFileSync(filePath, 'utf8') === content) {
            return;
        }
    } catch (_e) {
        // Fall through to write.
    }

    fs.writeFileSync(filePath, content);
}

function helperCargoToml(clvmArmJitPath) {
    return `[package]
name = "chialisp_arm_gdb_helper"
version = "0.1.0"
edition = "2024"

[dependencies]
chialisp = "0.4.4"
clvmr = "0.16.4"
gdbstub = "0.7.1"
clvm_to_arm_chialisp = { path = ${tomlString(path.join(clvmArmJitPath, 'clvm_to_arm_chialisp'))} }
clvm_to_arm_emulate = { path = ${tomlString(path.join(clvmArmJitPath, 'clvm_to_arm_emulate'))} }
clvm_to_arm_generate = { path = ${tomlString(path.join(clvmArmJitPath, 'clvm_to_arm_generate'))} }
`;
}

function helperMainRs() {
    return `use std::collections::HashMap;
use std::fs;
use std::net::TcpListener;
use std::rc::Rc;

use chialisp::classic::clvm_tools::stages::stage_0::{DefaultProgramRunner, TRunProgram};
use chialisp::compiler::compiler::{compile_file, DefaultCompilerOpts};
use chialisp::compiler::comptypes::CompilerOpts;
use chialisp::compiler::debug::build_symbol_table_mut;
use chialisp::compiler::dialect::AcceptedDialect;
use chialisp::compiler::frontend::frontend;
use chialisp::compiler::sexp::{decode_string, parse_sexp};
use chialisp::compiler::srcloc::Srcloc;
use clvmr::Allocator;

use clvm_to_arm_chialisp::sexp_trait::{CreateChialispSExp, RcSExp, SrclocWrap};
use clvm_to_arm_emulate::emu::Emu;
use clvm_to_arm_emulate::emu_stub::run_stub;
use clvm_to_arm_generate::code::{Program, TARGET_ADDR};

#[derive(Debug)]
struct Args {
    program: String,
    run_args: String,
    elf_out: String,
    port: u16,
}

fn parse_args() -> Result<Args, String> {
    let mut program = None;
    let mut run_args = None;
    let mut elf_out = None;
    let mut port = None;
    let mut iter = std::env::args().skip(1);

    while let Some(key) = iter.next() {
        let value = iter.next().ok_or_else(|| format!("missing value for {key}"))?;
        match key.as_str() {
            "--program" => program = Some(value),
            "--run-args" => run_args = Some(value),
            "--elf-out" => elf_out = Some(value),
            "--port" => {
                port = Some(value.parse::<u16>().map_err(|e| format!("invalid port: {e}"))?)
            }
            _ => return Err(format!("unknown argument {key}")),
        }
    }

    Ok(Args {
        program: program.ok_or_else(|| "missing --program".to_string())?,
        run_args: run_args.ok_or_else(|| "missing --run-args".to_string())?,
        elf_out: elf_out.ok_or_else(|| "missing --elf-out".to_string())?,
        port: port.ok_or_else(|| "missing --port".to_string())?,
    })
}

fn build_elf(program_path: &str, env: &str, elf_out: &str) -> Result<(Vec<u8>, Rc<HashMap<String, String>>), String> {
    let program = fs::read_to_string(program_path)
        .map_err(|e| format!("could not read chialisp program {program_path}: {e}"))?;
    let srcloc = Srcloc::start(program_path);
    let env_parsed = parse_sexp(srcloc.clone(), env.bytes())
        .map_err(|(loc, e)| format!("failed to parse run_args at {loc}: {e}"))?;
    let env_sexp = env_parsed
        .first()
        .ok_or_else(|| "run_args did not contain a CLVM value".to_string())?;

    let mut allocator = Allocator::new();
    let mut symbol_table = HashMap::new();
    let runner: Rc<dyn TRunProgram> = Rc::new(DefaultProgramRunner::new());
    let search_paths = vec![];
    let opts = Rc::new(DefaultCompilerOpts::new(program_path))
        .set_dialect(AcceptedDialect {
            stepping: Some(23),
            strict: true,
            int_fix: true,
            extra_numeric_constants: false,
        })
        .set_optimize(true)
        .set_search_paths(&search_paths)
        .set_frontend_opt(false);

    let parsed_program = parse_sexp(srcloc.clone(), program.bytes())
        .map_err(|(loc, e)| format!("failed to parse chialisp program at {loc}: {e}"))?;
    let fe = frontend(opts.clone(), &parsed_program)
        .map_err(|e| format!("failed to compose frontend program: {e:?}"))?;
    let range_results: HashMap<String, SrclocWrap> = fe
        .compileform()
        .helpers
        .iter()
        .map(|h| (decode_string(h.name()), SrclocWrap(h.loc())))
        .collect();

    let compiled = compile_file(&mut allocator, runner, opts, &program, &mut symbol_table)
        .map_err(|e| format!("failed to compile chialisp program: {e:?}"))?
        .to_sexp();
    build_symbol_table_mut(&mut symbol_table, &compiled);

    let symbols = Rc::new(symbol_table);
    let generator = Program::new::<CreateChialispSExp>(
        range_results,
        program_path,
        elf_out,
        RcSExp(Rc::new(compiled)),
        RcSExp(env_sexp.clone()),
        TARGET_ADDR,
        symbols.clone(),
    )?;

    let elf_data = generator.to_elf(elf_out)?;
    fs::write(elf_out, &elf_data.object_file)
        .map_err(|e| format!("could not write elf output {elf_out}: {e}"))?;

    Ok((elf_data.object_file, symbols))
}

fn main() -> Result<(), String> {
    let args = parse_args()?;
    let (elf_data, symbols) = build_elf(&args.program, &args.run_args, &args.elf_out)?;
    let mut emu = Emu::new(&elf_data, TARGET_ADDR, symbols)
        .map_err(|e| format!("could not create ARM emulator: {e:?}"))?;
    let listener = TcpListener::bind(("127.0.0.1", args.port))
        .map_err(|e| format!("could not bind GDB stub port {}: {e}", args.port))?;

    println!("CHIALISP_GDB_STUB_READY {}", args.port);
    let (stream, addr) = listener
        .accept()
        .map_err(|e| format!("could not accept GDB connection: {e}"))?;
    eprintln!("Debugger connected from {addr}");

    run_stub(Box::new(stream), &mut emu)
}
`;
}

function ensureHelperProject(workspace, clvmArmJitPath) {
    const helperDir = path.join(workspace, '.chialisp-debug', 'arm-gdb-helper');
    const srcDir = path.join(helperDir, 'src');
    fs.mkdirSync(srcDir, { recursive: true });
    writeFileIfChanged(path.join(helperDir, 'Cargo.toml'), helperCargoToml(clvmArmJitPath));
    writeFileIfChanged(path.join(srcDir, 'main.rs'), helperMainRs());
    return helperDir;
}

function runCargoHelper(helperDir, args) {
    const runArgs = [
        'run',
        '--release',
        '--manifest-path',
        path.join(helperDir, 'Cargo.toml'),
        '--',
        '--program',
        args.program,
        '--run-args',
        JSON.parse(args['run-args-json'])[0] || '()',
        '--elf-out',
        args['elf-out'],
        '--port',
        args.port,
    ];

    const child = spawn('cargo', runArgs, {
        cwd: helperDir,
        stdio: ['ignore', 'pipe', 'pipe'],
        env: process.env,
    });

    child.stdout.on('data', (chunk) => process.stdout.write(chunk));
    child.stderr.on('data', (chunk) => process.stderr.write(chunk));
    child.on('error', (err) => {
        console.error(`could not start cargo: ${err.message}`);
        process.exitCode = 1;
    });
    child.on('exit', (code) => {
        process.exit(code || 0);
    });

    process.on('SIGTERM', () => child.kill('SIGTERM'));
    process.on('SIGINT', () => child.kill('SIGINT'));
}

function main() {
    const args = parseArgs(process.argv.slice(2));
    for (const required of ['workspace', 'clvm-arm-jit', 'program', 'run-args-json', 'elf-out', 'port']) {
        if (!args[required]) {
            throw new Error(`missing --${required}`);
        }
    }

    const helperDir = ensureHelperProject(args.workspace, args['clvm-arm-jit']);
    runCargoHelper(helperDir, args);
}

try {
    main();
} catch (e) {
    console.error(e && e.stack ? e.stack : String(e));
    process.exit(1);
}
