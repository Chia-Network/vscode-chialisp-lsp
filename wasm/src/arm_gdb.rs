use std::cell::RefCell;
use std::collections::HashMap;
use std::io;
use std::mem::swap;
use std::rc::Rc;

use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;

use chialisp::classic::clvm_tools::stages::stage_0::{DefaultProgramRunner, TRunProgram};
use chialisp::compiler::compiler::{compile_file, DefaultCompilerOpts};
use chialisp::compiler::comptypes::CompilerOpts;
use chialisp::compiler::debug::build_symbol_table_mut;
use chialisp::compiler::dialect::AcceptedDialect;
use chialisp::compiler::frontend::frontend;
use chialisp::compiler::sexp::{decode_string, parse_sexp};
use chialisp::compiler::srcloc::Srcloc;
use clvm_to_arm_chialisp::sexp_trait::{CreateChialispSExp, RcSExp, SrclocWrap};
use clvm_to_arm_emulate::emu_stub::CallbackGdbStub;
use clvm_to_arm_generate::code::{Program, TARGET_ADDR};
use clvmr::Allocator;
use js_sys::{Object, Reflect, Uint8Array};

thread_local! {
    static NEXT_ARM_GDB_ID: RefCell<i32> = {
        RefCell::new(0)
    };
    static ARM_GDB_STUBS: RefCell<HashMap<i32, RefCell<CallbackGdbStub>>> = {
        RefCell::new(HashMap::new())
    };
}

fn next_arm_gdb_id() -> i32 {
    NEXT_ARM_GDB_ID.with(|next| {
        let mut borrowed = next.borrow_mut();
        let result = *borrowed;
        *borrowed += 1;
        result
    })
}

fn js_error(message: impl ToString) -> JsValue {
    JsValue::from_str(&message.to_string())
}

fn build_elf(
    program_path: &str,
    program: &str,
    run_arg: &str,
    elf_output_name: &str,
) -> Result<(Vec<u8>, Rc<HashMap<String, String>>), String> {
    let srcloc = Srcloc::start(program_path);
    let env_parsed = parse_sexp(srcloc.clone(), run_arg.bytes())
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

    let compiled = compile_file(&mut allocator, runner, opts, program, &mut symbol_table)
        .map_err(|e| format!("failed to compile chialisp program: {e:?}"))?
        .to_sexp();
    build_symbol_table_mut(&mut symbol_table, &compiled);

    let symbols = Rc::new(symbol_table);
    let generator = Program::new::<CreateChialispSExp>(
        range_results,
        program_path,
        elf_output_name,
        RcSExp(Rc::new(compiled)),
        RcSExp(env_sexp.clone()),
        TARGET_ADDR,
        symbols.clone(),
    )?;
    let elf_data = generator.to_elf(elf_output_name)?;

    Ok((elf_data.object_file, symbols))
}

#[wasm_bindgen]
pub fn arm_gdb_build_program(
    program_path: String,
    program: String,
    run_arg: String,
    elf_output_name: String,
) -> Result<JsValue, JsValue> {
    let (elf, symbols) =
        build_elf(&program_path, &program, &run_arg, &elf_output_name).map_err(js_error)?;
    let symbols_json = serde_json::to_string(symbols.as_ref()).map_err(js_error)?;
    let result = Object::new();

    Reflect::set(
        &result,
        &JsValue::from_str("elf"),
        &Uint8Array::from(elf.as_slice()).into(),
    )?;
    Reflect::set(
        &result,
        &JsValue::from_str("symbolsJson"),
        &JsValue::from_str(&symbols_json),
    )?;

    Ok(result.into())
}

#[wasm_bindgen]
pub fn create_arm_gdb_stub(
    elf: Vec<u8>,
    symbols_json: String,
    output: &JsValue,
) -> Result<i32, JsValue> {
    let output_fn = output
        .dyn_ref::<js_sys::Function>()
        .ok_or_else(|| js_error("output must be a function"))?
        .clone();
    let symbols: HashMap<String, String> = serde_json::from_str(&symbols_json).map_err(js_error)?;
    let stub = CallbackGdbStub::new(
        &elf,
        Rc::new(symbols),
        Box::new(move |bytes| {
            output_fn
                .call1(&JsValue::null(), &Uint8Array::from(bytes).into())
                .map(|_| ())
                .map_err(|err| {
                    io::Error::new(
                        io::ErrorKind::Other,
                        err.as_string()
                            .unwrap_or_else(|| "gdb output callback failed".to_string()),
                    )
                })
        }),
    )
    .map_err(js_error)?;
    let new_id = next_arm_gdb_id();

    ARM_GDB_STUBS.with(|stubs| {
        stubs.replace_with(|stubs| {
            let mut work_stubs = HashMap::new();
            swap(&mut work_stubs, stubs);
            work_stubs.insert(new_id, RefCell::new(stub));
            work_stubs
        })
    });

    Ok(new_id)
}

#[wasm_bindgen]
pub fn arm_gdb_stub_incoming_data(stub_id: i32, data: Vec<u8>) -> Result<(), JsValue> {
    ARM_GDB_STUBS.with(|stubs| {
        let borrowed = stubs.borrow();
        let stub = borrowed
            .get(&stub_id)
            .ok_or_else(|| js_error(format!("unknown ARM GDB stub id {stub_id}")))?;
        let result = stub.borrow_mut().incoming_data(&data).map_err(js_error);
        result
    })
}

#[wasm_bindgen]
pub fn arm_gdb_stub_interrupt(stub_id: i32) -> Result<(), JsValue> {
    ARM_GDB_STUBS.with(|stubs| {
        let borrowed = stubs.borrow();
        let stub = borrowed
            .get(&stub_id)
            .ok_or_else(|| js_error(format!("unknown ARM GDB stub id {stub_id}")))?;
        let result = stub.borrow_mut().interrupt().map_err(js_error);
        result
    })
}

#[wasm_bindgen]
pub fn arm_gdb_stub_disconnected(stub_id: i32) -> Result<Option<String>, JsValue> {
    ARM_GDB_STUBS.with(|stubs| {
        let borrowed = stubs.borrow();
        let stub = borrowed
            .get(&stub_id)
            .ok_or_else(|| js_error(format!("unknown ARM GDB stub id {stub_id}")))?;
        let result = Ok(stub
            .borrow()
            .disconnected()
            .map(|reason| format!("{reason:?}")));
        result
    })
}

#[wasm_bindgen]
pub fn destroy_arm_gdb_stub(stub_id: i32) {
    ARM_GDB_STUBS.with(|stubs| {
        stubs.replace_with(|stubs| {
            let mut work_stubs = HashMap::new();
            swap(&mut work_stubs, stubs);
            work_stubs.remove(&stub_id);
            work_stubs
        })
    });
}
