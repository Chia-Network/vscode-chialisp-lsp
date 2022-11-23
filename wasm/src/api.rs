use js_sys;
use serde_json;

use std::cell::RefCell;
use std::collections::HashMap;
use std::mem::swap;
use std::ops::DerefMut;
use std::rc::Rc;
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering;

use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;

use clvmr::allocator::Allocator;

use clvm_tools_rs::classic::clvm::__type_compatibility__::{Bytes, BytesFromType, Stream};
use clvm_tools_rs::classic::clvm::serialize::sexp_to_stream;
use clvm_tools_rs::classic::clvm_tools::clvmc::compile_clvm_inner;
use clvm_tools_rs::classic::clvm_tools::stages::stage_0::DefaultProgramRunner;
use clvm_tools_rs::compiler::cldb::{
    hex_to_modern_sexp, CldbOverrideBespokeCode, CldbRun, CldbRunEnv, CldbRunnable,
    CldbSingleBespokeOverride,
};
use clvm_tools_rs::compiler::clvm::{convert_to_clvm_rs, start_step};
use clvm_tools_rs::compiler::compiler::{
    extract_program_and_env, path_to_function, rewrite_in_program, DefaultCompilerOpts,
};
use clvm_tools_rs::compiler::comptypes::CompileErr;
use crate::lsp::types::{IFileReader, ILogWriter};
use crate::lsp::{LSPServiceProvider, LSPServiceMessageHandler};
use clvm_tools_rs::compiler::prims;
use clvm_tools_rs::compiler::repl::Repl;
use clvm_tools_rs::compiler::runtypes::RunFailure;
use clvm_tools_rs::compiler::sexp::SExp;
use clvm_tools_rs::compiler::srcloc::Srcloc;
use crate::jsval::{
    btreemap_to_object, get_property, js_object_from_sexp, js_pair, object_to_value,
    read_string_to_string_map, sexp_from_js_object,
};

struct JSErrWriter {
    err_writer: js_sys::Function,
}

impl ILogWriter for JSErrWriter {
    fn write(&self, val: &str) {
        let val_str = JsValue::from_str(val);
        self.err_writer.call1(&JsValue::null(), &val_str).unwrap();
    }
}

impl JSErrWriter {
    fn new(err_writer: &JsValue) -> Self {
        JSErrWriter {
            err_writer: err_writer.dyn_ref::<js_sys::Function>().unwrap().clone(),
        }
    }
}

struct JSFileReader {
    file_reader: js_sys::Function,
}

impl IFileReader for JSFileReader {
    fn read(&self, name: &str) -> Result<Vec<u8>, String> {
        let name_str = JsValue::from_str(name);
        let res = self.file_reader.call1(&JsValue::null(), &name_str);
        res.map_err(|_| "Could not read file".to_string()).and_then(|content| {
            if content.loose_eq(&JsValue::null()) {
                Err("could not read file".to_string())
            } else if let Some(s) = content.as_string() {
                Ok(s.as_bytes().iter().copied().collect())
            } else {
                Err("could not convert content to string".to_string())
            }
        })
    }
}

impl JSFileReader {
    fn new(file_reader: &JsValue) -> Self {
        JSFileReader {
            file_reader: file_reader.dyn_ref::<js_sys::Function>().unwrap().clone(),
        }
    }
}

#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

struct JsRunStep {
    allocator: Allocator,
    cldbrun: CldbRun,
}

struct JsRepl {
    allocator: RefCell<Allocator>,
    repl: RefCell<Repl>,
}

thread_local! {
    static NEXT_ID: AtomicUsize = {
        return AtomicUsize::new(0);
    };
    static RUNNERS: RefCell<HashMap<i32, JsRunStep>> = {
        return RefCell::new(HashMap::new());
    };
    static REPLS: RefCell<HashMap<i32, JsRepl>> = {
        return RefCell::new(HashMap::new());
    };
    static LSP_SERVERS: RefCell<HashMap<i32, RefCell<LSPServiceProvider>>> = {
        return RefCell::new(HashMap::new());
    };
}

fn get_next_id() -> i32 {
    NEXT_ID.with(|n| n.fetch_add(1, Ordering::SeqCst) as i32)
}

fn insert_runner(this_id: i32, runner: JsRunStep) {
    RUNNERS.with(|runcell| {
        runcell.replace_with(|coll| {
            let mut work_collection = HashMap::new();
            swap(coll, &mut work_collection);
            work_collection.insert(this_id, runner);
            work_collection
        });
    });
}

fn remove_runner(this_id: i32) {
    RUNNERS.with(|runcell| {
        runcell.replace_with(|coll| {
            let mut work_collection = HashMap::new();
            swap(coll, &mut work_collection);
            work_collection.remove(&this_id);
            work_collection
        });
    });
}

fn with_runner<F, V>(this_id: i32, f: F) -> Option<V>
where
    F: FnOnce(&mut JsRunStep) -> Option<V>,
{
    let mut result = None;
    RUNNERS.with(|runcell| {
        runcell.replace_with(|coll| {
            let mut work_collection = HashMap::new();
            swap(coll, &mut work_collection);
            match work_collection.get_mut(&this_id) {
                Some(r_ref) => {
                    result = f(r_ref);
                }
                _ => {}
            }
            work_collection
        });
    });
    result
}

fn create_clvm_runner_err(error: String) -> JsValue {
    let array = js_sys::Array::new();
    array.set(
        0,
        js_pair(JsValue::from_str("error"), JsValue::from_str(&error)),
    );
    return object_to_value(&js_sys::Object::from_entries(&array).unwrap());
}

fn create_clvm_runner_run_failure(err: &RunFailure) -> JsValue {
    match err {
        RunFailure::RunErr(l, e) => {
            return create_clvm_runner_err(format!("{}: Error {}", l.to_string(), e));
        }
        RunFailure::RunExn(l, e) => {
            return create_clvm_runner_err(format!("{}: Exn {}", l.to_string(), e.to_string()))
        }
    }
}

fn create_clvm_compile_failure(err: &CompileErr) -> JsValue {
    match err {
        CompileErr(l, e) => {
            return create_clvm_runner_err(format!("{}: Error {}", l.to_string(), e));
        }
    }
}

struct JsBespokeOverride {
    fun: js_sys::Function,
}

impl CldbSingleBespokeOverride for JsBespokeOverride {
    // We've been called so compose a javascript-compatible value to pass out
    // based on the env.
    // When the user returns, try to convert the result back to sexp.
    fn get_override(&self, env: Rc<SExp>) -> Result<Rc<SExp>, RunFailure> {
        let args = js_sys::Array::new();
        args.set(0, js_object_from_sexp(env.clone()));
        self.fun
            .apply(&JsValue::null(), &args)
            .map_err(|e| {
                let eval = js_sys::Array::from(&e);
                RunFailure::RunErr(
                    env.loc(),
                    format!("exception from javascript: {}", eval.to_string()),
                )
            })
            .and_then(|v| {
                sexp_from_js_object(env.loc(), &v)
                    .map(|s| Ok(s))
                    .unwrap_or_else(|| {
                        Err(RunFailure::RunErr(
                            env.loc(),
                            "javascript passed in a value that can't be converted to sexp"
                                .to_string(),
                        ))
                    })
            })
    }
}

/* Given a program in text (utf-8) form and arguments, compile the program and
 * return a runner object.
 */
#[wasm_bindgen]
pub fn create_clvm_runner(
    hex_prog: String,
    args_js: JsValue,
    symbols: &js_sys::Object,
    overrides: &js_sys::Object,
) -> JsValue {
    let mut allocator = Allocator::new();
    let runner = Rc::new(DefaultProgramRunner::new());
    let args_srcloc = Srcloc::start(&"*args*".to_string());
    let prog_srcloc = Srcloc::start(&"*program*".to_string());
    let mut prim_map = HashMap::new();
    let mut override_funs: HashMap<String, Box<dyn CldbSingleBespokeOverride>> = HashMap::new();

    let args = match sexp_from_js_object(args_srcloc.clone(), &args_js) {
        Some(v) => v,
        None => {
            return create_clvm_runner_run_failure(&RunFailure::RunErr(
                args_srcloc.clone(),
                "failed to convert args to sexp".to_string(),
            ));
        }
    };

    let symbol_table = match read_string_to_string_map(symbols) {
        Ok(s) => s,
        Err(e) => {
            return create_clvm_runner_run_failure(&RunFailure::RunErr(
                args_srcloc.clone(),
                format!("failed to read symbol table: {}", e),
            ));
        }
    };

    for ent in js_sys::Object::keys(&overrides).values() {
        let key = ent.unwrap().as_string().unwrap();
        let val = get_property(&overrides, &key).unwrap();
        match val.dyn_ref::<js_sys::Function>() {
            Some(f) => {
                override_funs.insert(key, Box::new(JsBespokeOverride { fun: f.clone() }));
            }
            _ => {}
        }
    }

    for p in prims::prims().iter() {
        prim_map.insert(p.0.clone(), Rc::new(p.1.clone()));
    }

    let program = match hex_to_modern_sexp(
        &mut allocator,
        &symbol_table,
        prog_srcloc.clone(),
        &hex_prog,
    ) {
        Ok(v) => v,
        Err(e) => {
            return create_clvm_runner_run_failure(&e);
        }
    };

    let runner_override: Box<dyn CldbRunnable> = Box::new(CldbOverrideBespokeCode::new(
        symbol_table.clone(),
        override_funs,
    ));
    let prim_map_rc = Rc::new(prim_map);
    let step = start_step(program.clone(), args.clone());
    let cldbenv = CldbRunEnv::new(None, vec![], runner_override);
    let cldbrun = CldbRun::new(runner.clone(), prim_map_rc.clone(), Box::new(cldbenv), step);

    let this_id = get_next_id();
    insert_runner(this_id, JsRunStep { allocator, cldbrun });

    return JsValue::from(this_id);
}

#[wasm_bindgen]
pub fn final_value(runner: i32) -> JsValue {
    with_runner(runner, |r| {
        r.cldbrun.final_result().map(|v| js_object_from_sexp(v))
    })
    .unwrap_or_else(|| JsValue::null())
}

#[wasm_bindgen]
pub fn remove_clvm_runner(runner: i32) {
    remove_runner(runner);
}

// Run until the given step, returning the current machine state.
#[wasm_bindgen]
pub fn run_step(runner: i32) -> JsValue {
    with_runner(runner, |r| {
        if r.cldbrun.is_ended() {
            return None;
        }

        r.cldbrun.step(&mut r.allocator)
    })
    .map(|result_hash| btreemap_to_object(result_hash.iter()))
    .unwrap_or_else(|| JsValue::null())
}

fn make_compile_output(result_stream: &Stream, symbol_table: &HashMap<String, String>) -> JsValue {
    let output_hex = result_stream.get_value().hex();
    let array = js_sys::Array::new();
    array.set(
        0,
        js_pair(JsValue::from_str("hex"), JsValue::from_str(&output_hex)),
    );
    let symbol_array = js_sys::Array::new();
    let mut idx = 0;
    for (k, v) in symbol_table.iter() {
        symbol_array.set(idx, js_pair(JsValue::from_str(&k), JsValue::from_str(&v)));
        idx += 1;
    }
    let symbol_object = object_to_value(&js_sys::Object::from_entries(&symbol_array).unwrap());
    array.set(1, js_pair(JsValue::from_str("symbols"), symbol_object));
    object_to_value(&js_sys::Object::from_entries(&array).unwrap())
}

// Compile a program, giving
// {"hex": "02392349234...", "symbols":{...}}
// or
// {"error": ...}
#[wasm_bindgen]
pub fn compile(input_js: JsValue, filename_js: JsValue, search_paths_js: Vec<JsValue>) -> JsValue {
    let mut allocator = Allocator::new();
    let mut symbol_table = HashMap::new();
    let mut result_stream = Stream::new(None);
    let input = input_js.as_string().unwrap();
    let filename = filename_js.as_string().unwrap();
    let search_paths: Vec<String> = search_paths_js
        .iter()
        .map(|j| j.as_string().unwrap())
        .collect();

    match compile_clvm_inner(
        &mut allocator,
        &search_paths,
        &mut symbol_table,
        &filename,
        &input,
        &mut result_stream,
    ) {
        Ok(_) => make_compile_output(&result_stream, &symbol_table),
        Err(e) => create_clvm_runner_err(e),
    }
}

fn find_function_hash(symbol_table: &HashMap<String, String>, f: &String) -> Option<String> {
    for (k, v) in symbol_table.iter() {
        if v == f {
            return Some(k.clone());
        }
    }
    None
}

// Given a program hex and symbols, compose the program that executes a given
// function with some given arguments as though the program's primary expression
// had been that.
// returns a string or {"error"...}
#[wasm_bindgen]
pub fn compose_run_function(
    hex_prog: String,
    symbol_table_js: &js_sys::Object,
    function_name: String,
) -> JsValue {
    let mut allocator = Allocator::new();
    let loc = Srcloc::start(&"*js*".to_string());
    let symbol_table = match read_string_to_string_map(symbol_table_js) {
        Ok(s) => s,
        Err(e) => {
            return create_clvm_compile_failure(&CompileErr(loc.clone(), e));
        }
    };
    let function_hash = match find_function_hash(&symbol_table, &function_name) {
        Some(f) => f,
        _ => {
            return create_clvm_compile_failure(&CompileErr(
                loc.clone(),
                format!("function not found in symbols: {}", function_name),
            ));
        }
    };
    let program = match hex_to_modern_sexp(&mut allocator, &symbol_table, loc.clone(), &hex_prog) {
        Ok(v) => v,
        Err(e) => {
            return create_clvm_runner_run_failure(&e);
        }
    };
    let main_env = match extract_program_and_env(program.clone()) {
        Some(em) => em,
        _ => {
            return create_clvm_compile_failure(&CompileErr(
                program.loc(),
                "could not extract env from program".to_string(),
            ));
        }
    };
    let hash_bytes = Bytes::new(Some(BytesFromType::Hex(function_hash.clone())));
    let function_path = match path_to_function(main_env.1.clone(), &hash_bytes.data().clone()) {
        Some(p) => p,
        _ => {
            return create_clvm_compile_failure(&CompileErr(
                program.loc(),
                format!(
                    "could not find function with hash from symbols: {}",
                    function_name
                ),
            ));
        }
    };

    let new_program = rewrite_in_program(function_path, main_env.1);
    let mut result_stream = Stream::new(None);
    let clvm_rs_value = match convert_to_clvm_rs(&mut allocator, new_program) {
        Ok(c) => c,
        Err(e) => {
            return create_clvm_runner_run_failure(&e);
        }
    };
    sexp_to_stream(&mut allocator, clvm_rs_value, &mut result_stream);
    JsValue::from_str(&result_stream.get_value().hex())
}

// Create a repl session
#[wasm_bindgen]
pub fn create_repl() -> i32 {
    let allocator = Allocator::new();
    let opts = Rc::new(DefaultCompilerOpts::new(&"*repl*".to_string()));
    let runner = Rc::new(DefaultProgramRunner::new());
    let repl = Repl::new(opts, runner.clone());
    let new_id = get_next_id();
    let mut prim_map = HashMap::new();

    for p in prims::prims().iter() {
        prim_map.insert(p.0.clone(), Rc::new(p.1.clone()));
    }

    REPLS.with(|repls| {
        repls.replace_with(|repls| {
            let mut work_repls = HashMap::new();
            swap(&mut work_repls, repls);
            work_repls.insert(
                new_id,
                JsRepl {
                    allocator: RefCell::new(allocator),
                    repl: RefCell::new(repl),
                },
            );
            work_repls
        })
    });

    new_id
}

#[wasm_bindgen]
pub fn destroy_repl(repl_id: i32) {
    REPLS.with(|repls| {
        repls.replace_with(|repls| {
            let mut work_repls = HashMap::new();
            swap(&mut work_repls, repls);
            work_repls.remove(&repl_id);
            work_repls
        })
    });
}

#[wasm_bindgen]
pub fn repl_run_string(repl_id: i32, input: String) -> JsValue {
    REPLS
        .with(|repls| {
            let repls = repls.borrow();
            if let Some(repl_container) = repls.get(&repl_id) {
                let mut a_borrowed = repl_container.allocator.borrow_mut();
                let a = a_borrowed.deref_mut();
                let mut r_borrowed = repl_container.repl.borrow_mut();
                let r = r_borrowed.deref_mut();
                r.process_line(a, input)
            } else {
                Err(CompileErr(
                    Srcloc::start(&"*repl*".to_string()),
                    "no such repl".to_string(),
                ))
            }
        })
        .map(|v| v.map(|v| js_object_from_sexp(v.to_sexp())))
        .unwrap_or_else(|e| {
            Some(create_clvm_runner_err(format!(
                "{}: {}",
                e.0.to_string(),
                e.1
            )))
        })
        .unwrap_or_else(|| JsValue::null())
}

#[wasm_bindgen]
pub fn sexp_to_string(v: &JsValue) -> JsValue {
    let loc = Srcloc::start(&"*val*".to_string());
    sexp_from_js_object(loc, v)
        .map(|s| JsValue::from_str(&s.to_string()))
        .unwrap_or_else(|| create_clvm_runner_err("unable to convert to value".to_string()))
}

#[wasm_bindgen]
pub fn create_lsp_service(file_reader: &JsValue, err_writer: &JsValue) -> i32 {
    let new_id = get_next_id();
    let log = Rc::new(JSErrWriter::new(err_writer));
    LSP_SERVERS.with(|servers| {
        servers.replace_with(|servers| {
            let mut work_services = HashMap::new();
            swap(&mut work_services, servers);
            work_services.insert(
                new_id,
                RefCell::new(LSPServiceProvider::new(
                    Rc::new(JSFileReader::new(file_reader)),
                    log,
                    false,
                )),
            );
            work_services
        })
    });
    new_id
}

#[wasm_bindgen]
pub fn destroy_lsp_service(lsp: i32) {
    LSP_SERVERS.with(|servers| {
        servers.replace_with(|servers| {
            let mut work_services = HashMap::new();
            swap(&mut work_services, servers);
            work_services.remove(&lsp);
            work_services
        })
    });
}

#[wasm_bindgen]
pub fn lsp_service_handle_msg(lsp_id: i32, msg: String) -> Vec<JsValue> {
    let mut res = Vec::new();
    LSP_SERVERS.with(|services| {
        let service = services.borrow();
        if let Some(service_cell) = service.get(&lsp_id) {
            let mut s_borrowed = service_cell.borrow_mut();
            let s = s_borrowed.deref_mut();
            let outmsgs = s.handle_message_from_string(msg);
            for m in outmsgs.iter() {
                if let Ok(r) = serde_json::to_value(m) {
                    res.push(JsValue::from_str(&r.to_string()));
                } else {
                    panic!("unable to convert message {:?} to json", m);
                }
            }
        }
    });
    res
}
