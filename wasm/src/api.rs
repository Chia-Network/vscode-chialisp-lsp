use std::cell::RefCell;
use std::collections::HashMap;
use std::io;
use std::mem::swap;
use std::ops::DerefMut;
use std::rc::Rc;
use std::sync::atomic::{AtomicUsize, Ordering};

use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;

use chialisp::classic::clvm_tools::stages::stage_0::DefaultProgramRunner;
use chialisp::compiler::prims;
use chialisp::compiler::debug::armjit::cmd::{compile_to_arm_elf_from_source, Args as ArmElfArgs};
use chialisp::compiler::debug::armjit::emu_stub::CallbackGdbStub;

use crate::dbg::handler::Debugger;
use crate::dbg::server::MessageBuffer;
use crate::interfaces::{IFileReader, ILogWriter};
use crate::lsp::{LSPServiceMessageHandler, LSPServiceProvider};

struct JSErrWriter {
    err_writer: js_sys::Function,
}

impl ILogWriter for JSErrWriter {
    fn log(&self, val: &str) {
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
    fn read_content(&self, name: &str) -> Result<String, String> {
        let name_str = JsValue::from_str(name);
        let res = self.file_reader.call1(&JsValue::null(), &name_str);
        res.map_err(|_| "Could not read file".to_string())
            .and_then(|content| {
                if content.loose_eq(&JsValue::null()) {
                    Err("could not read file".to_string())
                } else if let Some(s) = content.as_string() {
                    Ok(s)
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

thread_local! {
    static NEXT_ID: AtomicUsize = {
        AtomicUsize::new(0)
    };
    static LSP_SERVERS: RefCell<HashMap<i32, RefCell<LSPServiceProvider>>> = {
        RefCell::new(HashMap::new())
    };
    static DBG_SERVERS: RefCell<HashMap<i32, RefCell<MessageBuffer<Debugger>>>> = {
        RefCell::new(HashMap::new())
    };
    static GDB_STUBS: RefCell<HashMap<i32, RefCell<CallbackGdbStub>>> = {
        RefCell::new(HashMap::new())
    };
}

fn get_next_id() -> i32 {
    NEXT_ID.with(|n| n.fetch_add(1, Ordering::SeqCst) as i32)
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

#[wasm_bindgen]
pub fn create_dbg_service(file_reader: &JsValue, err_writer: &JsValue) -> i32 {
    let new_id = get_next_id();
    let fs = Rc::new(JSFileReader::new(file_reader));
    let log = Rc::new(JSErrWriter::new(err_writer));

    // Get prims
    let simple_prims = prims::prims();
    let mut prim_map = HashMap::new();

    for (name, sexp) in simple_prims.iter() {
        prim_map.insert(name.clone(), Rc::new(sexp.clone()));
    }

    let prims = Rc::new(prim_map);
    let runner = Rc::new(DefaultProgramRunner::new());
    let debugger = Debugger::new(fs, log, runner.clone(), prims.clone());
    let service = MessageBuffer::new(debugger);

    DBG_SERVERS.with(|servers| {
        servers.replace_with(|servers| {
            let mut work_services = HashMap::new();
            swap(&mut work_services, servers);
            work_services.insert(new_id, RefCell::new(service));
            work_services
        })
    });
    new_id
}

#[wasm_bindgen]
pub fn destroy_dbg_service(lsp: i32) {
    DBG_SERVERS.with(|servers| {
        servers.replace_with(|servers| {
            let mut work_services = HashMap::new();
            swap(&mut work_services, servers);
            work_services.remove(&lsp);
            work_services
        })
    });
}

#[wasm_bindgen]
pub fn dbg_service_handle_msg(lsp_id: i32, msg: String) -> Vec<JsValue> {
    let mut res = Vec::new();
    DBG_SERVERS.with(|services| {
        let service = services.borrow();
        if let Some(service_cell) = service.get(&lsp_id) {
            let mut s_borrowed = service_cell.borrow_mut();
            let s = s_borrowed.deref_mut();
            let outmsgs = s.process_message(msg.as_bytes());
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

fn create_js_error(message: String) -> JsValue {
    let error_object = js_sys::Object::new();
    let _ = js_sys::Reflect::set(
        &error_object,
        &JsValue::from_str("error"),
        &JsValue::from_str(&message),
    );
    error_object.into()
}

fn read_string_to_string_map(value: &JsValue) -> Result<HashMap<String, String>, String> {
    let object: js_sys::Object = value
        .dyn_ref::<js_sys::Object>()
        .cloned()
        .ok_or_else(|| "expected object".to_string())?;
    let entries = js_sys::Object::entries(&object);
    let mut result = HashMap::new();
    for entry in entries.iter() {
        let pair = js_sys::Array::from(&entry);
        let key = pair
            .get(0)
            .as_string()
            .ok_or_else(|| "symbol key should be string".to_string())?;
        let val = pair
            .get(1)
            .as_string()
            .ok_or_else(|| "symbol value should be string".to_string())?;
        result.insert(key, val);
    }
    Ok(result)
}

fn string_map_to_js_object(values: &HashMap<String, String>) -> js_sys::Object {
    let symbol_object = js_sys::Object::new();
    for (key, value) in values.iter() {
        let _ = js_sys::Reflect::set(
            &symbol_object,
            &JsValue::from_str(key),
            &JsValue::from_str(value),
        );
    }
    symbol_object
}

#[wasm_bindgen(js_name = compile_to_arm_elf)]
pub fn compile_to_arm_elf_js(
    input_js: JsValue,
    filename_js: JsValue,
    search_paths_js: Vec<JsValue>,
    env_js: JsValue,
) -> JsValue {
    let input = match input_js.as_string() {
        Some(s) => s,
        None => return create_js_error("input should be string".to_string()),
    };
    let filename = match filename_js.as_string() {
        Some(s) => s,
        None => return create_js_error("filename should be string".to_string()),
    };
    let env = match env_js.as_string() {
        Some(s) => s,
        None => return create_js_error("env should be string".to_string()),
    };
    let mut search_paths = Vec::new();
    for p in search_paths_js.iter() {
        if let Some(s) = p.as_string() {
            search_paths.push(s);
        } else {
            return create_js_error("search path should be string".to_string());
        }
    }

    let args = ArmElfArgs {
        include: search_paths,
        output: format!("{filename}.elf"),
        filename,
        env,
    };

    match compile_to_arm_elf_from_source(&args, input) {
        Ok(result) => {
            let output = js_sys::Object::new();
            let object_file = js_sys::Uint8Array::from(result.object_file.as_slice());
            let symbols = string_map_to_js_object(&result.symbol_table);
            let _ = js_sys::Reflect::set(
                &output,
                &JsValue::from_str("object_file"),
                &object_file.into(),
            );
            let _ = js_sys::Reflect::set(
                &output,
                &JsValue::from_str("synthetic_source"),
                &JsValue::from_str(&result.synthetic_source),
            );
            let _ = js_sys::Reflect::set(&output, &JsValue::from_str("symbols"), &symbols.into());
            output.into()
        }
        Err(e) => create_js_error(e),
    }
}

#[wasm_bindgen(js_name = create_gdb_stub)]
pub fn create_gdb_stub_js(
    object_file: Vec<u8>,
    symbols: JsValue,
    output_callback: JsValue,
) -> JsValue {
    let symbol_table = match read_string_to_string_map(&symbols) {
        Ok(s) => s,
        Err(e) => return create_js_error(e),
    };
    let callback = match output_callback.dyn_ref::<js_sys::Function>() {
        Some(f) => f.clone(),
        None => return create_js_error("output_callback should be function".to_string()),
    };

    let output = Box::new(move |data: &[u8]| -> Result<(), io::Error> {
        callback
            .call1(
                &JsValue::null(),
                &js_sys::Uint8Array::from(data).into(),
            )
            .map(|_| ())
            .map_err(|e| io::Error::new(io::ErrorKind::Other, format!("{e:?}")))
    });

    match CallbackGdbStub::new(&object_file, Rc::new(symbol_table), output) {
        Ok(stub) => {
            let new_id = get_next_id();
            GDB_STUBS.with(|servers| {
                servers.replace_with(|servers| {
                    let mut work_services = HashMap::new();
                    swap(&mut work_services, servers);
                    work_services.insert(new_id, RefCell::new(stub));
                    work_services
                })
            });
            JsValue::from(new_id)
        }
        Err(e) => create_js_error(e),
    }
}

#[wasm_bindgen(js_name = destroy_gdb_stub)]
pub fn destroy_gdb_stub_js(stub_id: i32) {
    GDB_STUBS.with(|servers| {
        servers.replace_with(|servers| {
            let mut work_services = HashMap::new();
            swap(&mut work_services, servers);
            work_services.remove(&stub_id);
            work_services
        })
    });
}

#[wasm_bindgen(js_name = gdb_stub_incoming_data)]
pub fn gdb_stub_incoming_data_js(stub_id: i32, data: Vec<u8>) -> JsValue {
    GDB_STUBS.with(|servers| {
        let service = servers.borrow();
        if let Some(service_cell) = service.get(&stub_id) {
            let mut s_borrowed = service_cell.borrow_mut();
            match s_borrowed.incoming_data(&data) {
                Ok(_) => JsValue::null(),
                Err(e) => create_js_error(e),
            }
        } else {
            create_js_error("no such gdb stub".to_string())
        }
    })
}

#[wasm_bindgen(js_name = gdb_stub_interrupt)]
pub fn gdb_stub_interrupt_js(stub_id: i32) -> JsValue {
    GDB_STUBS.with(|servers| {
        let service = servers.borrow();
        if let Some(service_cell) = service.get(&stub_id) {
            let mut s_borrowed = service_cell.borrow_mut();
            match s_borrowed.interrupt() {
                Ok(_) => JsValue::null(),
                Err(e) => create_js_error(e),
            }
        } else {
            create_js_error("no such gdb stub".to_string())
        }
    })
}
