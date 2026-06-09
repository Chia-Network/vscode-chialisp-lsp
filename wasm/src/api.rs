use std::cell::RefCell;
use std::collections::HashMap;
use std::mem::swap;
use std::ops::DerefMut;
use std::rc::Rc;
use std::sync::atomic::{AtomicUsize, Ordering};

use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;

use chialisp::classic::clvm_tools::stages::stage_0::DefaultProgramRunner;
use chialisp::compiler::prims;

use crate::jsinterface::{JSErrWriter, JSFileReader};
use crate::dbg::handler::Debugger;
use crate::dbg::server::MessageBuffer;

use crate::lsp::{LSPServiceMessageHandler, LSPServiceProvider};

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
