use wasm_bindgen::{JsCast, JsValue};

use crate::interfaces::{IFileReader, ILogWriter};

pub struct JSErrWriter {
    err_writer: js_sys::Function,
}

impl ILogWriter for JSErrWriter {
    fn log(&self, val: &str) {
        let val_str = JsValue::from_str(val);
        self.err_writer.call1(&JsValue::null(), &val_str).unwrap();
    }
}

impl JSErrWriter {
    pub fn new(err_writer: &JsValue) -> Self {
        JSErrWriter {
            err_writer: err_writer.dyn_ref::<js_sys::Function>().unwrap().clone(),
        }
    }
}

pub struct JSFileReader {
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
    pub fn new(file_reader: &JsValue) -> Self {
        JSFileReader {
            file_reader: file_reader.dyn_ref::<js_sys::Function>().unwrap().clone(),
        }
    }
}
