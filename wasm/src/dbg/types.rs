use std::collections::HashMap;
use std::rc::Rc;

use clvm_tools_rs::classic::clvm_tools::comp_input::RunAndCompileInputData;
use clvm_tools_rs::compiler::comptypes::CompileForm;
use clvm_tools_rs::compiler::sexp::SExp;

pub struct DebuggerSymbols {
    pub symbol_file: String,
    pub symbols: HashMap<String, String>,
}

pub struct DebuggerSourceAndContent {
    pub source_file: String,
    pub source_content: Vec<u8>,
    pub source_parsed: Vec<Rc<SExp>>,
}

pub struct DebuggerInputs {
    pub is_hex: bool,
    pub source: Option<DebuggerSourceAndContent>,
    pub compile_input: Option<RunAndCompileInputData>,
    pub symbols: Option<DebuggerSymbols>,
    pub compiled: Result<Option<CompileForm>, String>,
}

pub trait MessageHandler<M> {
    fn handle_message(
        &mut self,
        raw_json: &serde_json::Value,
        msg: &M,
    ) -> Result<Option<Vec<M>>, String>;
}
