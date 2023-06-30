use std::borrow::Borrow;
use std::cell::{Ref, RefCell};
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::rc::Rc;

use clvmr::allocator::Allocator;

use crate::interfaces::{IFileReader, ILogWriter};
use crate::lsp::patch::{compute_comment_lines, split_text, stringify_doc};
use crate::lsp::types::DocData;
use clvm_tools_rs::classic::clvm_tools::stages::stage_0::TRunProgram;
use clvm_tools_rs::compiler::CompileContextWrapper;
use clvm_tools_rs::compiler::compiler::{
    compile_pre_forms, create_prim_map, STANDARD_MACROS, ADVANCED_MACROS
};
use clvm_tools_rs::compiler::comptypes::{CompileErr, CompilerOpts, PrimaryCodegen};
use clvm_tools_rs::compiler::dialect::{AcceptedDialect, KNOWN_DIALECTS};
use clvm_tools_rs::compiler::optimize::get_optimizer;
use clvm_tools_rs::compiler::sexp::SExp;
use clvm_tools_rs::compiler::srcloc::Srcloc;

#[derive(Clone)]
pub struct LSPCompilerOpts {
    pub log: Rc<dyn ILogWriter>,
    pub fs: Rc<dyn IFileReader>,
    pub ws_root: Option<PathBuf>,
    pub include_dirs: Vec<String>,
    pub filename: String,
    pub compiler: Option<PrimaryCodegen>,
    pub in_defun: bool,
    pub stdenv: bool,
    pub optimize: bool,
    pub frontend_opt: bool,
    pub frontend_check_live: bool,
    pub start_env: Option<Rc<SExp>>,
    pub prim_map: Rc<HashMap<Vec<u8>, Rc<SExp>>>,
    pub dialect: AcceptedDialect,

    lsp: Rc<RefCell<HashMap<String, DocData>>>,
}

impl CompilerOpts for LSPCompilerOpts {
    fn filename(&self) -> String {
        self.filename.clone()
    }
    fn code_generator(&self) -> Option<PrimaryCodegen> {
        self.compiler.clone()
    }
    fn in_defun(&self) -> bool {
        self.in_defun
    }
    fn stdenv(&self) -> bool {
        self.stdenv
    }
    fn optimize(&self) -> bool {
        self.optimize
    }
    fn frontend_opt(&self) -> bool {
        self.frontend_opt
    }
    fn frontend_check_live(&self) -> bool {
        self.frontend_check_live
    }
    fn start_env(&self) -> Option<Rc<SExp>> {
        self.start_env.clone()
    }
    fn prim_map(&self) -> Rc<HashMap<Vec<u8>, Rc<SExp>>> {
        self.prim_map.clone()
    }
    fn get_search_paths(&self) -> Vec<String> {
        self.include_dirs.clone()
    }
    fn dialect(&self) -> AcceptedDialect {
        self.dialect.clone()
    }

    fn set_search_paths(&self, dirs: &[String]) -> Rc<dyn CompilerOpts> {
        let mut copy = self.clone();
        copy.include_dirs = dirs.to_owned();
        Rc::new(copy)
    }
    fn set_in_defun(&self, new_in_defun: bool) -> Rc<dyn CompilerOpts> {
        let mut copy = self.clone();
        copy.in_defun = new_in_defun;
        Rc::new(copy)
    }
    fn set_stdenv(&self, new_stdenv: bool) -> Rc<dyn CompilerOpts> {
        let mut copy = self.clone();
        copy.stdenv = new_stdenv;
        Rc::new(copy)
    }
    fn set_optimize(&self, optimize: bool) -> Rc<dyn CompilerOpts> {
        let mut copy = self.clone();
        copy.optimize = optimize;
        Rc::new(copy)
    }
    fn set_frontend_opt(&self, optimize: bool) -> Rc<dyn CompilerOpts> {
        let mut copy = self.clone();
        copy.frontend_opt = optimize;
        Rc::new(copy)
    }
    fn set_frontend_check_live(&self, check: bool) -> Rc<dyn CompilerOpts> {
        let mut copy = self.clone();
        copy.frontend_check_live = check;
        Rc::new(copy)
    }
    fn set_code_generator(&self, new_compiler: PrimaryCodegen) -> Rc<dyn CompilerOpts> {
        let mut copy = self.clone();
        copy.compiler = Some(new_compiler);
        Rc::new(copy)
    }
    fn set_start_env(&self, start_env: Option<Rc<SExp>>) -> Rc<dyn CompilerOpts> {
        let mut copy = self.clone();
        copy.start_env = start_env;
        Rc::new(copy)
    }
    fn set_dialect(&self, dialect: AcceptedDialect) -> Rc<dyn CompilerOpts> {
        let mut copy = self.clone();
        copy.dialect = dialect;
        Rc::new(copy)
    }
    fn set_prim_map(&self, prims: Rc<HashMap<Vec<u8>, Rc<SExp>>>) -> Rc<dyn CompilerOpts> {
        let mut copy = self.clone();
        copy.prim_map = prims;
        Rc::new(copy)
    }

    fn read_new_file(
        &self,
        inc_from: String,
        filename: String,
    ) -> Result<(String, Vec<u8>), CompileErr> {
        if filename == "*macros*" {
            if self.dialect().strict {
                return Ok((filename, ADVANCED_MACROS.as_bytes().to_vec()));
            } else {
                return Ok((filename, STANDARD_MACROS.as_bytes().to_vec()));
            }
        } else if let Some(content) = KNOWN_DIALECTS.get(&filename) {
            return Ok((filename, content.content.as_bytes().to_vec()));
        }

        let (computed_filename, content) = self.get_file(&filename).map_err(|_| {
            CompileErr(
                Srcloc::start(&inc_from),
                format!("could not find {} to include", filename),
            )
        })?;

        stringify_doc(&content.text)
            .map(|r| (computed_filename.clone(), r.as_bytes().to_vec()))
            .map_err(|x| CompileErr(Srcloc::start(&computed_filename), x))
    }

    fn compile_program(
        &self,
        allocator: &mut Allocator,
        runner: Rc<dyn TRunProgram>,
        sexp: Rc<SExp>,
        symbol_table: &mut HashMap<String, String>,
    ) -> Result<SExp, CompileErr> {
        let me = Rc::new(self.clone());
        let mut context_wrapper = CompileContextWrapper::new(
            allocator,
            runner,
            symbol_table,
            get_optimizer(&sexp.loc(), me.clone())?,
        );
        compile_pre_forms(&mut context_wrapper.context, me, &[sexp])
    }
}

pub fn get_file_content(
    log: Rc<dyn ILogWriter>,
    reader: Rc<dyn IFileReader>,
    ws_root: Option<PathBuf>,
    include_paths: &[String],
    name: &str,
) -> Result<(String, DocData), String> {
    log.log(&format!("get_file_content {}", name));
    for find_path in include_paths.iter() {
        let joined_find_to_root = if let Some(ref r) = ws_root {
            r.join(find_path).to_path_buf()
        } else {
            Path::new(r".").to_path_buf()
        };
        log.log(&format!(
            "joined_find_to_root {}",
            joined_find_to_root.to_str().unwrap()
        ));
        if let Some(try_path) = joined_find_to_root.join(name).to_str() {
            log.log(&format!("try path {}", try_path));
            if let Ok(filedata) = reader.read_content(try_path) {
                let doc_text = split_text(&filedata);
                let comments = compute_comment_lines(&doc_text);

                return Ok((
                    try_path.to_string(),
                    DocData {
                        fullname: try_path.to_string(),
                        text: doc_text,
                        version: -1,
                        comments,
                    },
                ));
            }
        }
    }
    Err(format!("don't have {} to open", name))
}

impl LSPCompilerOpts {
    pub fn new(
        log: Rc<dyn ILogWriter>,
        fs: Rc<dyn IFileReader>,
        ws_root: Option<PathBuf>,
        filename: &str,
        paths: &[String],
        docs: Rc<RefCell<HashMap<String, DocData>>>,
    ) -> Self {
        LSPCompilerOpts {
            log,
            fs,
            ws_root,
            include_dirs: paths.to_owned(),
            filename: filename.to_owned(),
            compiler: None,
            in_defun: false,
            stdenv: true,
            optimize: false,
            frontend_opt: false,
            frontend_check_live: true,
            start_env: None,
            prim_map: create_prim_map(),
            dialect: Default::default(),
            lsp: docs,
        }
    }

    fn get_file(&self, name: &str) -> Result<(String, DocData), String> {
        self.log.log(&format!("get_file {}", name));
        let cell: &RefCell<HashMap<String, DocData>> = self.lsp.borrow();
        let coll: Ref<HashMap<String, DocData>> = cell.borrow();
        coll.get(name)
            .map(|x| Ok((x.fullname.clone(), x.clone())))
            .unwrap_or_else(|| {
                get_file_content(
                    self.log.clone(),
                    self.fs.clone(),
                    self.ws_root.clone(),
                    &self.include_dirs,
                    name,
                )
            })
    }
}
