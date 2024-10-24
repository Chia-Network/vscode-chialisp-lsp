use std::borrow::Borrow;
use std::cell::{Ref, RefCell};
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::rc::Rc;

use clvmr::allocator::Allocator;

use crate::interfaces::{IFileReader, ILogWriter};
use crate::lsp::patch::{compute_comment_lines, split_text};
use crate::lsp::types::DocData;
use clvm_tools_rs::classic::clvm_tools::stages::stage_0::TRunProgram;
use clvm_tools_rs::compiler::BasicCompileContext;
use clvm_tools_rs::compiler::compiler::{compile_pre_forms, STANDARD_MACROS};
use clvm_tools_rs::compiler::comptypes::{CompileErr, CompilerOpts, CompilerOutput, HasCompilerOptsDelegation};
use clvm_tools_rs::compiler::dialect::{DialectDescription, KNOWN_DIALECTS};
use clvm_tools_rs::compiler::optimize::get_optimizer;
use clvm_tools_rs::compiler::sexp::SExp;
use clvm_tools_rs::compiler::srcloc::Srcloc;
use clvm_tools_rs::compiler::CompileContextWrapper;

use super::patch::get_bytes;

#[derive(Clone)]
pub struct LSPCompilerOpts {
    pub opts: Rc<dyn CompilerOpts>,
    pub log: Rc<dyn ILogWriter>,
    pub fs: Rc<dyn IFileReader>,
    pub ws_root: Option<PathBuf>,

    pub include_dirs: Vec<String>,

    pub lsp: Rc<RefCell<HashMap<String, DocData>>>,
    pub known_dialects: Rc<HashMap<String, DialectDescription>>,
}

impl HasCompilerOptsDelegation for LSPCompilerOpts {
    fn compiler_opts(&self) -> Rc<dyn CompilerOpts> {
        self.opts.clone()
    }
    fn update_compiler_opts<F: FnOnce(Rc<dyn CompilerOpts>) -> Rc<dyn CompilerOpts>>(&self, f: F) -> Rc<dyn CompilerOpts> {
        Rc::new(LSPCompilerOpts {
            opts: f(self.opts.clone()),
            .. self.clone()
        })
    }
    fn override_read_new_file(
        &self,
        inc_from: String,
        filename: String,
    ) -> Result<(String, Vec<u8>), CompileErr> {
        if filename == "*macros*" {
            return Ok((filename, STANDARD_MACROS.clone().into()));
        } else if let Some(content) = self.known_dialects.get(&filename) {
            return Ok((filename, content.content.as_bytes().to_vec()));
        }

        let (computed_filename, content) = self.get_file(&filename).map_err(|_| {
            CompileErr(
                Srcloc::start(&inc_from),
                format!("could not find {} to include", filename),
            )
        })?;

        Ok((computed_filename, get_bytes(&content.text)))
    }
    fn override_set_search_paths(
        &self,
        new_paths: &[String]
    ) -> Rc<dyn CompilerOpts> {
        let new_with_includes = self.opts.set_search_paths(new_paths);
        Rc::new(LSPCompilerOpts {
            opts: new_with_includes,
            include_dirs: new_paths.to_owned(),
            .. self.clone()
        })
    }

    fn override_compile_program(
        &self,
        context: &mut BasicCompileContext,
        sexp: Rc<SExp>,
    ) -> Result<CompilerOutput, CompileErr> {
        let me = Rc::new(self.clone());
        compile_pre_forms(context, me, &[sexp])
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
        opts: Rc<dyn CompilerOpts>,
        log: Rc<dyn ILogWriter>,
        fs: Rc<dyn IFileReader>,
        ws_root: Option<PathBuf>,
        paths: &[String],
        docs: Rc<RefCell<HashMap<String, DocData>>>,
    ) -> Self {
        LSPCompilerOpts {
            opts,
            log,
            fs,
            ws_root,
            include_dirs: paths.to_owned(),
            lsp: docs,
            known_dialects: Rc::new(KNOWN_DIALECTS.clone())
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
