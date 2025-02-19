use std::collections::HashMap;
use std::path::Path;
use std::rc::Rc;

use clvmr::allocator::Allocator;

use crate::interfaces::{IFileReader, ILogWriter};
use crate::lsp::patch::{compute_comment_lines, get_bytes, split_text};
use crate::lsp::types::DocData;
use clvm_tools_rs::classic::clvm_tools::stages::stage_0::TRunProgram;
use clvm_tools_rs::compiler::compiler::{compile_pre_forms, STANDARD_MACROS};
use clvm_tools_rs::compiler::comptypes::{CompileErr, CompilerOpts, HasCompilerOptsDelegation};
use clvm_tools_rs::compiler::dialect::{DialectDescription, KNOWN_DIALECTS};
use clvm_tools_rs::compiler::optimize::get_optimizer;
use clvm_tools_rs::compiler::sexp::SExp;
use clvm_tools_rs::compiler::srcloc::Srcloc;
use clvm_tools_rs::compiler::CompileContextWrapper;

#[derive(Clone)]
pub struct DbgCompilerOpts {
    pub opts: Rc<dyn CompilerOpts>,
    pub log: Rc<dyn ILogWriter>,
    pub fs: Rc<dyn IFileReader>,

    pub include_dirs: Vec<String>,
    pub known_dialects: Rc<HashMap<String, DialectDescription>>,
}

impl HasCompilerOptsDelegation for DbgCompilerOpts {
    fn compiler_opts(&self) -> Rc<dyn CompilerOpts> {
        self.opts.clone()
    }
    fn update_compiler_opts<F: FnOnce(Rc<dyn CompilerOpts>) -> Rc<dyn CompilerOpts>>(
        &self,
        f: F,
    ) -> Rc<dyn CompilerOpts> {
        Rc::new(DbgCompilerOpts {
            opts: f(self.opts.clone()),
            ..self.clone()
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
                format!("could not find {filename} to include"),
            )
        })?;

        Ok((computed_filename, get_bytes(&content.text)))
    }

    fn override_compile_program(
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
            get_optimizer(&Srcloc::start(&self.filename()), me.clone())?,
        );
        compile_pre_forms(&mut context_wrapper.context, me, &[sexp])
    }
}

pub fn get_file_content(
    log: Rc<dyn ILogWriter>,
    reader: Rc<dyn IFileReader>,
    include_paths: &[String],
    name: &str,
) -> Result<(String, DocData), String> {
    log.log(&format!("get_file_content {name}"));
    for find_path in include_paths.iter() {
        let joined_find_to_root = Path::new(find_path).to_path_buf();
        if let Some(try_path) = joined_find_to_root.join(name).to_str() {
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
    Err(format!("don't have {name} to open"))
}

impl DbgCompilerOpts {
    pub fn new(
        opts: Rc<dyn CompilerOpts>,
        log: Rc<dyn ILogWriter>,
        fs: Rc<dyn IFileReader>,
        include_dirs: &[String],
    ) -> Self {
        DbgCompilerOpts {
            log,
            fs,
            opts,
            include_dirs: include_dirs.to_vec(),
            known_dialects: Rc::new(KNOWN_DIALECTS.clone()),
        }
    }

    fn get_file(&self, name: &str) -> Result<(String, DocData), String> {
        get_file_content(self.log.clone(), self.fs.clone(), &self.include_dirs, name)
    }
}
