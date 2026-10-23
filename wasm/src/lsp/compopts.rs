use std::borrow::Borrow;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::rc::Rc;

use crate::interfaces::{IFileReader, ILogWriter};
use crate::lsp::patch::{compute_comment_lines, split_text};
use crate::lsp::types::DocData;
use chialisp::compiler::compiler::{compile_pre_forms, STANDARD_MACROS};
use chialisp::compiler::comptypes::{
    CompileErr, CompilerOpts, CompilerOutput, HasCompilerOptsDelegation,
};
use chialisp::compiler::dialect::{DialectDescription, KNOWN_DIALECTS};
use chialisp::compiler::sexp::SExp;
use chialisp::compiler::srcloc::Srcloc;
use chialisp::compiler::BasicCompileContext;

use super::patch::get_bytes;

pub fn file_candidates(
    ws_root: Option<PathBuf>,
    include_paths: &[String],
    name: &str,
) -> Vec<String> {
    let default_paths = vec![".".to_owned()];
    let paths = if include_paths.is_empty() {
        &default_paths
    } else {
        include_paths
    };
    paths
        .iter()
        .filter_map(|path| {
            let base = ws_root
                .as_ref()
                .map(|root| root.join(path))
                .unwrap_or_else(|| PathBuf::from(path));
            let joined = base.join(name);
            let mut normalized = PathBuf::new();
            for component in joined.components() {
                match component {
                    std::path::Component::CurDir => {}
                    std::path::Component::ParentDir => {
                        if normalized.file_name().is_some_and(|name| name != "..") {
                            normalized.pop();
                        } else if !normalized.has_root() {
                            normalized.push("..");
                        }
                    }
                    _ => normalized.push(component.as_os_str()),
                }
            }
            normalized.to_str().map(str::to_owned)
        })
        .collect()
}

#[derive(Clone)]
pub struct LSPCompilerOpts {
    pub opts: Rc<dyn CompilerOpts>,
    pub log: Rc<dyn ILogWriter>,
    pub fs: Rc<dyn IFileReader>,
    pub ws_root: Option<PathBuf>,

    pub include_dirs: Vec<String>,

    pub lsp: Rc<RefCell<HashMap<String, DocData>>>,
    pub known_dialects: Rc<HashMap<String, DialectDescription>>,
    // Program imports compile modules and the compiler writes both exports and
    // content-addressed cache entries. Analysis keeps those artifacts local to
    // this compiler-options snapshot, never in the user's filesystem.
    pub generated_files: Rc<RefCell<HashMap<String, Vec<u8>>>>,
    pub read_files: Rc<RefCell<HashSet<String>>>,
}

impl HasCompilerOptsDelegation for LSPCompilerOpts {
    fn compiler_opts(&self) -> Rc<dyn CompilerOpts> {
        self.opts.clone()
    }
    fn update_compiler_opts<F: FnOnce(Rc<dyn CompilerOpts>) -> Rc<dyn CompilerOpts>>(
        &self,
        f: F,
    ) -> Rc<dyn CompilerOpts> {
        Rc::new(LSPCompilerOpts {
            opts: f(self.opts.clone()),
            ..self.clone()
        })
    }
    fn override_read_new_file(
        &self,
        inc_from: String,
        filename: String,
    ) -> Result<(String, Vec<u8>), CompileErr> {
        if let Some(content) = self
            .generated_files
            .as_ref()
            .borrow()
            .get(&filename)
            .cloned()
        {
            return Ok((filename, content));
        }
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

        let bytes = get_bytes(&content.text);
        // The compiler registers imported helper definitions before the LSP
        // parses their document. Guard its incomplete-mod panic on that path.
        // Other file types may be opaque embed-file payloads.
        if [".clsp", ".clinc", ".cl"]
            .iter()
            .any(|suffix| computed_filename.ends_with(suffix))
        {
            crate::lsp::reparse::validate_compiler_input(&computed_filename, &bytes)?;
        }
        Ok((computed_filename, bytes))
    }
    fn override_write_new_file(&self, target: &str, content: &[u8]) -> Result<(), CompileErr> {
        self.generated_files
            .as_ref()
            .borrow_mut()
            .insert(target.to_owned(), content.to_vec());
        Ok(())
    }

    fn override_set_search_paths(&self, new_paths: &[String]) -> Rc<dyn CompilerOpts> {
        let new_with_includes = self.opts.set_search_paths(new_paths);
        Rc::new(LSPCompilerOpts {
            opts: new_with_includes,
            include_dirs: new_paths.to_owned(),
            ..self.clone()
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
    log.log(&format!("get_file_content {name}"));
    log.log(&format!("include_paths {include_paths:?}"));
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
            known_dialects: Rc::new(KNOWN_DIALECTS.clone()),
            generated_files: Rc::new(RefCell::new(HashMap::new())),
            read_files: Rc::new(RefCell::new(HashSet::new())),
        }
    }

    fn get_file(&self, name: &str) -> Result<(String, DocData), String> {
        self.log.log(&format!("get_file {}", name));
        for path in file_candidates(self.ws_root.clone(), &self.include_dirs, name) {
            let uri = crate::lsp::types::urlify(&path);
            let doc = {
                let coll = self.lsp.as_ref().borrow();
                coll.get(&uri).or_else(|| coll.get(name)).cloned()
            };
            if let Some(doc) = doc {
                self.read_files.borrow_mut().insert(uri.clone());
                return Ok((uri, doc));
            }
            if let Ok(content) = self.fs.read_content(&path) {
                let text = split_text(&content);
                let comments = compute_comment_lines(&text);
                let doc = DocData {
                    fullname: uri.clone(),
                    text,
                    comments,
                    version: -1,
                };
                self.lsp.borrow_mut().insert(uri.clone(), doc.clone());
                self.read_files.borrow_mut().insert(uri.clone());
                return Ok((uri, doc));
            }
        }
        Err(format!("don't have {} to open", name))
    }
}

#[cfg(test)]
mod generated_artifact_tests {
    use super::*;
    use crate::interfaces::{EPrintWriter, FSFileReader};
    use chialisp::compiler::compiler::DefaultCompilerOpts;

    #[test]
    fn compiler_artifacts_are_shared_in_memory_and_never_written_to_disk() {
        let output = std::env::temp_dir().join(format!(
            "chialisp-lsp-memory-output-{}.hex",
            std::process::id()
        ));
        assert!(!output.exists());
        let target = output.to_str().unwrap();
        let opts = LSPCompilerOpts::new(
            Rc::new(DefaultCompilerOpts::new("source.clsp")),
            Rc::new(EPrintWriter::default()),
            Rc::new(FSFileReader::default()),
            None,
            &[],
            Rc::new(RefCell::new(HashMap::new())),
        );
        opts.write_new_file(target, b"ff0180").unwrap();
        // Compiler-options clones created for program imports share the cache.
        let imported_opts = opts.set_filename("imported.clsp");
        assert_eq!(
            imported_opts
                .read_new_file("parent.clsp".into(), target.into())
                .unwrap()
                .1,
            b"ff0180"
        );
        assert!(!output.exists());
    }
}
