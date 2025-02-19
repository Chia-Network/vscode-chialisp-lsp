use std::borrow::Borrow;
use std::collections::{BTreeMap, HashMap};
use std::io::BufRead;
use std::path::PathBuf;
use std::rc::Rc;

use debug_types::events::StoppedReason;
use debug_types::requests::{InitializeRequestArguments, LaunchRequestArguments, SourceArguments};
use debug_types::responses::{ResponseBody, SourceResponse};
use debug_types::types::{Breakpoint, Source, SourceBreakpoint};

use clvm_tools_rs::classic::clvm::sexp::sexp_as_bin;
use clvm_tools_rs::classic::clvm_tools::clvmc::{compile_clvm_text, CompileError};
use clvm_tools_rs::compiler::cldb::hex_to_modern_sexp;
use clvm_tools_rs::compiler::cldb_hierarchy::{
    HierarchialRunner, HierarchialStepResult, RunPurpose,
};

use clvmr::Allocator;

use clvm_tools_rs::classic::clvm::__type_compatibility__::{Bytes, BytesFromType};
use clvm_tools_rs::compiler::compiler::DefaultCompilerOpts;
use clvm_tools_rs::compiler::comptypes::{CompileErr, CompileForm, CompilerOpts};
use clvm_tools_rs::compiler::frontend::frontend;
use clvm_tools_rs::compiler::runtypes::RunFailure;
use clvm_tools_rs::compiler::sexp::{decode_string, parse_sexp, SExp};
use clvm_tools_rs::compiler::srcloc::Srcloc;

use crate::interfaces::{EPrintWriter, IFileReader, ILogWriter};
use crate::lsp::types::{DocPosition, DocRange};

const LARGE_COLUMN: u32 = 100000;

/// When stepping in or out, set a depth to stop at if reached before a breakpoint.
#[derive(Clone, Debug)]
pub enum TargetDepth {
    /// Step out to this depth.
    LessThan(usize),
    /// Stop when the depth is the same as the current depth.
    LessOrEqual(usize),
}

/// A structure containing what we know about the code as we passed the entry into
/// this scope.  Since CLVM doesn't model the stack in the same way as a
/// traditional vm, we synthesize a stack as we go.  cldh_hierarchy will tell us
/// that the shape of the stack changed, and we'll push and pop frames as needed
/// to match.  This contains our understanding of the state of execution when
/// each frame was pushed.
#[derive(Clone, Debug)]
pub struct StoredScope {
    pub scope_id: u32,

    pub name: String,
    pub named_args: HashMap<String, Rc<SExp>>,

    pub rundata: Option<BTreeMap<String, String>>,

    pub source: Srcloc,
}

#[derive(Clone, Debug)]
pub struct RecognizedBreakpoint {
    // hash: String, // Future: break by hash (clippy)
    spec: Breakpoint,
}

/// This is a running debugger.  It's treated as an object with mutability via
/// its step and set_breakpoints methods.  It is the primary object that implements
/// tracking the debug state using the transitions emitted by the HierarchialRunner
/// it holds.
///
/// It is created by the launch() method of Debugger.
pub struct RunningDebugger {
    pub initialized: InitializeRequestArguments,
    pub launch_info: LaunchRequestArguments,
    pub target_depth: Option<TargetDepth>,
    pub stopped_reason: Option<StoppedReason>,

    pub source_file: String,
    pub compiled: Option<CompileForm>,

    pub running: bool,
    pub run: HierarchialRunner,
    pub symbols: Rc<HashMap<String, String>>,
    pub output_stack: Vec<StoredScope>,
    pub result: Option<String>,

    pub breakpoints: HashMap<String, HashMap<String, RecognizedBreakpoint>>,
    pub next_bp_id: usize,
    pub at_breakpoint: Option<Breakpoint>,

    pub opts: Rc<dyn CompilerOpts>,
}

fn fuzzy_file_match(location_filename: &str, want_filename: &str) -> bool {
    if let (Some(location_fn), Some(want_fn)) = (
        PathBuf::from(location_filename).file_name(),
        PathBuf::from(want_filename).file_name(),
    ) {
        location_fn == want_fn
    } else {
        false
    }
}

#[test]
fn test_fuzzy_file_match_1() {
    assert!(fuzzy_file_match("test/foo/bar.clsp", "bar/baz/bar.clsp"));
}

fn resolve_function(symbols: Rc<HashMap<String, String>>, name: &str) -> Option<String> {
    for (k, v) in symbols.iter() {
        if v == name {
            return Some(k.clone());
        }
    }

    None
}

#[test]
fn test_resolve_function_1() {
    let symbols_map = HashMap::from([(
        "de3687023fa0a095d65396f59415a859dd46fc84ed00504bf4c9724fca08c9de".to_string(),
        "fact".to_string(),
    )]);
    let symbols = Rc::new(symbols_map);
    assert_eq!(
        resolve_function(symbols, "fact"),
        Some("de3687023fa0a095d65396f59415a859dd46fc84ed00504bf4c9724fca08c9de".to_string())
    );
}

/// Given symbols, possibly a CompileForm and a SourceBreakpoint, try a few tricks
/// to figure out where we should stop in the source when a breakpoint is set.
///
/// This can certainly be improved.
fn find_location(
    symbols: Rc<HashMap<String, String>>,
    compiled: &Option<CompileForm>,
    log: Rc<dyn ILogWriter>,
    file: &str,
    b: &SourceBreakpoint,
) -> Option<(String, Srcloc)> {
    let whole_line = DocRange {
        start: DocPosition {
            line: b.line - 1,
            character: 0,
        },
        end: DocPosition {
            line: b.line - 1,
            character: LARGE_COLUMN,
        },
    };
    let breakpoint_range = b
        .column
        .map(|_col| DocRange {
            start: DocPosition {
                line: b.line - 1,
                character: 0,
            },
            end: DocPosition {
                line: b.line - 1,
                character: LARGE_COLUMN,
            },
        })
        .unwrap_or_else(|| whole_line.clone());

    let breakpoint_loc = breakpoint_range.to_srcloc(file);
    for (k, v) in symbols.iter() {
        if let Some(parsed_srcloc) = parse_srcloc(v) {
            let borrowed_filename: &String = parsed_srcloc.file.borrow();
            if !fuzzy_file_match(file, borrowed_filename) {
                continue;
            }
            let normalized_loc = Srcloc::new(
                breakpoint_loc.file.clone(),
                parsed_srcloc.line,
                parsed_srcloc.col,
            );
            if normalized_loc.overlap(&breakpoint_loc) {
                return Some((k.clone(), parsed_srcloc.clone()));
            }
        }
    }

    let whole_line_loc = whole_line.to_srcloc(file);
    compiled.as_ref().and_then(|c| {
        for h in c.helpers.iter() {
            let original_loc = h.loc();
            let original_loc_file: &String = original_loc.file.borrow();
            if !fuzzy_file_match(original_loc_file, file) {
                continue;
            }
            let normalized_loc = Srcloc::new(
                whole_line_loc.file.clone(),
                original_loc.line,
                original_loc.col,
            );
            log.log(&format!("{normalized_loc} vs target loc {whole_line_loc}"));
            if whole_line_loc.overlap(&normalized_loc) {
                log.log(&format!("found function {}", decode_string(h.name())));
                return resolve_function(symbols, &decode_string(h.name()))
                    .map(|funhash| (funhash, original_loc));
            }
        }

        None
    })
}

#[test]
fn test_simple_find_location_classic_symbols_1() {
    let log = Rc::new(EPrintWriter::new());
    let symbols_map = HashMap::from([(
        "de3687023fa0a095d65396f59415a859dd46fc84ed00504bf4c9724fca08c9de".to_string(),
        "fact".to_string(),
    )]);
    let symbols = Rc::new(symbols_map);
    let program =
        "(mod (X)\n  (defun fact (X) (if (= X 1) 1 (* X (fact (- X 1)))))\n  (fact 5)\n  )";
    let parsed = parse_sexp(Srcloc::start("fact.clsp"), program.bytes()).expect("should parse");
    let opts = Rc::new(DefaultCompilerOpts::new("fact.clsp"));
    let compiled = frontend(opts, &parsed).expect("should compile");
    let breakpoint_spec = SourceBreakpoint {
        column: Some(0),
        condition: None,
        hit_condition: None,
        line: 2,
        log_message: None,
    };
    let (hash, _) = find_location(symbols, &Some(compiled), log, "fact.clsp", &breakpoint_spec)
        .expect("should be found");
    assert_eq!(
        hash,
        "de3687023fa0a095d65396f59415a859dd46fc84ed00504bf4c9724fca08c9de"
    );
}

impl RunningDebugger {
    /// Given a request from the consumer, try to find and vend the requested
    /// source file.  This can become a lot more sophisticated.  We might consider
    /// storing the actual input files and other stuff in the symbols.
    ///
    /// In its current incarnation, this tries the CompilerOpts in RunningDebugger
    /// to access the filesystem via the abstraction it carries.
    pub fn get_source_file(
        &self,
        log: Rc<dyn ILogWriter>,
        sfargs: &SourceArguments,
    ) -> Option<ResponseBody> {
        log.log(&format!("get_source_file {sfargs:?}"));
        sfargs
            .source
            .as_ref()
            .and_then(|s| s.name.as_ref())
            .and_then(|n| {
                self.opts
                    .read_new_file(self.source_file.to_string(), n.to_string())
                    .ok()
            })
            .map(|content| {
                ResponseBody::Source(SourceResponse {
                    content: String::from_utf8(content.1.clone()).unwrap(),
                    mime_type: Some("text/plain".to_owned()),
                })
            })
    }

    /// Set these breakpoints in the source file they're indicated for.
    pub fn set_breakpoints(
        &mut self,
        log: Rc<dyn ILogWriter>,
        s: &Source,
        breakpoints: Vec<SourceBreakpoint>,
    ) -> Vec<Breakpoint> {
        let use_path = s
            .path
            .as_ref()
            .cloned()
            .map(Some)
            .unwrap_or_else(|| s.name.clone());
        log.log(&format!("s.path {:?} s.name {:?}", s.path, s.name));
        let bp_id_start = self.next_bp_id;
        self.next_bp_id += breakpoints.len();

        let empty_breakpoint = |(i, b): (usize, &SourceBreakpoint)| Breakpoint {
            id: Some(i + bp_id_start),
            column: b.column,
            end_column: b.column,
            line: Some(b.line),
            end_line: Some(b.line),
            instruction_reference: None,
            message: Some("No source file to break in".to_string()),
            offset: None,
            source: None,
            verified: false,
        };

        if let Some(p) = use_path {
            log.log(&format!("path {p}"));

            // Clear previous breakpoints for the translation unit.
            self.breakpoints.remove(&p);

            let mut inserted_breakpoints = HashMap::new();
            let mut reported_breakpoints = Vec::new();

            for (i, b) in breakpoints.iter().enumerate() {
                // Verfified if we overlap at least one location in the symbols
                // We.ll be simple and set it to the first matching point following
                // the given location.
                if let Some((hash, found)) =
                    find_location(self.symbols.clone(), &self.compiled, log.clone(), &p, b)
                {
                    let end_col = found.until.clone().map(|e| e.col as u32);
                    let end_line = found.until.map(|e| e.line as u32);
                    let bp = Breakpoint {
                        id: Some(i + bp_id_start),
                        line: Some(found.line as u32),
                        column: Some(found.col as u32),
                        end_line,
                        end_column: end_col,
                        message: None,
                        offset: None,
                        instruction_reference: None,
                        source: Some(s.clone()),
                        verified: true,
                    };
                    reported_breakpoints.push(bp.clone());
                    inserted_breakpoints.insert(
                        hash.clone(),
                        RecognizedBreakpoint {
                            // hash,
                            spec: bp,
                        },
                    );
                } else {
                    reported_breakpoints.push(empty_breakpoint((i, b)));
                }
            }

            self.breakpoints.insert(p, inserted_breakpoints.clone());

            reported_breakpoints
        } else {
            breakpoints
                .iter()
                .enumerate()
                .map(empty_breakpoint)
                .collect()
        }
    }

    pub fn get_stack_depth(&self) -> usize {
        self.output_stack.len()
    }

    fn real_step(
        &mut self,
        frame: StoredScope,
        info: BTreeMap<String, String>,
    ) -> Option<BTreeMap<String, String>> {
        let running_frames = self
            .run
            .running
            .iter()
            .map(|f| f.purpose.clone())
            .filter(|p| matches!(p, RunPurpose::Main))
            .count();

        // Ensure we're showing enough frames.
        if running_frames >= self.output_stack.len() {
            self.output_stack.push(frame.clone());
        }

        if running_frames < self.output_stack.len() {
            if let Some(popped) = self.output_stack.pop() {
                if !self.output_stack.is_empty() {
                    let last_idx = self.output_stack.len() - 1;
                    self.output_stack[last_idx].rundata = popped.rundata;
                }
            }
        }

        if !self.output_stack.is_empty() {
            let last_idx = self.output_stack.len() - 1;
            self.output_stack[last_idx] = frame;
        }

        Some(info)
    }

    /// Step the clvm by one tick, resulting in possibly new information and
    /// possibly a stack change.
    pub fn step(&mut self, log: Rc<dyn ILogWriter>) -> Option<BTreeMap<String, String>> {
        // Clear breakpoint.
        self.at_breakpoint = None;

        loop {
            if self.run.is_ended() {
                return None;
            }

            match self.run.step() {
                Ok(HierarchialStepResult::ShapeChange) => {
                    // Nothing.
                }
                Ok(HierarchialStepResult::Info(Some(info))) => {
                    if self.run.running.is_empty() {
                        return None;
                    }

                    let frame_idx = self.run.running.len() - 1;
                    let frame = &self.run.running[frame_idx];
                    let step = frame.run.current_step();
                    let frame_hex_string =
                        Bytes::new(Some(BytesFromType::Raw(frame.function_hash.clone()))).hex();

                    for (_translation_unit, breakpoints) in self.breakpoints.iter() {
                        if let Some(bp) = breakpoints.get(&frame_hex_string) {
                            // Break here.
                            log.log(&format!("reached breakpoint {:?}", bp.spec));
                            self.at_breakpoint = Some(bp.spec.clone());
                        }
                    }

                    if let Some(result) = info.get("Value") {
                        self.result = Some(result.clone());
                    }

                    return self.real_step(
                        StoredScope {
                            scope_id: (frame_idx + 1) as u32,
                            name: frame.function_name.clone(),

                            source: step.loc(),
                            named_args: frame.named_args.clone(),
                            rundata: Some(info.clone()),
                        },
                        info,
                    );
                }
                Ok(HierarchialStepResult::Info(None)) => {
                    // Nothing
                }
                Ok(HierarchialStepResult::Done(Some(_))) => {
                    // I don't think anything is needed.
                }
                Ok(HierarchialStepResult::Done(None)) => {
                    // Nothing
                }
                Err(RunFailure::RunErr(l, e)) => {
                    println!("Runtime Error: {l}: {e}");
                    // Nothing
                }
                Err(RunFailure::RunExn(l, e)) => {
                    println!("Raised exception: {l}: {e}");
                    // Nothing
                }
            }
        }
    }
}

fn is_mod(sexp: Rc<SExp>) -> bool {
    if let SExp::Cons(_, a, _) = sexp.borrow() {
        if let SExp::Atom(_, n) = a.borrow() {
            n == b"mod"
        } else {
            false
        }
    } else {
        false
    }
}

fn is_hex_file(filedata: &[u8]) -> bool {
    filedata.iter().all(|b| {
        b.is_ascii_whitespace()
            || (*b >= b'0' && *b <= b'9')
            || (*b >= b'a' && *b <= b'f')
            || (*b >= b'A' && *b <= b'F')
    })
}

// Try to cut the filename up at dots and find a matching .sym file to some
// prefix of the components.
// fact.clvm.hex -> fact.clvm.hex.sym, fact.clvm.sym, fact.sym
// Stop at the proper file name.
// Return the name of the located file and the content.
fn try_locate_related_file(
    fs: Rc<dyn IFileReader>,
    fname: &str,
    new_ext: &str,
) -> Option<(String, Vec<u8>)> {
    let path_components = PathBuf::from(fname);
    if let (Some(directory), Some(only_filename)) =
        (path_components.parent(), path_components.file_name())
    {
        if let Some(rust_str_filename) = only_filename.to_str() {
            let filename_bytes = rust_str_filename.as_bytes().to_vec();
            let mut dots: Vec<usize> = filename_bytes
                .iter()
                .copied()
                .enumerate()
                .filter(|(_, ch)| *ch == b'.')
                .map(|(i, _)| i)
                .collect();
            dots.push(only_filename.len());
            for d in dots.iter() {
                let mut synthesized_filename: Vec<u8> =
                    filename_bytes.iter().copied().take(*d).collect();
                synthesized_filename.append(&mut new_ext.as_bytes().to_vec());
                let synth_fname = PathBuf::from(&decode_string(&synthesized_filename));

                if let Some(total_filename) = directory.join(synth_fname).to_str() {
                    if let Ok(content) = fs.read_content(total_filename) {
                        return Some((total_filename.to_owned(), content.as_bytes().to_vec()));
                    }
                }
            }
        }
    }

    None
}

fn try_locate_symbols(fs: Rc<dyn IFileReader>, fname: &str) -> Option<(String, Vec<u8>)> {
    try_locate_related_file(fs, fname, ".sym")
}

fn try_locate_source_file(fs: Rc<dyn IFileReader>, fname: &str) -> Option<(String, Vec<u8>)> {
    for ext in vec![".clsp", ".clvm"].iter() {
        if let Some(res) = try_locate_related_file(fs.clone(), fname, ext) {
            return Some(res);
        }
    }

    None
}

pub struct RunStartData {
    pub source_file: String,
    pub program: Rc<SExp>,
    pub program_lines: Vec<String>,
    pub arguments: Rc<SExp>,
    pub symbols: HashMap<String, String>,
    // is_hex: bool, // Future: if tools need to know this. (clippy)
    pub compiled: Option<CompileForm>,
}

#[derive(Clone, Debug)]
enum SrclocParseAction {
    ReadingFileName,
    ReadingLineNumber(usize, usize),
    AfterLineNumber(usize, usize),
    ReadingColumn(usize, usize, usize, Option<usize>),
}

struct ParsedSrclocPart {
    file: Vec<u8>,
    line: usize,
    col: usize,
    ext: Option<usize>,
}

/// A simple parser for srcloc for recovering them from messages and symbols.
pub fn parse_srcloc(s: &str) -> Option<Srcloc> {
    let parse_one_loc = |skip| {
        let mut parse_state = SrclocParseAction::ReadingFileName;
        for (i, ch) in s.as_bytes().iter().skip(skip).copied().enumerate() {
            match (&parse_state, ch) {
                (SrclocParseAction::ReadingFileName, b'(') => {
                    parse_state = SrclocParseAction::ReadingLineNumber(i, 0);
                }
                (SrclocParseAction::ReadingFileName, _) => {}
                (SrclocParseAction::ReadingLineNumber(eof, l), b')') => {
                    parse_state = SrclocParseAction::AfterLineNumber(*eof, *l);
                }
                (SrclocParseAction::ReadingLineNumber(eof, l), ch) => {
                    if !ch.is_ascii_digit() {
                        return None;
                    }
                    parse_state = SrclocParseAction::ReadingLineNumber(
                        *eof,
                        10 * *l + ((ch - b'0') as usize),
                    );
                }
                (SrclocParseAction::AfterLineNumber(eof, l), b':') => {
                    parse_state = SrclocParseAction::ReadingColumn(*eof, *l, 0, None);
                }
                (SrclocParseAction::AfterLineNumber(_, _), _) => {
                    return None;
                }
                (SrclocParseAction::ReadingColumn(eof, l, c, _), b'-') => {
                    parse_state = SrclocParseAction::ReadingColumn(*eof, *l, *c, Some(i + 1));
                }
                (SrclocParseAction::ReadingColumn(eof, l, c, e), ch) => {
                    if !ch.is_ascii_digit() {
                        return None;
                    }
                    parse_state = SrclocParseAction::ReadingColumn(
                        *eof,
                        *l,
                        10 * *c + ((ch - b'0') as usize),
                        *e,
                    );
                }
            }
        }

        if let SrclocParseAction::ReadingColumn(f, line, col, ext) = parse_state {
            Some(ParsedSrclocPart {
                file: s.as_bytes().iter().copied().take(f).collect(),
                line,
                col,
                ext,
            })
        } else {
            None
        }
    };

    parse_one_loc(0).and_then(|parsed| {
        let filename_rc = Rc::new(decode_string(&parsed.file));
        let loc = Srcloc::new(filename_rc.clone(), parsed.line, parsed.col);
        if let Some(ext) = parsed.ext {
            parse_one_loc(ext).map(|second| {
                if second.file != parsed.file {
                    // Incomplete range, treat the head as a marker.
                    return loc;
                }
                loc.ext(&Srcloc::new(filename_rc, second.line, second.col))
            })
        } else {
            Some(loc)
        }
    })
}

/// Try to obtain anything we're able to locate related to the chialisp program
/// or clvm hex that was launched.
pub fn read_program_data(
    fs: Rc<dyn IFileReader>,
    log: Rc<dyn ILogWriter>,
    allocator: &mut Allocator,
    opts: Rc<dyn CompilerOpts>,
    _i: &InitializeRequestArguments,
    _l: &LaunchRequestArguments,
    name: &str,
    read_in_file: &[u8],
) -> Result<RunStartData, String> {
    let program_lines: Vec<String> = read_in_file.lines().map(|x| x.unwrap()).collect();
    let parse_err_map = |e: (Srcloc, String)| format!("{}: {}", e.0, e.1);
    let compile_err_map = |e: CompileErr| format!("{}: {}", e.0, e.1);
    let run_err_map = |e: RunFailure| {
        let res = match e {
            RunFailure::RunErr(l, e) => format!("{l}: {e}"),
            RunFailure::RunExn(l, v) => format!("{l}: exception {v}"),
        };
        log.log(&format!("runfailure {res}"));
        res
    };

    let mut use_symbol_table = HashMap::new();
    let is_hex = is_hex_file(read_in_file);

    let mut compiled = None;
    if let Some((source_file, source_content)) = try_locate_source_file(fs.clone(), name) {
        let source_parsed = parse_sexp(Srcloc::start(&source_file), source_content.iter().copied())
            .map_err(parse_err_map)?;

        let frontend_compiled = frontend(opts.clone(), &source_parsed).map_err(compile_err_map)?;

        compiled = Some(frontend_compiled);
    }

    let mut parsed_program = if is_hex {
        let prog_srcloc = Srcloc::start(name);

        // Synthesize content by disassembling the file.
        use_symbol_table = if let Some((symfile, symdata)) = try_locate_symbols(fs.clone(), name) {
            serde_json::from_str(&decode_string(&symdata))
                .map_err(|_| format!("Failure decoding symbols from {symfile}"))?
        } else {
            HashMap::new()
        };

        hex_to_modern_sexp(
            allocator,
            &use_symbol_table,
            prog_srcloc,
            &decode_string(read_in_file),
        )
        .map_err(run_err_map)?
    } else {
        let parsed =
            parse_sexp(Srcloc::start(name), read_in_file.iter().copied()).map_err(parse_err_map)?;

        if parsed.is_empty() {
            return Err(format!("Empty program file {name}"));
        }

        parsed[0].clone()
    };

    if is_mod(parsed_program.clone()) {
        // Compile program.
        let clvm_res = compile_clvm_text(
            allocator,
            opts.clone(),
            &mut use_symbol_table,
            &decode_string(read_in_file),
            name,
            true,
        )
        .map_err(|e| {
            let formatted = match e {
                CompileError::Classic(_x, y) => y,
                CompileError::Modern(l, v) => format!("{l}: {v}"),
            };
            log.log(&format!("error compiling: {formatted}"));
            formatted
        })?;
        let bin = sexp_as_bin(allocator, clvm_res).hex();
        parsed_program =
            hex_to_modern_sexp(allocator, &use_symbol_table, Srcloc::start(name), &bin)
                .map_err(run_err_map)?;
    };

    let arguments = Rc::new(SExp::Nil(parsed_program.loc()));

    Ok(RunStartData {
        source_file: name.to_owned(),
        program: parsed_program,
        program_lines,
        arguments,
        symbols: use_symbol_table,
        // is_hex,
        compiled,
    })
}
