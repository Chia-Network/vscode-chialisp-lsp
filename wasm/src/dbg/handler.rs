use serde::Deserialize;

use std::borrow::Borrow;
use std::collections::{BTreeMap, HashMap};
use std::io::BufRead;
use std::mem::swap;
use std::path::PathBuf;
use std::rc::Rc;

use debug_types::events::{ContinuedEvent, Event, EventBody, StoppedEvent, StoppedReason};
use debug_types::requests::{
    InitializeRequestArguments, LaunchRequestArguments, RequestCommand, SourceArguments,
};
use debug_types::responses::{
    InitializeResponse, Response, ResponseBody, ScopesResponse, SetBreakpointsResponse,
    SetExceptionBreakpointsResponse, SourceResponse, StackTraceResponse, ThreadsResponse,
    VariablesResponse,
};
use debug_types::types::{
    Breakpoint, Capabilities, ChecksumAlgorithm, Scope, Source, SourceBreakpoint, StackFrame,
    Thread, Variable,
};
use debug_types::{MessageKind, ProtocolMessage};

use clvmr::allocator::Allocator;

use clvm_tools_rs::classic::clvm::__type_compatibility__::{Bytes, BytesFromType};
use clvm_tools_rs::classic::clvm::sexp::sexp_as_bin;
use clvm_tools_rs::classic::clvm_tools::clvmc::compile_clvm_text;
use clvm_tools_rs::classic::clvm_tools::clvmc::CompileError;
use clvm_tools_rs::classic::clvm_tools::stages::stage_0::TRunProgram;
use clvm_tools_rs::classic::platform::argparse::ArgumentValue;
use clvm_tools_rs::classic::clvm_tools::comp_input::RunAndCompileInputData;

use clvm_tools_rs::compiler::cldb::hex_to_modern_sexp;
use clvm_tools_rs::compiler::cldb_hierarchy::{
    HierarchialRunner, HierarchialStepResult, RunPurpose,
};
use clvm_tools_rs::compiler::compiler::DefaultCompilerOpts;
use clvm_tools_rs::compiler::comptypes::{CompileErr, CompileForm, CompilerOpts};
use clvm_tools_rs::compiler::frontend::frontend;
use clvm_tools_rs::compiler::runtypes::RunFailure;
use clvm_tools_rs::compiler::sexp::{decode_string, parse_sexp, SExp};
use clvm_tools_rs::compiler::srcloc::Srcloc;

use crate::dbg::compopts::DbgCompilerOpts;
use crate::dbg::types::{DebuggerInputs, DebuggerSourceAndContent, MessageHandler, ProgramKind};
#[cfg(test)]
use crate::interfaces::EPrintWriter;
use crate::interfaces::{IFileReader, ILogWriter};
use crate::lsp::types::{ConfigJson, DocPosition, DocRange};

// Lifecycle:
// (a (code... ) (c arg ...))
// We encounter this and we're creating the arguments for a future call to
// (code ...)
// We enter (code ...) proper via the OpResult of (a ... args) and can fish the
// arguments.  After that, we're running the subfunction.

const LARGE_COLUMN: u32 = 100000;

/// Things which are in the launch request in practice but are not part of the
/// documented minimal interface to this data.  Since these fields are ad-hoc
/// we define a structure here that they'll decode into when tried with serde_json.
#[derive(Clone, Debug, Deserialize)]
pub struct ExtraLaunchData {
    #[serde(rename = "stopOnEntry")]
    stop_on_entry: Option<bool>,
    #[serde(rename = "args")]
    args: Option<Vec<String>>,
    #[serde(rename = "program")]
    program: Option<String>,
}

/// Used to make some aspects of deserializing easier via serde_json easier.
/// This container allows {"arguments":{ ... t-like thing ...}} to decode if
/// t-like thing decodes.
#[derive(Clone, Debug, Deserialize)]
pub struct RequestContainer<T> {
    arguments: T,
}

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
    scope_id: u32,

    name: String,
    named_args: HashMap<String, Rc<SExp>>,

    rundata: Option<BTreeMap<String, String>>,

    source: Srcloc,
}

#[derive(Clone, Debug)]
struct RecognizedBreakpoint {
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
    target_depth: Option<TargetDepth>,
    stopped_reason: Option<StoppedReason>,

    source_file: String,
    compiled: Option<CompileForm>,

    running: bool,
    run: HierarchialRunner,
    symbols: Rc<HashMap<String, String>>,
    output_stack: Vec<StoredScope>,
    result: Option<String>,

    breakpoints: HashMap<String, HashMap<String, RecognizedBreakpoint>>,
    next_bp_id: usize,
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
    fn get_source_file(
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
    fn set_breakpoints(
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

    fn get_stack_depth(&self) -> usize {
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
    fn step(&mut self, log: Rc<dyn ILogWriter>) -> Option<BTreeMap<String, String>> {
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

/// State diagram for the debugger.
///
/// Starts in PreInitialization until we get an Initialization request.
/// A few messages are documented to be interpretable in the Initialized state
/// but we don't know what we're debugging until we get to launched.  I don't
/// yet know what the expected actions are in every case.
///
/// Transitions from Initialized to Launched when we receive a launch request.
/// The launch request contains information that isn't documented in the official
/// message docs.  We decode it below as:
///
///   launch_extra: Option<RequestContainer<ExtraLaunchData>>
///
/// Which puts together the right combination of path and fields to retrieve the
/// information we use from launch.json.
///
/// Given all that info, we try to read the main file, detect whether it's hex,
/// try to find the chialisp that goes with it and try to find the symbols.
/// once all that's found we create a RunningDebugger and set our state to
/// Launched(RunningDebugger) which we mutate throughout the process.
pub enum State {
    PreInitialization,
    Initialized(InitializeRequestArguments),
    Launched(RunningDebugger),
}

impl State {
    fn state_name(&self) -> String {
        match self {
            State::PreInitialization => "PreInitialization",
            State::Initialized(_) => "Initialized",
            State::Launched(_) => "Launched",
        }
        .to_string()
    }
}

/// The main object that controls the debug process.  It contains the state, the
/// global objects (fs and logger) as well as program to run, clvm runner and
/// other bits needed.
///
/// State contains a mutated RunningDebugger on every state.
pub struct Debugger {
    // External interface
    pub fs: Rc<dyn IFileReader>,
    pub log: Rc<dyn ILogWriter>,

    pub state: State,
    // We'll store a short program here for how to run the target program.
    pub expression: Option<Rc<SExp>>,

    pub runner: Rc<dyn TRunProgram>,
    pub prim_map: Rc<HashMap<Vec<u8>, Rc<SExp>>>,
    pub msg_seq: i64,
}

impl Debugger {
    pub fn new(
        fs: Rc<dyn IFileReader>,
        log: Rc<dyn ILogWriter>,
        runner: Rc<dyn TRunProgram>,
        prim_map: Rc<HashMap<Vec<u8>, Rc<SExp>>>,
    ) -> Self {
        Debugger {
            fs,
            log,
            state: State::PreInitialization,
            expression: None,
            runner,
            prim_map,
            msg_seq: 0,
        }
    }
}

fn get_initialize_response() -> InitializeResponse {
    InitializeResponse {
        capabilities: Capabilities {
            supports_configuration_done_request: None,
            supports_function_breakpoints: None,
            supports_conditional_breakpoints: None,
            supports_hit_conditional_breakpoints: None,
            supports_evaluate_for_hovers: None,
            exception_breakpoint_filters: None,
            supports_step_back: Some(true),
            supports_goto_targets_request: None,
            supports_step_in_targets_request: None,
            supports_completions_request: None,
            supports_modules_request: None,
            completion_trigger_characters: None,
            additional_module_columns: None,
            supported_checksum_algorithms: Some(vec![ChecksumAlgorithm::SHA256]),
            support_suspend_debuggee: None,
            support_terminate_debuggee: Some(true),
            supports_breakpoint_locations_request: Some(true),
            supports_cancel_request: None,
            supports_clipboard_context: None,
            supports_data_breakpoints: None,
            supports_delayed_stack_trace_loading: None,
            supports_disassemble_request: Some(true),
            supports_exception_filter_options: None,
            supports_exception_info_request: None,
            supports_exception_options: None,
            supports_instruction_breakpoints: None,
            supports_loaded_sources_request: Some(true),
            supports_log_points: Some(true),
            supports_read_memory_request: None,
            supports_restart_frame: None,
            supports_restart_request: Some(true),
            supports_set_expression: None,
            supports_set_variable: None,
            supports_single_thread_execution_requests: None,
            supports_stepping_granularity: None,
            supports_terminate_request: Some(true),
            supports_terminate_threads_request: None,
            supports_value_formatting_options: Some(true),
            supports_write_memory_request: None,
        },
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

struct RunStartData {
    source_file: String,
    program: Rc<SExp>,
    program_lines: Vec<String>,
    arguments: Rc<SExp>,
    symbols: HashMap<String, String>,
    // is_hex: bool, // Future: if tools need to know this. (clippy)
    compiled: Option<CompileForm>,
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

struct LaunchArgs<'a> {
    proto_msg: &'a ProtocolMessage,
    name: &'a str,
    init_args: &'a InitializeRequestArguments,
    launch_request: &'a LaunchRequestArguments,
    program: &'a str,
    args_for_program: &'a [String],
    stop_on_entry: bool,
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

impl Debugger {
    fn get_source_loc(&self, running: &RunningDebugger, name: &str) -> Option<Srcloc> {
        let get_helper_loc = |name: &str| {
            if let Some(compiled) = running.compiled.as_ref() {
                for h in compiled.helpers.iter() {
                    if decode_string(h.name()) == name {
                        return Some(h.loc());
                    }
                }
            }
            None
        };

        get_helper_loc(name)
            .map(Some)
            .unwrap_or_else(|| parse_srcloc(name))
    }

    fn get_scope_source(&self, running: &RunningDebugger, scope: &StoredScope) -> Option<Source> {
        let source_from_loc = |loc: Srcloc| {
            let borrowed_filename: &String = loc.file.borrow();
            Some(Source {
                adapter_data: None,
                checksums: None,
                name: Some(borrowed_filename.clone()),
                path: None,
                origin: None,
                presentation_hint: None,
                source_reference: None,
                sources: None,
            })
        };
        self.get_source_loc(running, &scope.name)
            .and_then(source_from_loc)
    }

    /// Try to read chialisp.json from the workspace root via the filesystem
    /// abstraction.
    fn read_chialisp_json(&self) -> Result<ConfigJson, String> {
        let chialisp_json_content = self.fs.read_content("chialisp.json")?;
        let decoded_chialisp_json: ConfigJson = serde_json::from_str(&chialisp_json_content)
            .map_err(|_| "error decoding chialisp.json".to_string())?;
        Ok(decoded_chialisp_json)
    }

    /// Try to obtain anything we're able to locate related to the chialisp program
    /// or clvm hex that was launched.
    fn read_program_data(
        &self,
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
            self.log.log(&format!("runfailure {res}"));
            res
        };

        let mut use_symbol_table = HashMap::new();
        let mut inputs = DebuggerInputs {
            is_hex: is_hex_file(read_in_file),
            source: None,
            compile_input: None,
            symbols: None,
            compiled: Err("no program read yet".to_string()),
        };

        if let Some((source_file, source_content)) = try_locate_source_file(self.fs.clone(), name) {
            let source_parsed =
                parse_sexp(Srcloc::start(&source_file), source_content.iter().copied())
                    .map_err(parse_err_map)?;

            let source_and_content = DebuggerSourceAndContent {
                source_file,
                source_content,
                source_parsed,
            };

            let mut compile_input_args = HashMap::new();
            compile_input_args.insert(
                "path_or_code".to_string(),
                ArgumentValue::ArgString(
                    Some(source_and_content.source_file.clone()),
                    decode_string(&source_and_content.source_content)
                )
            );
            // We don't get this info explicitly at this point.  We will grab it downstream when
            // we receive a better view of the launch request.
            compile_input_args.insert("env".to_string(), ArgumentValue::ArgString(None, "()".to_string()));
            if !inputs.is_hex {
                inputs.compile_input = Some(RunAndCompileInputData::new(
                    allocator,
                    &compile_input_args
                )?);
            };

            let frontend_compiled =
                frontend(opts.clone(), &source_and_content.source_parsed).map_err(compile_err_map)?;

            inputs.source = Some(source_and_content);
            inputs.compiled = Ok(ProgramKind::FromModern(frontend_compiled));
        }

        let mut parsed_program = if inputs.is_hex {
            let prog_srcloc = Srcloc::start(name);

            // Synthesize content by disassembling the file.
            use_symbol_table =
                if let Some((symfile, symdata)) = try_locate_symbols(self.fs.clone(), name) {
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
            let parsed = parse_sexp(Srcloc::start(name), read_in_file.iter().copied())
                .map_err(parse_err_map)?;

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
                self.log.log(&format!("error compiling: {formatted}"));
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
            compiled: match &inputs.compiled {
                Err(_) => None,
                Ok(ProgramKind::FromHex(sexp)) => { None },
                Ok(ProgramKind::FromClassic(node)) => { None },
                Ok(ProgramKind::FromModern(cf)) => { Some(cf.clone()) },
            }
        })
    }

    /// Given a launch command and the extra data it receives from launch.json,
    /// try to locate enough pieces to run a debug session.
    ///
    /// The result is the new message seq number for outgoing messages, the
    /// state to be transitioned to and a list of initial replies and events
    /// to be sent to the consumer.  After sending the messages and updating
    /// the state, we're synchronized to communicate with the consumer.
    fn launch(
        &self,
        launch_args: LaunchArgs,
    ) -> Result<(i64, State, Vec<ProtocolMessage>), String> {
        let mut allocator = Allocator::new();
        let mut seq_nr = self.msg_seq;
        let config = self.read_chialisp_json()?;
        let read_in_file = self.fs.read_content(launch_args.program)?;
        let def_opts = Rc::new(DefaultCompilerOpts::new(&launch_args.name));
        let opts = Rc::new(DbgCompilerOpts::new(
            def_opts,
            self.log.clone(),
            self.fs.clone(),
            &config.include_paths,
        ));
        let mut launch_data = self.read_program_data(
            &mut allocator,
            opts.clone(),
            launch_args.init_args,
            launch_args.launch_request,
            launch_args.program,
            read_in_file.as_bytes(),
        )?;

        if !launch_args.args_for_program.is_empty() {
            let parsed_argv0 = parse_sexp(
                Srcloc::start("*args*"),
                launch_args.args_for_program[0].bytes(),
            )
            .map_err(|(l, e)| format!("{l}: {e}"))?;
            if !parsed_argv0.is_empty() {
                launch_data.arguments = parsed_argv0[0].clone();
            }
        }

        let symbol_rc = Rc::new(launch_data.symbols);

        let run = HierarchialRunner::new(
            self.runner.clone(),
            self.prim_map.clone(),
            Some(launch_args.name.to_string()),
            Rc::new(launch_data.program_lines),
            symbol_rc.clone(),
            launch_data.program,
            launch_data.arguments,
        );
        let state = State::Launched(RunningDebugger {
            initialized: launch_args.init_args.clone(),
            launch_info: launch_args.launch_request.clone(),
            running: !launch_args.stop_on_entry,
            run,
            opts,
            output_stack: Vec::new(),
            stopped_reason: None,
            target_depth: None,
            result: None,
            source_file: launch_data.source_file,
            compiled: launch_data.compiled,
            breakpoints: HashMap::new(),
            next_bp_id: 1,
            at_breakpoint: None,
            symbols: symbol_rc,
        });

        seq_nr += 1;
        let mut out_messages = vec![ProtocolMessage {
            seq: self.msg_seq,
            message: MessageKind::Response(Response {
                request_seq: launch_args.proto_msg.seq,
                success: true,
                message: None,
                body: Some(ResponseBody::Launch),
            }),
        }];

        // Signal that we're paused if stop on entry.
        if launch_args.stop_on_entry {
            seq_nr += 1;
            out_messages.push(ProtocolMessage {
                seq: self.msg_seq,
                message: MessageKind::Event(Event {
                    body: Some(EventBody::Stopped(StoppedEvent {
                        reason: StoppedReason::Entry,
                        description: None,
                        thread_id: Some(1),
                        preserve_focus_hint: None, // Some(true),
                        text: None,
                        all_threads_stopped: Some(true),
                        hit_breakpoint_ids: None,
                    })),
                }),
            });
        }

        Ok((seq_nr, state, out_messages))
    }
}

impl MessageHandler<ProtocolMessage> for Debugger {
    /// Handle a message from the protocol state when one is decoded.  We're given
    /// the raw json as well so we can fish things out of it that are nonstandard.
    ///
    /// This is mutable on Debugger, so it updates the condition of Debugger.
    fn handle_message(
        &mut self,
        raw_json: &serde_json::Value,
        pm: &ProtocolMessage,
    ) -> Result<Option<Vec<ProtocolMessage>>, String> {
        let mut state = State::PreInitialization;
        self.log.log(&format!(
            "got message {}",
            serde_json::to_string(pm).unwrap()
        ));

        swap(&mut state, &mut self.state);

        if let MessageKind::Request(req) = &pm.message {
            // This is a big switch for being in some state and receiving a
            // protocol message.  We match the pair of them to determine what
            // to do.  If unmatched, we'll not change state, log an error and
            // return a failure to the consumer.
            match (state, req) {
                (State::PreInitialization, RequestCommand::Initialize(irq)) => {
                    self.state = State::Initialized(irq.clone());
                    self.msg_seq += 2;
                    return Ok(Some(vec![
                        ProtocolMessage {
                            seq: self.msg_seq - 1,
                            message: MessageKind::Response(Response {
                                request_seq: pm.seq,
                                success: true,
                                message: None,
                                body: Some(ResponseBody::Initialize(get_initialize_response())),
                            }),
                        },
                        ProtocolMessage {
                            seq: self.msg_seq,
                            message: MessageKind::Event(Event {
                                body: Some(EventBody::Initialized),
                            }),
                        },
                    ]));
                }
                (State::Initialized(i), RequestCommand::Launch(l)) => {
                    let launch_extra: Option<RequestContainer<ExtraLaunchData>> =
                        serde_json::from_value(raw_json.clone())
                            .map(Some)
                            .unwrap_or(None);

                    self.log.log(&format!("launch extra {launch_extra:?}"));
                    let stop_on_entry = launch_extra
                        .as_ref()
                        .and_then(|l| l.arguments.stop_on_entry)
                        .unwrap_or(true);
                    let args = launch_extra
                        .as_ref()
                        .and_then(|l| l.arguments.args.clone())
                        .unwrap_or(vec![]);
                    if let Some(name) = &l.name {
                        let program = launch_extra
                            .and_then(|l| l.arguments.program)
                            .unwrap_or(name.clone());

                        let (new_seq, new_state, out_msgs) = self.launch(LaunchArgs {
                            proto_msg: pm,
                            name,
                            init_args: &i,
                            launch_request: l,
                            program: &program,
                            args_for_program: &args,
                            stop_on_entry,
                        })?;
                        self.msg_seq = new_seq;
                        self.state = new_state;

                        return Ok(Some(out_msgs));
                    } else {
                        self.state = State::Initialized(i);
                        self.log.log("No program provided");
                    }
                }

                // Pre initialization: recognize and respond to the breakpoint
                // request (but that's all we can do now).
                (State::Initialized(i), RequestCommand::SetBreakpoints(_b)) => {
                    self.msg_seq += 1;
                    self.state = State::Initialized(i);
                    return Ok(Some(vec![ProtocolMessage {
                        seq: self.msg_seq,
                        message: MessageKind::Response(Response {
                            request_seq: pm.seq,
                            success: false,
                            message: Some("nothing loaded yet".to_string()),
                            body: None,
                        }),
                    }]));
                }

                // ProtocolMessage { seq: 8, message: Request(SetBreakpoints(SetBreakpointsArguments { source: Source { name: Some("fact.clsp"), path: Some("/home/arty/dev/chia/clvm_tools_rs/fact.clsp"), source_reference: None, presentation_hint: None, origin: None, sources: None, adapter_data: None, checksums: None }, breakpoints: Some([SourceBreakpoint { line: 2, column: Some(4), condition: None, hit_condition: None, log_message: None }]), lines: Some([2]), source_modified: Some(false) })) }
                // Set breakpoints from source.  Requires advertised capability in
                // package.json.
                (State::Launched(mut r), RequestCommand::SetBreakpoints(b)) => {
                    self.msg_seq += 1;
                    let result_breakpoints = r.set_breakpoints(
                        self.log.clone(),
                        &b.source,
                        b.breakpoints.clone().unwrap_or(vec![]),
                    );
                    self.state = State::Launched(r);
                    return Ok(Some(vec![ProtocolMessage {
                        seq: self.msg_seq,
                        message: MessageKind::Response(Response {
                            request_seq: pm.seq,
                            success: true,
                            message: None,
                            body: Some(ResponseBody::SetBreakpoints(SetBreakpointsResponse {
                                breakpoints: result_breakpoints,
                            })),
                        }),
                    }]));
                }
                // The way the code in debugServer is structured, if no other
                // breakpoints of a supported type are sent, an empty,
                // SetExceptionBreakpoints will be sent.  We don't advertise
                // support for this type so we can send an empty response.
                (State::Launched(r), RequestCommand::SetExceptionBreakpoints(_b)) => {
                    self.msg_seq += 1;
                    self.state = State::Launched(r);
                    return Ok(Some(vec![ProtocolMessage {
                        seq: self.msg_seq,
                        message: MessageKind::Response(Response {
                            request_seq: pm.seq,
                            success: true,
                            message: None,
                            body: Some(ResponseBody::SetExceptionBreakpoints(
                                SetExceptionBreakpointsResponse {
                                    breakpoints: Some(vec![]),
                                },
                            )),
                        }),
                    }]));
                }
                (State::Launched(r), RequestCommand::Source(s)) => {
                    self.msg_seq += 1;

                    let source_lookup = r.get_source_file(self.log.clone(), s);
                    self.state = State::Launched(r);

                    return Ok(Some(vec![ProtocolMessage {
                        seq: self.msg_seq,
                        message: MessageKind::Response(Response {
                            request_seq: pm.seq,
                            success: true,
                            message: None,
                            body: source_lookup,
                        }),
                    }]));
                }
                (State::Launched(r), RequestCommand::Threads) => {
                    self.msg_seq += 1;
                    self.state = State::Launched(r);

                    return Ok(Some(vec![ProtocolMessage {
                        seq: self.msg_seq,
                        message: MessageKind::Response(Response {
                            request_seq: pm.seq,
                            success: true,
                            message: None,
                            body: Some(ResponseBody::Threads(ThreadsResponse {
                                threads: vec![Thread {
                                    id: 1,
                                    name: "main".to_string(),
                                }],
                            })),
                        }),
                    }]));
                }
                (State::Launched(r), RequestCommand::StackTrace(_)) => {
                    let stack_frames: Vec<StackFrame> = r
                        .output_stack
                        .iter()
                        .rev()
                        .map(|s| {
                            let loc = self
                                .get_source_loc(&r, &s.name)
                                .unwrap_or_else(|| s.source.clone());
                            let filename_borrowed: &String = loc.file.borrow();
                            StackFrame {
                                id: s.scope_id as i32,
                                name: filename_borrowed.clone(),
                                source: self.get_scope_source(&r, s),
                                line: loc.line as u32,
                                column: loc.col as u32,
                                end_line: None,
                                end_column: None,
                                can_restart: Some(true),
                                instruction_pointer_reference: None,
                                module_id: None,
                                presentation_hint: None,
                            }
                        })
                        .collect();

                    self.msg_seq += 1;
                    self.state = State::Launched(r);

                    return Ok(Some(vec![ProtocolMessage {
                        seq: self.msg_seq,
                        message: MessageKind::Response(Response {
                            request_seq: pm.seq,
                            success: true,
                            message: None,
                            body: Some(ResponseBody::StackTrace(StackTraceResponse {
                                stack_frames,
                                total_frames: None,
                            })),
                        }),
                    }]));
                }
                (State::Launched(r), RequestCommand::Variables(vreq)) => {
                    let s: Vec<StoredScope> = r
                        .output_stack
                        .iter()
                        .filter(|s| s.scope_id as i32 == vreq.variables_reference)
                        .cloned()
                        .collect();

                    let mut variables = Vec::new();

                    if let Some(scope) = s.get(0) {
                        let mut function_args = scope
                            .named_args
                            .iter()
                            .map(|(k, v)| Variable {
                                indexed_variables: None,
                                named_variables: None,
                                presentation_hint: None,
                                value: v.to_string(),
                                var_type: None,
                                variables_reference: -1,
                                memory_reference: None,
                                evaluate_name: Some(format!("{}:{}", scope.scope_id, k.clone())),
                                name: k.clone(),
                            })
                            .collect();
                        variables.append(&mut function_args);
                        let mut copy_info_member = |name, target: &str| {
                            if let Some(v) = scope.rundata.as_ref().and_then(|info| info.get(name))
                            {
                                variables.push(Variable {
                                    name: target.to_owned(),
                                    indexed_variables: None,
                                    named_variables: None,
                                    presentation_hint: None,
                                    value: v.to_string(),
                                    var_type: None,
                                    variables_reference: -1,
                                    memory_reference: None,
                                    evaluate_name: None,
                                });
                            }
                        };

                        copy_info_member("Function", "_op");
                        copy_info_member("Arguments", "_args");
                        copy_info_member("Failure", "_failure");

                        if let Some(result) = r.result.as_ref() {
                            variables.push(Variable {
                                name: "_result".to_owned(),
                                indexed_variables: None,
                                named_variables: None,
                                presentation_hint: None,
                                value: result.clone(),
                                var_type: None,
                                variables_reference: -1,
                                memory_reference: None,
                                evaluate_name: None,
                            });
                        }
                    };

                    self.msg_seq += 1;
                    self.state = State::Launched(r);

                    return Ok(Some(vec![ProtocolMessage {
                        seq: self.msg_seq,
                        message: MessageKind::Response(Response {
                            request_seq: pm.seq,
                            success: true,
                            message: None,
                            body: Some(ResponseBody::Variables(VariablesResponse { variables })),
                        }),
                    }]));
                }
                (State::Launched(r), RequestCommand::Scopes(sreq)) => {
                    let scopes = r
                        .output_stack
                        .iter()
                        .filter(|s| s.scope_id as i32 == sreq.frame_id)
                        .map(|s| Scope {
                            name: s.name.clone(),
                            column: None,
                            end_column: None,
                            line: None,
                            end_line: None,
                            expensive: false,
                            indexed_variables: None,
                            named_variables: Some(s.named_args.len()),
                            source: self.get_scope_source(&r, s),
                            variables_reference: s.scope_id as i32,
                            presentation_hint: None,
                        })
                        .collect();

                    self.msg_seq += 1;
                    self.state = State::Launched(r);

                    return Ok(Some(vec![ProtocolMessage {
                        seq: self.msg_seq,
                        message: MessageKind::Response(Response {
                            request_seq: pm.seq,
                            success: true,
                            message: None,
                            body: Some(ResponseBody::Scopes(ScopesResponse { scopes })),
                        }),
                    }]));
                }
                (State::Launched(mut r), RequestCommand::Pause(_)) => {
                    let mut out_messages = Vec::new();

                    self.msg_seq += 1;
                    out_messages.push(ProtocolMessage {
                        seq: self.msg_seq,
                        message: MessageKind::Response(Response {
                            request_seq: pm.seq,
                            success: true,
                            message: None,
                            body: Some(ResponseBody::Pause),
                        }),
                    });

                    r.running = false;
                    r.stopped_reason = None;
                    self.msg_seq += 1;
                    out_messages.push(ProtocolMessage {
                        seq: self.msg_seq,
                        message: MessageKind::Event(Event {
                            body: Some(EventBody::Stopped(StoppedEvent {
                                reason: StoppedReason::Pause,
                                description: None,
                                thread_id: Some(1),
                                preserve_focus_hint: None,
                                text: None,
                                all_threads_stopped: Some(true),
                                hit_breakpoint_ids: None,
                            })),
                        }),
                    });

                    self.state = State::Launched(r);
                    return Ok(Some(out_messages));
                }
                (State::Launched(mut r), RequestCommand::StepIn(si)) => {
                    r.step(self.log.clone());

                    self.msg_seq += 1;

                    let mut out_messages = Vec::new();
                    if si.thread_id != -1 {
                        out_messages.push(ProtocolMessage {
                            seq: self.msg_seq,
                            message: MessageKind::Response(Response {
                                request_seq: pm.seq,
                                success: true,
                                message: None,
                                body: Some(ResponseBody::StepIn),
                            }),
                        });
                    }

                    let stack_depth = r.get_stack_depth();
                    // We should signal stopped if:
                    // - This is an organic step request from vscode or
                    // - We're running and
                    //   - The program ended or
                    //     - The stack depth target was reached.
                    let should_stop = si.thread_id != -1
                        || r.at_breakpoint.is_some()
                        || (r.running
                            && (r.run.is_ended()
                                || match r.target_depth {
                                    None => false,
                                    Some(TargetDepth::LessThan(n)) => stack_depth < n,
                                    Some(TargetDepth::LessOrEqual(n)) => stack_depth <= n,
                                }));

                    // If this message was not synthetic, we should send a
                    // continued event.
                    if si.thread_id != -1 {
                        self.msg_seq += 1;
                        out_messages.push(ProtocolMessage {
                            seq: self.msg_seq,
                            message: MessageKind::Event(Event {
                                body: Some(EventBody::Continued(ContinuedEvent {
                                    thread_id: 1,
                                    all_threads_continued: Some(true),
                                })),
                            }),
                        });
                    }

                    // If we should stop, then we emit a stopped message.
                    if should_stop {
                        r.running = false;
                        r.stopped_reason =
                            r.at_breakpoint.as_ref().map(|_| StoppedReason::Breakpoint);

                        self.msg_seq += 1;
                        out_messages.push(ProtocolMessage {
                            seq: self.msg_seq,
                            message: MessageKind::Event(Event {
                                body: Some(EventBody::Stopped(StoppedEvent {
                                    reason: r
                                        .stopped_reason
                                        .as_ref()
                                        .cloned()
                                        .unwrap_or(StoppedReason::Step),
                                    description: None,
                                    thread_id: Some(1),
                                    preserve_focus_hint: None, // Some(true),
                                    text: None,
                                    all_threads_stopped: Some(true),
                                    hit_breakpoint_ids: None,
                                })),
                            }),
                        });
                    }

                    self.state = State::Launched(r);
                    return Ok(Some(out_messages));
                }
                (State::Launched(mut r), RequestCommand::Next(_)) => {
                    let depth = r.get_stack_depth();

                    let mut out_messages = Vec::new();

                    self.msg_seq += 1;
                    out_messages.push(ProtocolMessage {
                        seq: self.msg_seq,
                        message: MessageKind::Response(Response {
                            request_seq: pm.seq,
                            success: true,
                            message: Some("run next".to_string()),
                            body: Some(ResponseBody::Next),
                        }),
                    });

                    self.msg_seq += 1;
                    out_messages.push(ProtocolMessage {
                        seq: self.msg_seq,
                        message: MessageKind::Event(Event {
                            body: Some(EventBody::Continued(ContinuedEvent {
                                thread_id: 1,
                                all_threads_continued: Some(true),
                            })),
                        }),
                    });

                    r.running = true;
                    r.stopped_reason = Some(StoppedReason::Pause);
                    r.target_depth = Some(TargetDepth::LessOrEqual(depth));
                    self.state = State::Launched(r);

                    return Ok(Some(out_messages));
                }
                (State::Launched(mut r), RequestCommand::StepOut(_)) => {
                    let depth = r.get_stack_depth();
                    let mut out_messages = Vec::new();

                    self.msg_seq += 1;
                    out_messages.push(ProtocolMessage {
                        seq: self.msg_seq,
                        message: MessageKind::Response(Response {
                            request_seq: pm.seq,
                            success: true,
                            message: Some("run step out".to_string()),
                            body: Some(ResponseBody::StepOut),
                        }),
                    });

                    self.msg_seq += 1;
                    out_messages.push(ProtocolMessage {
                        seq: self.msg_seq,
                        message: MessageKind::Event(Event {
                            body: Some(EventBody::Continued(ContinuedEvent {
                                thread_id: 1,
                                all_threads_continued: Some(true),
                            })),
                        }),
                    });

                    r.running = true;
                    r.stopped_reason = Some(StoppedReason::Pause);
                    r.target_depth = Some(TargetDepth::LessThan(depth));
                    self.state = State::Launched(r);

                    return Ok(Some(out_messages));
                }
                (State::Launched(mut r), RequestCommand::Continue(_)) => {
                    let mut out_messages = Vec::new();

                    self.msg_seq += 1;
                    out_messages.push(ProtocolMessage {
                        seq: self.msg_seq,
                        message: MessageKind::Response(Response {
                            request_seq: pm.seq,
                            success: true,
                            message: Some("run continue".to_string()),
                            body: Some(ResponseBody::StepOut),
                        }),
                    });

                    self.msg_seq += 1;
                    out_messages.push(ProtocolMessage {
                        seq: self.msg_seq,
                        message: MessageKind::Event(Event {
                            body: Some(EventBody::Continued(ContinuedEvent {
                                thread_id: 1,
                                all_threads_continued: Some(true),
                            })),
                        }),
                    });

                    r.running = true;
                    r.stopped_reason = Some(StoppedReason::Pause);
                    r.target_depth = None;
                    self.state = State::Launched(r);

                    return Ok(Some(out_messages));
                }
                (st, _rq) => {
                    self.log.log(&format!(
                        "Don't know what to do with {:?} in state {}",
                        req,
                        st.state_name()
                    ));
                    self.state = st;
                    self.msg_seq += 1;

                    self.log.log(&format!("unhandled message {pm:?}"));
                    return Ok(Some(vec![ProtocolMessage {
                        seq: self.msg_seq,
                        message: MessageKind::Response(Response {
                            request_seq: pm.seq,
                            success: false,
                            message: Some(format!("unhandled message {pm:?}")),
                            body: None,
                        }),
                    }]));
                }
            }
        }

        self.log.log(&format!("unhandled message {pm:?}"));
        Err(format!("unhandled message {pm:?}"))
    }
}
