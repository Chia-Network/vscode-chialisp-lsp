use serde::Deserialize;

use std::borrow::Borrow;
use std::collections::{BTreeMap, HashMap};
use std::io::BufRead;
use std::mem::swap;
use std::path::PathBuf;
use std::rc::Rc;

use debug_types::events::{ContinuedEvent, Event, EventBody, StoppedEvent, StoppedReason};
use debug_types::requests::{InitializeRequestArguments, LaunchRequestArguments, RequestCommand, SourceArguments};
use debug_types::responses::{
    InitializeResponse, Response, ResponseBody, ScopesResponse, SourceResponse, StackTraceResponse,
    ThreadsResponse, VariablesResponse,
};
use debug_types::types::{
    Capabilities, ChecksumAlgorithm, Scope, Source, StackFrame, Thread, Variable,
};
use debug_types::{MessageKind, ProtocolMessage};

use clvmr::allocator::Allocator;

use clvm_tools_rs::classic::clvm_tools::stages::stage_0::TRunProgram;
use clvm_tools_rs::compiler::cldb::hex_to_modern_sexp;
use clvm_tools_rs::compiler::cldb_hierarchy::{
    HierarchialRunner, HierarchialStepResult, RunPurpose
};
use clvm_tools_rs::compiler::compiler::{compile_file, DefaultCompilerOpts};
use clvm_tools_rs::compiler::comptypes::{CompileErr, CompileForm, CompilerOpts};
use clvm_tools_rs::compiler::frontend::frontend;
use crate::interfaces::{IFileReader, ILogWriter};
use crate::dbg::types::MessageHandler;
use clvm_tools_rs::compiler::runtypes::RunFailure;
use clvm_tools_rs::compiler::sexp::{decode_string, parse_sexp, SExp};
use clvm_tools_rs::compiler::srcloc::Srcloc;

// Lifecycle:
// (a (code... ) (c arg ...))
// We encounter this and we're creating the arguments for a future call to
// (code ...)
// We enter (code ...) proper via the OpResult of (a ... args) and can fish the
// arguments.  After that, we're running the subfunction.

#[derive(Clone, Debug, Deserialize)]
pub struct ExtraLaunchData {
    #[serde(rename = "stopOnEntry")]
    stop_on_entry: Option<bool>,
    #[serde(rename = "args")]
    args: Option<Vec<String>>,
    #[serde(rename = "program")]
    program: Option<String>,
}

#[derive(Clone, Debug, Deserialize)]
pub struct RequestContainer<T> {
    arguments: T,
}

#[derive(Clone, Debug)]
pub enum TargetDepth {
    LessThan(usize),
    LessOrEqual(usize),
}

#[derive(Clone, Debug)]
pub struct StoredScope {
    scope_id: u32,

    name: String,
    named_args: HashMap<String, Rc<SExp>>,

    rundata: Option<BTreeMap<String, String>>,

    source: Srcloc,
}

pub struct RunningDebugger {
    pub initialized: InitializeRequestArguments,
    pub launch_info: LaunchRequestArguments,
    target_depth: Option<TargetDepth>,
    stopped_reason: Option<StoppedReason>,

    source_file: String,
    compiled: Option<CompileForm>,

    running: bool,
    run: HierarchialRunner,
    output_stack: Vec<StoredScope>,
    result: Option<String>,

    pub opts: Rc<dyn CompilerOpts>,
}

impl RunningDebugger {
    fn get_source_file(
        &self,
        fs: Rc<dyn IFileReader>,
        log: Rc<dyn ILogWriter>,
        sfargs: &SourceArguments
    ) -> Option<ResponseBody> {
        log.write(&format!("get_source_file {:?}", sfargs));
        sfargs.source
            .as_ref()
            .and_then(|s| s.name.as_ref())
            .and_then(|n| {
                PathBuf::from(&self.source_file)
                    .parent()
                    .map(|p| (n,p.to_owned()))
            }).map(|(n,p)| p.join(n)).and_then(|path| {
                path.to_str().map(|x| x.to_owned())
            }).and_then(|path_string| {
                fs.read(&path_string).ok()
            }).map(|content| {
                ResponseBody::Source(SourceResponse {
                    content: decode_string(&content),
                    mime_type: Some("text/plain".to_owned())
                })
            })
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

    fn step(&mut self) -> Option<BTreeMap<String, String>> {
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
                    println!("Runtime Error: {}: {}", l, e);
                    // Nothing
                }
                Err(RunFailure::RunExn(l, e)) => {
                    println!("Raised exception: {}: {}", l, e);
                    // Nothing
                }
            }
        }
    }
}

pub enum State {
    PreInitialization,
    Initialized(InitializeRequestArguments),
    Launched(RunningDebugger),
}

pub enum BreakpointLocation {
    Srcloc(Srcloc),
    Treehash(String),
}

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
            supports_configuration_done_request: Some(true),
            supports_function_breakpoints: Some(true),
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
            supports_data_breakpoints: Some(true),
            supports_delayed_stack_trace_loading: None,
            supports_disassemble_request: Some(true),
            supports_exception_filter_options: None,
            supports_exception_info_request: None,
            supports_exception_options: None,
            supports_instruction_breakpoints: Some(true),
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
    new_ext: &str
) -> Option<(String, Vec<u8>)> {
    let path_components = PathBuf::from(fname);
    if let (Some(directory), Some(only_filename)) =
        (path_components.parent(), path_components.file_name()) {
        if let Some(rust_str_filename) = only_filename.to_str() {
            let filename_bytes = rust_str_filename.as_bytes().to_vec();
            let mut dots: Vec<usize> = filename_bytes.iter().copied().enumerate().filter(|(_,ch)| {
                *ch == b'.'
            }).map(|(i,_)| i).collect();
            dots.push(only_filename.len());
            for d in dots.iter() {
                let mut synthesized_filename: Vec<u8> =
                    filename_bytes.iter().copied().take(*d).collect();
                synthesized_filename.append(&mut new_ext.as_bytes().to_vec());
                let synth_fname =
                    PathBuf::from(&decode_string(&synthesized_filename));

                if let Some(total_filename) =
                    directory.join(synth_fname).to_str()
                {
                    if let Ok(content) = fs.read(total_filename) {
                        return Some((total_filename.to_owned(), content));
                    }
                }
            }
        }
    }

    None
}

fn try_locate_symbols(
    fs: Rc<dyn IFileReader>,
    fname: &str
) -> Option<(String, Vec<u8>)> {
    try_locate_related_file(fs, fname, ".sym")
}

fn try_locate_source_file(
    fs: Rc<dyn IFileReader>,
    fname: &str
) -> Option<(String, Vec<u8>)> {
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
    is_hex: bool,
    compiled: Option<CompileForm>
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
    ext: Option<usize>
}

pub fn parse_srcloc(s: &str) -> Option<Srcloc> {
    let parse_one_loc = |skip| {
        let mut parse_state = SrclocParseAction::ReadingFileName;
        for (i,ch) in s.as_bytes().iter().skip(skip).copied().enumerate() {
            match (&parse_state, ch) {
                (SrclocParseAction::ReadingFileName, b'(') => {
                    parse_state = SrclocParseAction::ReadingLineNumber(i, 0);
                }
                (SrclocParseAction::ReadingFileName, _) => { }
                (SrclocParseAction::ReadingLineNumber(eof, l), b')') => {
                    parse_state = SrclocParseAction::AfterLineNumber(*eof, *l);
                }
                (SrclocParseAction::ReadingLineNumber(eof, l), ch) => {
                    if !ch.is_ascii_digit() { return None; }
                    parse_state =
                        SrclocParseAction::ReadingLineNumber(
                            *eof,
                            10 * *l + ((ch - b'0') as usize)
                        );
                }
                (SrclocParseAction::AfterLineNumber(eof, l), b':') => {
                    parse_state =
                        SrclocParseAction::ReadingColumn(
                            *eof,
                            *l,
                            0,
                            None
                        );
                }
                (SrclocParseAction::AfterLineNumber(_, _), _) => {
                    return None;
                }
                (SrclocParseAction::ReadingColumn(eof, l, c, _), b'-') => {
                    parse_state =
                        SrclocParseAction::ReadingColumn(
                            *eof,
                            *l,
                            *c,
                            Some(i+1)
                        );
                }
                (SrclocParseAction::ReadingColumn(eof, l, c, e), ch) => {
                    if !ch.is_ascii_digit() { return None; }
                    parse_state =
                        SrclocParseAction::ReadingColumn(
                            *eof,
                            *l,
                            10 * *c + ((ch - b'0') as usize),
                            *e
                        );
                }
            }
        }

        if let SrclocParseAction::ReadingColumn(f, line, col, ext) = parse_state {
            Some(ParsedSrclocPart {
                file: s.as_bytes().iter().copied().take(f).collect(),
                line,
                col,
                ext
            })
        } else {
            None
        }
    };

    parse_one_loc(0).and_then(|parsed| {
        let filename_rc = Rc::new(decode_string(&parsed.file));
        let loc =
            Srcloc::new(
                filename_rc.clone(),
                parsed.line,
                parsed.col
            );
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
    fn get_source_loc(
        &self,
        running: &RunningDebugger,
        name: &str
    ) -> Option<Srcloc> {
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

        get_helper_loc(name).map(Some).unwrap_or_else(|| {
            parse_srcloc(name)
        })
    }

    fn get_scope_source(
        &self,
        running: &RunningDebugger,
        scope: &StoredScope
    ) -> Option<Source> {
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
                sources: None
            })
        };
        self.get_source_loc(running, &scope.name).and_then(source_from_loc)
    }

    fn read_program_data(
        &self,
        allocator: &mut Allocator,
        opts: Rc<dyn CompilerOpts>,
        _i: &InitializeRequestArguments,
        _l: &LaunchRequestArguments,
        name: &str,
        read_in_file: &[u8]
    ) -> Result<RunStartData, String> {
        let program_lines: Vec<String> =
            read_in_file.lines().map(|x| x.unwrap()).collect();
        let parse_err_map = |e: (Srcloc, String)| format!("{}: {}", e.0, e.1);
        let compile_err_map = |e: CompileErr| format!("{}: {}", e.0, e.1);
        let run_err_map = |e: RunFailure| {
            match e {
                RunFailure::RunErr(l,e) => format!("{}: {}", l, e),
                RunFailure::RunExn(l,v) => format!("{}: exception {}", l, v)
            }
        };

        let mut use_symbol_table = HashMap::new();
        let is_hex = is_hex_file(read_in_file);

        self.log.write(&format!("read program data {} hex {}\n", name, is_hex));

        let mut compiled = None;
        if let Some((source_file, source_content)) =
            try_locate_source_file(self.fs.clone(), name)
        {
            self.log.write(&format!("source file {}", source_file));

            let source_parsed = parse_sexp(
                Srcloc::start(&source_file),
                source_content.iter().copied()
            ).map_err(parse_err_map)?;

            compiled = Some(frontend(
                opts.clone(),
                &source_parsed
            ).map_err(compile_err_map)?);
        }

        let mut parsed_program =
            if is_hex {
                self.log.write(&format!("prog source {}\n", name));
                let prog_srcloc = Srcloc::start(&name);

                // Synthesize content by disassembling the file.
                use_symbol_table =
                    if let Some((symfile, symdata)) = try_locate_symbols(self.fs.clone(), name) {
                        serde_json::from_str(&decode_string(&symdata)).map_err(|_| {
                            format!("Failure decoding symbols from {}", symfile)
                        })?
                    } else {
                        HashMap::new()
                    };

                hex_to_modern_sexp(
                    allocator,
                    &use_symbol_table,
                    prog_srcloc.clone(),
                    &decode_string(read_in_file),
                ).map_err(run_err_map)?
            } else {
                let parsed =
                    parse_sexp(Srcloc::start(name), read_in_file.iter().copied())
                    .map_err(parse_err_map)?;

                if parsed.is_empty() {
                    return Err(format!("Empty program file {}", name));
                }

                parsed[0].clone()
            };

        if is_mod(parsed_program.clone()) {
            // Compile program.
            let unopt_res = compile_file(
                allocator,
                self.runner.clone(),
                opts.clone(),
                &decode_string(&read_in_file),
                &mut use_symbol_table,
            )
                .map_err(compile_err_map)?;
            parsed_program = Rc::new(unopt_res);
        };

        let arguments = Rc::new(SExp::Nil(parsed_program.loc()));

        return Ok(RunStartData {
            source_file: name.to_owned(),
            program: parsed_program.clone(),
            program_lines: program_lines,
            arguments: arguments.clone(),
            symbols: use_symbol_table,
            is_hex,
            compiled
        });
    }


    fn launch(
        &self,
        pm: &ProtocolMessage,
        name: &str,
        i: &InitializeRequestArguments,
        l: &LaunchRequestArguments,
        program: &str,
        args: &[String],
        stop_on_entry: bool,
    ) -> Result<(i64, State, Vec<ProtocolMessage>), String> {
        let mut allocator = Allocator::new();
        let mut seq_nr = self.msg_seq;
        let read_in_file = self.fs.read(program)?;
        let opts = Rc::new(DefaultCompilerOpts::new(name));
        let mut launch_data =
            self.read_program_data(
                &mut allocator, opts.clone(), i, l, program, &read_in_file
            )?;

        if !args.is_empty() {
            let parsed_argv0 = parse_sexp(Srcloc::start("*args*"), args[0].bytes()).map_err(|(l,e)| format!("{}: {}", l, e))?;
            if !parsed_argv0.is_empty() {
                launch_data.arguments = parsed_argv0[0].clone();
            }
        }

        let run = HierarchialRunner::new(
            self.runner.clone(),
            self.prim_map.clone(),
            Some(name.to_string()),
            Rc::new(launch_data.program_lines),
            Rc::new(launch_data.symbols),
            launch_data.program,
            launch_data.arguments,
        );
        let state = State::Launched(RunningDebugger {
            initialized: i.clone(),
            launch_info: l.clone(),
            running: !stop_on_entry,
            run,
            opts,
            output_stack: Vec::new(),
            stopped_reason: None,
            target_depth: None,
            result: None,
            source_file: launch_data.source_file,
            compiled: launch_data.compiled
        });

        seq_nr += 1;
        let mut out_messages = vec![ProtocolMessage {
            seq: self.msg_seq,
            message: MessageKind::Response(Response {
                request_seq: pm.seq,
                success: true,
                message: None,
                body: Some(ResponseBody::Launch),
            }),
        }];

        // Signal that we're paused if stop on entry.
        if stop_on_entry {
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
    fn handle_message(
        &mut self,
        raw_json: &serde_json::Value,
        pm: &ProtocolMessage,
    ) -> Result<Option<Vec<ProtocolMessage>>, String> {
        let mut state = State::PreInitialization;
        self.log.write(&format!(
            "got message {}",
            serde_json::to_string(pm).unwrap()
        ));

        swap(&mut state, &mut self.state);

        if let MessageKind::Request(req) = &pm.message {
            match (state, req) {
                (State::PreInitialization, RequestCommand::Initialize(irq)) => {
                    self.state = State::Initialized(irq.clone());
                    self.msg_seq += 1;
                    return Ok(Some(vec![ProtocolMessage {
                        seq: self.msg_seq,
                        message: MessageKind::Response(Response {
                            request_seq: pm.seq,
                            success: true,
                            message: None,
                            body: Some(ResponseBody::Initialize(get_initialize_response())),
                        }),
                    }]));
                }
                (State::Initialized(i), RequestCommand::Launch(l)) => {
                    let launch_extra: Option<RequestContainer<ExtraLaunchData>> =
                        serde_json::from_value(raw_json.clone())
                            .map(Some)
                            .unwrap_or(None);

                    self.log.write(&format!("launch extra {:?}", launch_extra));
                    let stop_on_entry = launch_extra.as_ref()
                        .and_then(|l| l.arguments.stop_on_entry)
                        .unwrap_or(true);
                    let args = launch_extra.as_ref()
                        .and_then(|l| l.arguments.args.clone())
                        .unwrap_or(vec![]);
                    if let Some(name) = &l.name {
                        let program = launch_extra
                            .and_then(|l| l.arguments.program)
                            .unwrap_or(name.clone());

                        let (new_seq, new_state, out_msgs) =
                            self.launch(
                                pm,
                                name,
                                &i,
                                l,
                                &program,
                                &args,
                                stop_on_entry
                            )?;
                        self.msg_seq = new_seq;
                        self.state = new_state;

                        return Ok(Some(out_msgs));
                    } else {
                        self.state = State::Initialized(i);
                        self.log.write("No program provided");
                    }
                }
                (State::Launched(r), RequestCommand::Source(s)) => {
                    self.msg_seq += 1;

                    let source_lookup = r.get_source_file(
                        self.fs.clone(),
                        self.log.clone(),
                        s
                    );
                    self.state = State::Launched(r);

                    return Ok(Some(vec![ProtocolMessage {
                        seq: self.msg_seq,
                        message: MessageKind::Response(Response {
                            request_seq: pm.seq,
                            success: true,
                            message: None,
                            body: source_lookup
                        })
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
                            let loc = self.get_source_loc(&r, &s.name).
                                unwrap_or_else(|| s.source.clone());
                            let filename_borrowed: &String = loc.file.borrow();
                            StackFrame {
                                id: s.scope_id as i32,
                                name: filename_borrowed.clone(),
                                source: self.get_scope_source(&r, &s),
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
                        let mut function_args = scope.named_args
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
                            if let Some(v) = scope.rundata.as_ref().and_then(|info| {
                                info.get(name)
                            }) {
                                variables.push(Variable {
                                    name: target.to_owned(),
                                    indexed_variables: None,
                                    named_variables: None,
                                    presentation_hint: None,
                                    value: v.to_string(),
                                    var_type: None,
                                    variables_reference: -1,
                                    memory_reference: None,
                                    evaluate_name: None
                                });
                            }
                        };

                        copy_info_member("Function", "_op");
                        copy_info_member("Arguments", "_args");

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
                                evaluate_name: None
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
                            source: self.get_scope_source(&r, &s),
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
                    r.step();

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
                        r.stopped_reason = None;

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
                    self.log
                        .write(&format!("Don't know what to do with {:?}", req));
                    self.state = st;
                }
            }
        }

        self.log.write(&format!("unhandled message {:?}", pm));
        Err(format!("unhandled message {:?}", pm))
    }
}
