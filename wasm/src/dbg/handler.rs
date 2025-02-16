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
use crate::dbg::obj::{RunningDebugger, TargetDepth};
use crate::dbg::source::{SrclocParseAction, StoredScope, parse_srcloc};
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
    #[serde(rename = "symbols")]
    symbols: Option<String>,
}

/// Used to make some aspects of deserializing easier via serde_json easier.
/// This container allows {"arguments":{ ... t-like thing ...}} to decode if
/// t-like thing decodes.
#[derive(Clone, Debug, Deserialize)]
pub struct RequestContainer<T> {
    arguments: T,
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

struct LaunchArgs<'a> {
    proto_msg: &'a ProtocolMessage,
    name: &'a str,
    init_args: &'a InitializeRequestArguments,
    launch_request: &'a LaunchRequestArguments,
    program: &'a str,
    args_for_program: &'a [String],
    symbols: &'a str,
    stop_on_entry: bool,
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
                    let symbols = launch_extra
                        .as_ref()
                        .and_then(|l| l.arguments.symbols.clone())
                        .unwrap_or("".to_string());
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
                            symbols: &symbols,
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
