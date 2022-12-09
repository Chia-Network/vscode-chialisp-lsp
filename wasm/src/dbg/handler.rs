use serde::Deserialize;

use std::borrow::Borrow;
use std::collections::{BTreeMap, HashMap};
use std::io::BufRead;
use std::mem::swap;
use std::rc::Rc;

use debug_types::events::{ContinuedEvent, Event, EventBody, StoppedEvent, StoppedReason};
use debug_types::requests::{InitializeRequestArguments, LaunchRequestArguments, RequestCommand};
use debug_types::responses::{
    InitializeResponse, Response, ResponseBody, ScopesResponse, StackTraceResponse,
    ThreadsResponse, VariablesResponse,
};
use debug_types::types::{
    Capabilities, ChecksumAlgorithm, Scope, Source, StackFrame, Thread, Variable,
};
use debug_types::{MessageKind, ProtocolMessage};

use clvmr::allocator::Allocator;

use clvm_tools_rs::classic::clvm_tools::stages::stage_0::TRunProgram;
use clvm_tools_rs::compiler::cldb_hierarchy::{
    HierarchialRunner, HierarchialStepResult, RunPurpose
};
use clvm_tools_rs::compiler::compiler::{compile_file, DefaultCompilerOpts};
use clvm_tools_rs::compiler::comptypes::CompilerOpts;
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
    stop_on_entry: bool,
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

    running: bool,
    run: HierarchialRunner,
    output_stack: Vec<StoredScope>,
    result: Option<String>,

    pub opts: Rc<dyn CompilerOpts>,
}

impl RunningDebugger {
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

impl Debugger {
    fn launch(
        &self,
        pm: &ProtocolMessage,
        name: &str,
        i: &InitializeRequestArguments,
        l: &LaunchRequestArguments,
        stop_on_entry: bool,
    ) -> Result<(i64, State, Vec<ProtocolMessage>), String> {
        let mut allocator = Allocator::new();
        let mut seq_nr = self.msg_seq;
        let read_in_file = self.fs.read(name)?;
        let opts = Rc::new(DefaultCompilerOpts::new(name));
        let mut parsed_program = parse_sexp(Srcloc::start(name), read_in_file.iter().copied())
            .map_err(|e| format!("{}: {}", e.0, e.1))?;
        if parsed_program.is_empty() {
            return Err(format!("Empty program file {}", name));
        }

        let mut use_symbol_table = HashMap::new();
        if is_mod(parsed_program[0].clone()) {
            // Compile program.
            let unopt_res = compile_file(
                &mut allocator,
                self.runner.clone(),
                opts.clone(),
                &decode_string(&read_in_file),
                &mut use_symbol_table,
            )
            .map_err(|e| format!("{}: {}", e.0, e.1))?;
            parsed_program = vec![Rc::new(unopt_res)];
        }

        // XXX Empty arguments for now.
        let arguments = Rc::new(SExp::Nil(parsed_program[0].loc()));
        let program_lines: Vec<String> = read_in_file.lines().map(|x| x.unwrap()).collect();
        let run = HierarchialRunner::new(
            self.runner.clone(),
            self.prim_map.clone(),
            Some(name.to_string()),
            Rc::new(program_lines),
            Rc::new(use_symbol_table),
            parsed_program[0].clone(),
            arguments,
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
                    let stop_on_entry = launch_extra
                        .map(|l| l.arguments.stop_on_entry)
                        .unwrap_or(false);

                    self.log.write(&format!("stop on entry: {}", stop_on_entry));

                    if let Some(name) = &l.name {
                        let (new_seq, new_state, out_msgs) =
                            self.launch(pm, name, &i, l, stop_on_entry)?;
                        self.msg_seq = new_seq;
                        self.state = new_state;

                        return Ok(Some(out_msgs));
                    } else {
                        self.state = State::Initialized(i);
                        self.log.write("No program provided");
                    }
                }
                (State::Launched(r), RequestCommand::Source(_s)) => {
                    self.msg_seq += 1;
                    self.state = State::Launched(r);

                    return Ok(Some(vec![ProtocolMessage {
                        seq: self.msg_seq,
                        message: MessageKind::Response(Response {
                            request_seq: pm.seq,
                            success: false,
                            message: None,
                            body: None
                            /*
                            Some(ResponseBody::Source(SourceResponse {
                                content: "( )".to_string(),
                                mime_type: "text/plain"
                            }))
                            */
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
                            let loc = s.source.clone();
                            let fn_borrowed: &String = loc.file.borrow();
                            StackFrame {
                                id: s.scope_id as i32,
                                name: s.name.clone(),
                                source: Some(Source {
                                    name: Some(fn_borrowed.clone()),
                                    path: None,
                                    source_reference: None,
                                    presentation_hint: None,
                                    origin: None,
                                    sources: None,
                                    adapter_data: None,
                                    checksums: None,
                                }),
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
                            source: None,
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
                                preserve_focus_hint: None, // Some(true),
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
