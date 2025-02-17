/// An object interface to a chialisp debugger which allows better
/// testing and use apart from the message handler interface.
use std::collections::{BTreeMap, HashMap};
use std::rc::Rc;

use debug_types::events::StoppedReason;
use debug_types::requests::{InitializeRequestArguments, LaunchRequestArguments, SourceArguments};
use debug_types::responses::{ResponseBody, SourceResponse};
use debug_types::types::{Breakpoint, Source, SourceBreakpoint};

use clvm_tools_rs::classic::clvm::__type_compatibility__::{Bytes, BytesFromType};
use clvm_tools_rs::compiler::cldb_hierarchy::{
    HierarchialRunner, HierarchialStepResult, RunPurpose,
};
use clvm_tools_rs::compiler::compiler::DefaultCompilerOpts;
use clvm_tools_rs::compiler::comptypes::{CompileForm, CompilerOpts};
use clvm_tools_rs::compiler::frontend::frontend;
use clvm_tools_rs::compiler::runtypes::RunFailure;
use clvm_tools_rs::compiler::sexp::parse_sexp;
use clvm_tools_rs::compiler::srcloc::Srcloc;

use crate::dbg::source::{find_location, StoredScope};
use crate::interfaces::{EPrintWriter, ILogWriter};

/// When stepping in or out, set a depth to stop at if reached before a breakpoint.
#[derive(Clone, Debug)]
pub enum TargetDepth {
    /// Step out to this depth.
    LessThan(usize),
    /// Stop when the depth is the same as the current depth.
    LessOrEqual(usize),
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
            let mut found_breakpoint = false;

            for (i, b) in breakpoints.iter().enumerate() {
                // Verfified if we overlap at least one location in the symbols
                // We.ll be simple and set it to the first matching point following
                // the given location.
                if let Some((hash, found)) =
                    find_location(self.symbols.clone(), &self.compiled, log.clone(), &p, b)
                {
                    let end_col = found.until.clone().map(|e| e.col as u32);
                    let end_line = found.until.map(|e| e.line as u32);
                    found_breakpoint = true;
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

            // We didn't find anything in the symbols to suggest where the breakpoint is.
            // As a second choice, we can enumerate the forms we have in our documents to
            // find a function with a compatible name.
            if !found_breakpoint {
                log.log("didn't find breakpoint, scanning forms");
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
