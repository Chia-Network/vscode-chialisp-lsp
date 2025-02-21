use std::rc::Rc;

use clvmr::Allocator;
use debug_types::requests::InitializeRequestArguments;
use debug_types::types::{Source, SourceBreakpoint};

use clvm_tools_rs::classic::clvm_tools::stages::stage_0::{DefaultProgramRunner, TRunProgram};
use clvm_tools_rs::compiler::compiler::DefaultCompilerOpts;

use crate::dbg::obj::{ObjLaunchArgs, RunningDebugger};
use crate::interfaces::{EPrintWriter, FSFileReader};

#[test]
fn test_start_classic_program_get_args() {
    let log = Rc::new(EPrintWriter::new());
    let fs = Rc::new(FSFileReader::new());
    let runner: Rc<dyn TRunProgram> = Rc::new(DefaultProgramRunner::new());
    let mut allocator = Allocator::new();
    let main_name = "../test/classic-arg-decode.clsp";
    let sym_name = "../test/classic-arg-decode.sym";
    let opts = Rc::new(DefaultCompilerOpts::new(main_name));

    let ira = InitializeRequestArguments {
        adapter_id: "".to_string(),
        client_id: None,
        client_name: None,
        columns_start_at1: None,
        lines_start_at1: None,
        locale: None,
        path_format: None,
        supports_invalidated_event: None,
        supports_memory_event: None,
        supports_memory_references: None,
        supports_progress_reporting: None,
        supports_run_in_terminal_request: None,
        supports_variable_paging: None,
        supports_variable_type: None,
    };
    let launch_args = ObjLaunchArgs {
        args_for_program: &["()".to_string()],
        name: main_name,
        program: main_name,
        stop_on_entry: true,
        symbols: sym_name,
        init_args: &ira,
    };

    let mut debugger = RunningDebugger::create(
        &mut allocator,
        fs.clone(),
        log.clone(),
        runner.clone(),
        opts.clone(),
        &launch_args,
    )
    .unwrap();

    // Ensure that we've got a breakpoint in F.
    let breakpoints = vec![SourceBreakpoint {
        column: Some(1),
        condition: None,
        hit_condition: None,
        line: 3,
        log_message: None,
    }];
    let recognized_bps = debugger.set_breakpoints(
        log,
        &Source {
            adapter_data: None,
            checksums: None,
            name: Some(main_name.to_string()),
            origin: None,
            path: None,
            presentation_hint: None,
            source_reference: None,
            sources: None,
        },
        breakpoints,
    );

    // This is recovered from the association with the source file.  It isn't in the symbols
    // although using --extra-syms would have added it at compile time.
    assert_eq!(
        debugger
            .symbols
            .get("a63655f92a682296fe0afe6593b013f3d1ea5302438129dcd5af967af8e9b8f1_arguments"),
        Some(&"(X Y)".to_string())
    );

    // The range of the function in the source file is recovered from what we can collect by
    // running the compiler frontend for the common chialisp subset.
    let bp_ranges: Vec<_> = recognized_bps
        .iter()
        .map(|b| (b.line, b.column, b.end_line, b.end_column))
        .collect();
    let want_ranges = vec![(Some(2), Some(3), Some(3), Some(26))];
    assert_eq!(bp_ranges, want_ranges);
}
