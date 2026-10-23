use super::*;
use crate::interfaces::{IFileReader, ILogWriter};
use std::cell::{Cell, RefCell};

#[derive(Default)]
struct MemoryFiles {
    files: RefCell<HashMap<String, String>>,
    reads: Cell<usize>,
}
impl IFileReader for MemoryFiles {
    fn read_content(&self, name: &str) -> Result<String, String> {
        self.reads.set(self.reads.get() + 1);
        self.files
            .borrow()
            .get(name)
            .cloned()
            .ok_or_else(|| format!("missing {name}"))
    }
}
struct QuietLog;
impl ILogWriter for QuietLog {
    fn log(&self, _: &str) {}
}
fn server(fs: Rc<MemoryFiles>) -> LSPServiceProvider {
    fs.files.borrow_mut().insert(
        "/audit/std/prelude.clinc".into(),
        "(defconstant PRELUDE 1)".into(),
    );
    let mut service = LSPServiceProvider::new(fs, Rc::new(QuietLog), true);
    service.set_workspace_root(PathBuf::from("/audit"));
    service.set_config(ConfigJson {
        include_paths: vec![".".to_owned()],
    });
    service
}
fn send(service: &mut LSPServiceProvider, value: serde_json::Value) -> Vec<Message> {
    service
        .handle_message(&serde_json::from_value(value).unwrap())
        .unwrap()
}
fn open(service: &mut LSPServiceProvider, name: &str, source: &str) -> Vec<Message> {
    send(
        service,
        serde_json::json!({"method":"textDocument/didOpen", "params":{"textDocument":{
            "uri": format!("file:///audit/{name}"), "languageId":"chialisp", "version":1,"text":source
        }}}),
    )
}
fn errors(messages: &[Message]) -> Vec<String> {
    messages
        .iter()
        .filter_map(|message| match message {
            Message::Notification(not) if not.method == "textDocument/publishDiagnostics" => {
                Some(&not.params)
            }
            _ => None,
        })
        .flat_map(|params| {
            params["diagnostics"]
                .as_array()
                .unwrap()
                .iter()
                .map(|diag| diag["message"].as_str().unwrap().to_owned())
        })
        .collect()
}
#[test]
fn every_request_gets_a_response_and_shutdown_has_a_lifecycle() {
    let mut service = server(Rc::new(MemoryFiles::default()));
    let capabilities = LSPServiceProvider::get_capabilities();
    assert_eq!(
        capabilities.completion_provider.unwrap().resolve_provider,
        Some(false)
    );
    for method in [
        "unknown",
        "completionItem/resolve",
        "textDocument/semanticTokens/full/delta",
    ] {
        let reply = send(
            &mut service,
            serde_json::json!({"id":1,"method":method,"params":{}}),
        );
        assert!(
            matches!(&reply[..], [Message::Response(response)] if response.error.as_ref().unwrap().code == -32601)
        );
    }
    open(&mut service, "empty.clsp", "");
    let reply = send(
        &mut service,
        serde_json::json!({"id":2,"method":"textDocument/completion","params":{"textDocument":{"uri":"file:///audit/empty.clsp"},"position":{"line":0,"character":0}}}),
    );
    assert_eq!(
        reply
            .iter()
            .filter(|msg| matches!(msg, Message::Response(_)))
            .count(),
        1
    );
    let reply = send(
        &mut service,
        serde_json::json!({"id":3,"method":"shutdown","params":null}),
    );
    assert!(
        matches!(&reply[..],[Message::Response(response)] if response.result == Some(serde_json::Value::Null))
    );
    let reply = send(
        &mut service,
        serde_json::json!({"id":4,"method":"textDocument/completion","params":{}}),
    );
    assert!(
        matches!(&reply[..],[Message::Response(response)] if response.error.as_ref().unwrap().code == -32600)
    );
}
#[test]
fn imports_preserve_open_buffers_and_close_returns_ownership_to_disk() {
    let fs = Rc::new(MemoryFiles::default());
    fs.files.borrow_mut().insert(
        "/audit/lib.clinc".into(),
        "(defun diskfun (X) (+ X 1))".into(),
    );
    let mut service = server(fs);
    open(&mut service, "lib.clinc", "(defun unsavedfun (X) (+ X 2))");
    open(
        &mut service,
        "main.clsp",
        "(import lib)\n(export (X) (unsavedfun X))",
    );
    let doc = service.get_doc("file:///audit/lib.clinc").unwrap();
    assert!(stringify_doc(&doc.text).unwrap().contains("unsavedfun"));
    let before = service.get_parsed("file:///audit/main.clsp").unwrap();
    assert!(!before
        .errors
        .iter()
        .any(|error| error.1.contains("unsavedfun")));
    let after = send(
        &mut service,
        serde_json::json!({"method":"textDocument/didClose","params":{"textDocument":{"uri":"file:///audit/lib.clinc"}}}),
    );
    assert!(!service.open_documents.contains("file:///audit/lib.clinc"));
    assert!(errors(&after)
        .iter()
        .any(|error| error.contains("unsavedfun")));
    assert!(
        stringify_doc(&service.get_doc("file:///audit/lib.clinc").unwrap().text)
            .unwrap()
            .contains("diskfun")
    );
}
#[test]
fn diagnostics_refresh_on_open_change_and_stale_versions_are_ignored() {
    let mut service = server(Rc::new(MemoryFiles::default()));
    assert!(
        errors(&open(&mut service, "a.clsp", "(mod () (missing 1))"))
            .iter()
            .any(|error| error.contains("missing"))
    );
    let good = send(
        &mut service,
        serde_json::json!({"method":"textDocument/didChange","params":{"textDocument":{"uri":"file:///audit/a.clsp","version":2},"contentChanges":[{"text":"(mod () 1)"}]}}),
    );
    assert!(errors(&good).is_empty());
    send(
        &mut service,
        serde_json::json!({"method":"textDocument/didChange","params":{"textDocument":{"uri":"file:///audit/a.clsp","version":1},"contentChanges":[{"text":"(mod () (missing 1))"}]}}),
    );
    assert_eq!(
        stringify_doc(&service.get_doc("file:///audit/a.clsp").unwrap().text)
            .unwrap()
            .trim_end(),
        "(mod () 1)"
    );
}
#[test]
fn dependency_loading_is_linear_and_cycles_are_diagnostics() {
    let fs = Rc::new(MemoryFiles::default());
    for i in 0..9 {
        fs.files.borrow_mut().insert(
            format!("/audit/d{i}.clinc"),
            format!(
                "{}(defun f{i} () {i})",
                if i > 0 {
                    format!("(import d{})\n", i - 1)
                } else {
                    String::new()
                }
            ),
        );
    }
    let mut service = server(fs.clone());
    open(&mut service, "main.clsp", "(import d8)\n(export () 1)");
    assert!(fs.reads.get() < 100, "read count {}", fs.reads.get());
    let fs = Rc::new(MemoryFiles::default());
    fs.files
        .borrow_mut()
        .insert("/audit/cycle.clinc".into(), "(include cycle.clinc)".into());
    let mut service = server(fs);
    let response = open(&mut service, "main.clsp", "(import cycle)\n(export () 1)");
    assert!(errors(&response)
        .iter()
        .any(|error| error.contains("Circular")));
}
#[test]
fn assign_and_export_bindings_are_completed() {
    for source in [
        "(mod () (defun F () (assign named 1 (+ named 2))) (F))",
        "(export (named) (+ named 2))",
        "(export () (let ((named 1)) (+ named 2)))",
    ] {
        let mut service = server(Rc::new(MemoryFiles::default()));
        open(&mut service, "a.clsp", source);
        let position = source.rfind("named 2").unwrap() + 3;
        let reply = send(
            &mut service,
            serde_json::json!({"id":1,"method":"textDocument/completion","params":{"textDocument":{"uri":"file:///audit/a.clsp"},"position":{"line":0,"character":position}}}),
        );
        let result = reply
            .iter()
            .find_map(|message| {
                if let Message::Response(response) = message {
                    response.result.as_ref()
                } else {
                    None
                }
            })
            .unwrap();
        assert!(
            result["items"]
                .as_array()
                .unwrap()
                .iter()
                .any(|item| item["label"] == "named"),
            "{}: {}",
            source,
            result
        );
    }
}

#[test]
fn nested_export_and_source_macro_scopes_offer_bindings() {
    for (source, symbol, marker) in [
        (
            "(export (named) (let () (assign local 1 (list named local))))",
            "named",
            "named local",
        ),
        (
            "(export (named) (let () (assign local 1 (list named local))))",
            "local",
            "local))))",
        ),
        (
            "(defmac keep (named) named)\n(export () (keep 1))",
            "named",
            "named)\n",
        ),
        (
            "(mod () (defun F () (let* ((named 1) (other 2)) (let () (+ named other)))) (F))",
            "named",
            "named other",
        ),
    ] {
        let mut service = server(Rc::new(MemoryFiles::default()));
        open(&mut service, "a.clsp", source);
        let offset = source.rfind(marker).unwrap() + symbol.len();
        let before = &source[..offset];
        let line = before.bytes().filter(|byte| *byte == b'\n').count();
        let character = before.rsplit('\n').next().unwrap().encode_utf16().count();
        let reply = send(
            &mut service,
            serde_json::json!({"id": 1, "method": "textDocument/completion", "params": {
                "textDocument": {"uri": "file:///audit/a.clsp"}, "position": {"line": line, "character": character}
            }}),
        );
        let result = reply
            .iter()
            .find_map(|message| match message {
                Message::Response(response) => response.result.as_ref(),
                _ => None,
            })
            .unwrap();
        assert!(
            result["items"]
                .as_array()
                .unwrap()
                .iter()
                .any(|item| item["label"] == symbol),
            "{}: {}",
            source,
            result
        );
    }
}

#[test]
fn parallel_let_bindings_are_hidden_in_their_initializers() {
    let source = "(mod () (let ((named 1) (other named)) (+ named other)))";
    let mut service = server(Rc::new(MemoryFiles::default()));
    open(&mut service, "a.clsp", source);
    let position = source.find("named))").unwrap() + 3;
    let reply = send(
        &mut service,
        serde_json::json!({"id": 1, "method": "textDocument/completion", "params": {
            "textDocument": {"uri": "file:///audit/a.clsp"}, "position": {"line": 0, "character": position}
        }}),
    );
    let result = reply
        .iter()
        .find_map(|message| match message {
            Message::Response(response) => response.result.as_ref(),
            _ => None,
        })
        .unwrap();
    assert!(!result["items"]
        .as_array()
        .unwrap()
        .iter()
        .any(|item| item["label"] == "named"));
}

#[test]
fn incomplete_mod_in_imported_helper_is_a_diagnostic() {
    let fs = Rc::new(MemoryFiles::default());
    fs.files
        .borrow_mut()
        .insert("/audit/lib.clinc".into(), "(defun broken () (mod))".into());
    let mut service = server(fs);
    let response = open(
        &mut service,
        "main.clsp",
        "(import lib)\n(export () (broken))",
    );
    assert!(errors(&response)
        .iter()
        .any(|error| error.contains("incomplete mod form")));
    open(&mut service, "good.clsp", "(mod () 1)");
    assert!(service
        .get_parsed("file:///audit/good.clsp")
        .unwrap()
        .errors
        .is_empty());
}

#[test]
fn include_quick_fix_is_only_offered_for_missing_files() {
    let fs = Rc::new(MemoryFiles::default());
    fs.files
        .borrow_mut()
        .insert("/audit/lib.clinc".into(), "(defconstant X 1)".into());
    let mut service = server(fs);
    for (name, expected) in [("lib.clinc", 0), ("absent.clinc", 1)] {
        open(
            &mut service,
            "fix.clsp",
            &format!("(mod () (include {name}) 1)"),
        );
        let replies = send(
            &mut service,
            serde_json::json!({
                "id":99,"method":"textDocument/codeAction","params":{
                    "textDocument":{"uri":"file:///audit/fix.clsp"},
                    "range":{"start":{"line":0,"character":17},"end":{"line":0,"character":17+name.len()}},
                    "context":{"diagnostics":[]}
                }
            }),
        );
        assert!(
            matches!(&replies[..], [Message::Response(response)] if response.result.as_ref().unwrap().as_array().unwrap().len() == expected),
            "{replies:?}"
        );
    }
}

#[test]
fn program_imports_produce_real_program_and_hash_constants_without_file_outputs() {
    use chialisp::compiler::comptypes::{BodyForm, HelperForm};
    use chialisp::compiler::sexp::SExp;
    let fs = Rc::new(MemoryFiles::default());
    fs.files.borrow_mut().insert(
        "/audit/child.clsp".into(),
        "(include *standard-cl-23*)\n(export (X) (+ X 1))".into(),
    );
    let mut service = server(fs);
    open(
        &mut service,
        "main.clsp",
        "(import child)\n(export () (c program program_hash))",
    );
    let parsed = service.get_parsed("file:///audit/main.clsp").unwrap();
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let namespace = parsed
        .compiled
        .helpers
        .iter()
        .find_map(|helper| match helper {
            HelperForm::Defnamespace(namespace)
                if namespace.longname.components == vec![b"child".to_vec()] =>
            {
                Some(namespace)
            }
            _ => None,
        })
        .expect("compiled program namespace");
    let constant = |name: &[u8]| {
        namespace
            .helpers
            .iter()
            .find_map(|helper| match helper {
                HelperForm::Defconstant(value) if value.name == name => Some(value),
                _ => None,
            })
            .expect("program import constant")
    };
    assert!(matches!(
        constant(b"program").body.as_ref(),
        BodyForm::Quoted(SExp::Cons(_, _, _))
    ));
    assert!(
        matches!(constant(b"program_hash").body.as_ref(), BodyForm::Quoted(SExp::QuotedString(_, _, hash)) if hash.len() == 32)
    );
}
