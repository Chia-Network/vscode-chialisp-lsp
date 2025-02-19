use regex::Regex;
use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;
use std::rc::Rc;

use crate::lsp::{
    LSPServiceMessageHandler, LSPServiceProvider, TK_COMMENT_IDX, TK_DEFINITION_BIT,
    TK_FUNCTION_IDX, TK_KEYWORD_IDX, TK_NUMBER_IDX, TK_PARAMETER_IDX, TK_STRING_IDX,
    TK_VARIABLE_IDX,
};

use lsp_server::{Message, Notification, Request, RequestId};
use lsp_types::{
    CompletionItem, CompletionParams, CompletionResponse, DidChangeTextDocumentParams,
    DidOpenTextDocumentParams, PartialResultParams, Position, Range, SemanticToken, SemanticTokens,
    SemanticTokensParams, TextDocumentContentChangeEvent, TextDocumentIdentifier, TextDocumentItem,
    TextDocumentPositionParams, Url, VersionedTextDocumentIdentifier, WorkDoneProgressParams,
};

use crate::dbg::source::parse_srcloc;
use crate::interfaces::{EPrintWriter, FSFileReader};
use crate::lsp::parse::{is_first_in_list, make_simple_ranges, ParsedDoc};
use crate::lsp::patch::{split_text, stringify_doc, PatchableDocument};
use crate::lsp::reparse::{combine_new_with_old_parse, reparse_subset};
use crate::lsp::types::{ConfigJson, DocData, DocPosition, DocRange, IncludeData};
use clvm_tools_rs::compiler::compiler::DefaultCompilerOpts;
use clvm_tools_rs::compiler::comptypes::CompilerOpts;
use clvm_tools_rs::compiler::prims;
use clvm_tools_rs::compiler::srcloc::Srcloc;

fn make_did_open_message(uri: &String, v: i32, body: String) -> Message {
    Message::Notification(Notification {
        method: "textDocument/didOpen".to_string(),
        params: serde_json::to_value(DidOpenTextDocumentParams {
            text_document: TextDocumentItem {
                uri: Url::parse(uri).unwrap(),
                language_id: "chialisp".to_string(),
                version: v,
                text: body,
            },
        })
        .unwrap(),
    })
}

fn make_get_semantic_tokens_msg(uri: &String, rid: i32) -> Message {
    Message::Request(Request {
        id: RequestId::from(rid),
        method: "textDocument/semanticTokens/full".to_string(),
        params: serde_json::to_value(SemanticTokensParams {
            work_done_progress_params: WorkDoneProgressParams {
                work_done_token: None,
            },
            partial_result_params: PartialResultParams {
                partial_result_token: None,
            },
            text_document: TextDocumentIdentifier {
                uri: Url::parse(uri).unwrap(),
            },
        })
        .unwrap(),
    })
}

fn make_insert_text_msg(uri: &String, v: i32, replace: DocRange, text: String) -> Message {
    Message::Notification(Notification {
        method: "textDocument/didChange".to_string(),
        params: serde_json::to_value(DidChangeTextDocumentParams {
            text_document: VersionedTextDocumentIdentifier {
                version: v,
                uri: Url::parse(uri).unwrap(),
            },
            content_changes: vec![TextDocumentContentChangeEvent {
                range: Some(replace.to_range()),
                range_length: None,
                text: text,
            }],
        })
        .unwrap(),
    })
}

fn make_completion_request_msg(uri: &String, rid: i32, position: Position) -> Message {
    Message::Request(Request {
        id: RequestId::from(rid),
        method: "textDocument/completion".to_string(),
        params: serde_json::to_value(CompletionParams {
            text_document_position: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier {
                    uri: Url::parse(uri).unwrap(),
                },
                position: position,
            },
            work_done_progress_params: WorkDoneProgressParams {
                work_done_token: None,
            },
            partial_result_params: PartialResultParams {
                partial_result_token: None,
            },
            context: None,
        })
        .unwrap(),
    })
}

fn decode_completion_response(m: &Message) -> Option<Vec<CompletionItem>> {
    serde_json::from_str(&serde_json::to_value(&m).unwrap().to_string())
        .ok()
        .and_then(|deser| {
            if let Message::Response(cr) = deser {
                Some(cr)
            } else {
                None
            }
        })
        .and_then(|cr| cr.result)
        .and_then(|cr| serde_json::from_str(&cr.to_string()).ok())
        .map(|cr| match cr {
            CompletionResponse::Array(v) => v.clone(),
            CompletionResponse::List(cl) => cl.items.clone(),
        })
}

fn find_completion_response(out_msgs: &[Message]) -> Option<CompletionItem> {
    for c in out_msgs.iter() {
        if let Some(completion_result) = decode_completion_response(&c) {
            return Some(completion_result[0].clone());
        }
    }

    None
}

fn get_msg_params(msg: &Message) -> String {
    match msg {
        Message::Request(req) => req.params.to_string(),
        Message::Notification(not) => not.params.to_string(),
        Message::Response(res) => res
            .result
            .as_ref()
            .map(|r| serde_json::to_string(r).unwrap())
            .unwrap_or_else(|| "null".to_string()),
    }
}

#[test]
fn can_receive_did_open_file_and_give_semantic_tokens() {
    let mut lsp = LSPServiceProvider::new(
        Rc::new(FSFileReader::new()),
        Rc::new(EPrintWriter::new()),
        true,
    );
    let file = "file:test.cl".to_string();
    let open_msg = make_did_open_message(&file, 1, "(mod () (defun F () ()) (F))".to_string());
    let sem_tok = make_get_semantic_tokens_msg(&file, 2);
    lsp.handle_message(&open_msg)
        .expect("should be ok to take open msg");
    let r2 = lsp
        .handle_message(&sem_tok)
        .expect("should be ok to send sem tok");
    for msg in r2.iter() {
        eprintln!(">> {}", serde_json::to_value(msg).unwrap().to_string());
    }
    let decoded_tokens: SemanticTokens = serde_json::from_str(&get_msg_params(&r2[0])).unwrap();
    assert_eq!(
        decoded_tokens.data,
        vec![
            SemanticToken {
                delta_line: 0,
                delta_start: 1,
                length: 3,
                token_type: TK_KEYWORD_IDX,
                token_modifiers_bitset: 0,
            },
            SemanticToken {
                delta_line: 0,
                delta_start: 8,
                length: 5,
                token_type: TK_KEYWORD_IDX,
                token_modifiers_bitset: 0,
            },
            SemanticToken {
                delta_line: 0,
                delta_start: 6,
                length: 1,
                token_type: TK_FUNCTION_IDX,
                token_modifiers_bitset: 1 << TK_DEFINITION_BIT
            },
            SemanticToken {
                delta_line: 0,
                delta_start: 10,
                length: 1,
                token_type: TK_FUNCTION_IDX,
                token_modifiers_bitset: 0
            }
        ]
    );
}

// Run an lsp over some messages so we can check out what it does.
fn run_lsp(lsp: &mut LSPServiceProvider, messages: &Vec<Message>) -> Result<Vec<Message>, String> {
    let mut res = Vec::new();
    for m in messages.iter() {
        let mut new_msgs = lsp.handle_message(m)?;
        res.append(&mut new_msgs);
    }
    Ok(res)
}

#[test]
fn test_completion_from_argument_single_level() {
    let mut lsp = LSPServiceProvider::new(
        Rc::new(FSFileReader::new()),
        Rc::new(EPrintWriter::new()),
        true,
    );
    let file = "file:///test.cl".to_string();
    let open_msg = make_did_open_message(
        &file,
        1,
        indoc! {"
(mod (A) ;;; COLLATZ conjecture

;; set language standard
  (include *standard-cl-22*)
;; Determine if number is odd
  (defun-inline odd (X) (logand X 1))
                ;; Actual collatz function
  ;; determines number of step til 1
  (defun collatz (N X zoom)
    (if (= X 1) ; We got 1
      N ; Return the number of steps
      (let ((incN (+ N 1))) ; Next N
        (if (odd X) ; Is it odd?
          (collatz zoo (+ 1 (* 3 X))) ; Odd? 3 X + 1
          (collatz incN (/ X 2)) ; Even? X / 2
          )
        )
      )
    )
  (collatz 0 A) ; Run it
  )            "}
        .to_string(),
    );
    let complete_msg = make_completion_request_msg(
        &file,
        2,
        Position {
            line: 13,
            character: 21,
        },
    );
    let out_msgs = run_lsp(&mut lsp, &vec![open_msg, complete_msg]).unwrap();
    assert_eq!(out_msgs.len() > 0, true);
    let completion_result = find_completion_response(&out_msgs).unwrap();
    assert_eq!(completion_result.label, "zoom");
}

#[test]
fn test_completion_from_argument_single_level_at_end() {
    let mut lsp = LSPServiceProvider::new(
        Rc::new(FSFileReader::new()),
        Rc::new(EPrintWriter::new()),
        true,
    );
    let file = "file:///test.cl".to_string();
    let open_msg = make_did_open_message(
        &file,
        1,
        indoc! {"
(mod (A) ;;; COLLATZ conjecture

;; set language standard
  (include *standard-cl-22*)
;; Determine if number is odd
  (defun-inline odd (X) (logand X 1))
                ;; Actual collatz function
  ;; determines number of step til 1
  (defun collatz (N X zoom)
    (if (= X 1) ; We got 1
      N ; Return the number of steps
      (let ((incN (+ N 1))) ; Next N
        (if (odd X) ; Is it odd?
          (collatz zoo (+ 1 (* 3 X))) ; Odd? 3 X + 1
          (collatz incN (/ X 2)) ; Even? X / 2
          )
        )
      )
    )
  (collatz 0 A) ; Run it
  )            "}
        .to_string(),
    );
    let complete_msg = make_completion_request_msg(
        &file,
        2,
        Position {
            line: 13,
            character: 22,
        },
    );
    let out_msgs = run_lsp(&mut lsp, &vec![open_msg, complete_msg]).unwrap();
    assert_eq!(out_msgs.len() > 0, true);
    let completion_result = find_completion_response(&out_msgs).unwrap();
    assert_eq!(completion_result.label, "zoom");
}

#[test]
fn test_completion_from_argument_let_binding() {
    let mut lsp = LSPServiceProvider::new(
        Rc::new(FSFileReader::new()),
        Rc::new(EPrintWriter::new()),
        true,
    );
    let file = "file:///test.cl".to_string();
    let open_msg = make_did_open_message(
        &file,
        1,
        indoc! {"
(mod (A) ;;; COLLATZ conjecture

;; set language standard
  (include *standard-cl-22*)
;; Determine if number is odd
  (defun-inline odd (X) (logand X 1))
                ;; Actual collatz function
  ;; determines number of step til 1
  (defun collatz (N X zoom)
    (if (= X 1) ; We got 1
      N ; Return the number of steps
      (let ((incN (+ N 1))) ; Next N
        (if (odd X) ; Is it odd?
          (collatz inc (+ 1 (* 3 X))) ; Odd? 3 X + 1
          (collatz incN (/ X 2)) ; Even? X / 2
          )
        )
      )
    )
  (collatz 0 A) ; Run it
  )            "}
        .to_string(),
    );
    let complete_msg = make_completion_request_msg(
        &file,
        2,
        Position {
            line: 13,
            character: 21,
        },
    );
    let out_msgs = run_lsp(&mut lsp, &vec![open_msg, complete_msg]).unwrap();
    assert_eq!(out_msgs.len() > 0, true);
    let completion_result = find_completion_response(&out_msgs).unwrap();
    assert_eq!(completion_result.label, "incN");
}

#[test]
fn test_completion_from_argument_top_level_only_expected() {
    let mut lsp = LSPServiceProvider::new(
        Rc::new(FSFileReader::new()),
        Rc::new(EPrintWriter::new()),
        true,
    );
    let file = "file:///test.cl".to_string();
    let open_msg = make_did_open_message(
        &file,
        1,
        indoc! {"
(mod (X1) ;;; COLLATZ conjecture

;; set language standard
  (include *standard-cl-22*)
;; Determine if number is odd
  (defun-inline odd (X2) (logand X2 1))
                ;; Actual collatz function
  ;; determines number of step til 1
  (defun collatz (N X3)
    (if (= X3 1) ; We got 1
      N ; Return the number of steps
      (let ((incN (+ N 1))) ; Next N
        (if (odd X3) ; Is it odd?
          (collatz zoo (+ 1 (* 3 X3))) ; Odd? 3 X + 1
          (collatz incN (/ X3 2)) ; Even? X / 2
          )
        )
      )
    )
  (collatz 0 X) ; Run it
  )            "}
        .to_string(),
    );
    let complete_msg = make_completion_request_msg(
        &file,
        2,
        Position {
            line: 19,
            character: 14,
        },
    );
    let out_msgs = run_lsp(&mut lsp, &vec![open_msg, complete_msg]).unwrap();
    assert_eq!(out_msgs.len() > 0, true);
    let completion_result = find_completion_response(&out_msgs).unwrap();
    assert_eq!(completion_result.label, "X1");
}

#[test]
fn test_completion_from_argument_function() {
    let mut lsp = LSPServiceProvider::new(
        Rc::new(FSFileReader::new()),
        Rc::new(EPrintWriter::new()),
        true,
    );
    let file = "file:///test.cl".to_string();
    let open_msg = make_did_open_message(
        &file,
        1,
        indoc! {"
(mod (A) ;;; COLLATZ conjecture
  (defun-inline odd (X) (logand X 1))
  (+ (od) 2)
  )"}
        .to_string(),
    );
    let complete_msg = make_completion_request_msg(
        &file,
        2,
        Position {
            line: 2,
            character: 8,
        },
    );
    let out_msgs = run_lsp(&mut lsp, &vec![open_msg, complete_msg]).unwrap();
    let completion_response = find_completion_response(&out_msgs).unwrap();
    assert_eq!(completion_response.label, "odd");
}

#[test]
fn test_first_in_list() {
    let file_data = "( test1 test2)".to_string();
    let doc = DocData {
        fullname: "file:test.cl".to_string(),
        text: split_text(&file_data),
        version: -1,
        comments: HashMap::new(),
    };
    let pos = Position {
        line: 0,
        character: 5,
    };
    assert_eq!(is_first_in_list(&doc, &pos), true);
}

#[test]
fn test_not_first_in_list() {
    let file_data = "( test1 test2)".to_string();
    let doc = DocData {
        fullname: "file:test.cl".to_string(),
        text: split_text(&file_data),
        version: -1,
        comments: HashMap::new(),
    };
    let pos = Position {
        line: 0,
        character: 10,
    };
    assert_eq!(is_first_in_list(&doc, &pos), false);
}

#[test]
fn test_patch_document_1() {
    let content = "(mod (A) ;;; COLLATZ conjecture\n\n;; set language standard\n  (include *standard-cl-22*)\n;; Determine if number is odd\n  (defun-inline odd (X) (logand X 1))\n                ;; Actual collatz function\n  ;; determines number of step til 1\n  (defun collatz (N X zook)\n    (if (= X 1) ; We got 1\n      N ; Return the number of steps\n      (let ((incN (+ N 1))) ; Next N\n        (if (odd X) ; Is it odd?\n          (collatz incN (+ 1 (* 3 X))) ; Odd? 3 X + 1\n          (collatz incN (/ X 2)) ; Even? X / 2\n          )\n        )\n      )\n    )\n  (collatz 0 A) ; Run it\n  )".to_string();
    let changes = vec![TextDocumentContentChangeEvent {
        range_length: None,
        range: Some(Range {
            start: Position {
                character: 22,
                line: 13,
            },
            end: Position {
                character: 23,
                line: 13,
            },
        }),
        text: "".to_string(),
    }];
    let doc = (DocData {
        fullname: "file:test.cl".to_string(),
        text: split_text(&content),
        version: -1,
        comments: HashMap::new(),
    })
    .apply_patch(0, &changes);
    eprintln!("edited: {}", stringify_doc(&doc.text).unwrap());
    assert_eq!(stringify_doc(&doc.text).unwrap(), "(mod (A) ;;; COLLATZ conjecture\n\n;; set language standard\n  (include *standard-cl-22*)\n;; Determine if number is odd\n  (defun-inline odd (X) (logand X 1))\n                ;; Actual collatz function\n  ;; determines number of step til 1\n  (defun collatz (N X zook)\n    (if (= X 1) ; We got 1\n      N ; Return the number of steps\n      (let ((incN (+ N 1))) ; Next N\n        (if (odd X) ; Is it odd?\n          (collatz inc (+ 1 (* 3 X))) ; Odd? 3 X + 1\n          (collatz incN (/ X 2)) ; Even? X / 2\n          )\n        )\n      )\n    )\n  (collatz 0 A) ; Run it\n  )\n");
}

#[test]
fn test_patch_document_2() {
    let content = "(mod (A) ;;; COLLATZ conjecture\n\n;; set language standard\n  (include *standard-cl-22*)\n;; Determine if number is odd\n  (defun-inline odd (X) (logand X 1))\n                ;; Actual collatz function\n  ;; determines number of step til 1\n  (defun collatz (N X zook)\n    (if (= X 1) ; We got 1\n      N ; Return the number of steps\n      (let ((incN (+ N 1))) ; Next N\n        (if (odd X) ; Is it odd?\n          (collatz  (+ 1 (* 3 X))) ; Odd? 3 X + 1\n          (collatz incN (/ X 2)) ; Even? X / 2\n          )\n        )\n      )\n    )\n  (collatz 0 A) ; Run it\n  )".to_string();
    let changes = vec![TextDocumentContentChangeEvent {
        range_length: None,
        range: Some(Range {
            start: Position {
                character: 19,
                line: 13,
            },
            end: Position {
                character: 19,
                line: 13,
            },
        }),
        text: "z".to_string(),
    }];
    let doc = (DocData {
        fullname: "file:test.cl".to_string(),
        text: split_text(&content),
        version: -1,
        comments: HashMap::new(),
    })
    .apply_patch(1, &changes);
    eprintln!("edited: {}", stringify_doc(&doc.text).unwrap());
    assert_eq!(stringify_doc(&doc.text).unwrap(), "(mod (A) ;;; COLLATZ conjecture\n\n;; set language standard\n  (include *standard-cl-22*)\n;; Determine if number is odd\n  (defun-inline odd (X) (logand X 1))\n                ;; Actual collatz function\n  ;; determines number of step til 1\n  (defun collatz (N X zook)\n    (if (= X 1) ; We got 1\n      N ; Return the number of steps\n      (let ((incN (+ N 1))) ; Next N\n        (if (odd X) ; Is it odd?\n          (collatz z (+ 1 (* 3 X))) ; Odd? 3 X + 1\n          (collatz incN (/ X 2)) ; Even? X / 2\n          )\n        )\n      )\n    )\n  (collatz 0 A) ; Run it\n  )\n");
}

#[test]
fn test_patch_document_3() {
    let content = "(test\n  1\n  2\n  3)".to_string();
    let changes = vec![TextDocumentContentChangeEvent {
        range_length: None,
        range: Some(Range {
            start: Position {
                character: 0,
                line: 1,
            },
            end: Position {
                character: 0,
                line: 2,
            },
        }),
        text: "  *\n".to_string(),
    }];
    let doc = (DocData {
        fullname: "file:test.cl".to_string(),
        text: split_text(&content),
        version: -1,
        comments: HashMap::new(),
    })
    .apply_patch(1, &changes);
    eprintln!("edited: {}", stringify_doc(&doc.text).unwrap());
    assert_eq!(stringify_doc(&doc.text).unwrap(), "(test\n  *\n  2\n  3)\n");
}

#[test]
fn test_simple_ranges() {
    let content = "(mod ()\n  (defun F (X)\n    ()\n    )\n  (F 3)\n  )".to_string();
    let simple_ranges = make_simple_ranges(&split_text(&content));
    assert_eq!(
        simple_ranges,
        vec![
            DocRange {
                start: DocPosition {
                    line: 0,
                    character: 5
                },
                end: DocPosition {
                    line: 0,
                    character: 7
                }
            },
            DocRange {
                start: DocPosition {
                    line: 1,
                    character: 2,
                },
                end: DocPosition {
                    line: 3,
                    character: 5
                }
            },
            DocRange {
                start: DocPosition {
                    line: 4,
                    character: 2
                },
                end: DocPosition {
                    line: 4,
                    character: 7
                }
            }
        ]
    );
}

#[test]
fn test_tricky_patch_1() {
    let content = indoc! {"
(mod (password new_puzhash amount)
  (include *standard-cl-21*) ;; Specify chialisp-21 compilation.

  (defconstant CREATE_COIN 51)

  (defun get-real-password ()
    0x2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824
    )

  (defun check-password (password)
    (let ((password-hash (sha256 password))
      (real-hash (get-real-password)))
      (= password-hash real-hash)
      )
    )
    

  (if (check-password password)
    (list (list CREATE_COIN new_puzhash amount))
    (x)
    )
  )
"}
    .to_string();
    let changes = vec![
        TextDocumentContentChangeEvent {
            range_length: None,
            range: Some(Range {
                start: Position {
                    character: 5,
                    line: 15,
                },
                end: Position {
                    character: 5,
                    line: 15,
                },
            }),
            text: "\n    ".to_string(),
        },
        TextDocumentContentChangeEvent {
            range_length: None,
            range: Some(Range {
                start: Position {
                    character: 0,
                    line: 15,
                },
                end: Position {
                    character: 4,
                    line: 15,
                },
            }),
            text: "".to_string(),
        },
    ];
    let doc = (DocData {
        fullname: "file:test.cl".to_string(),
        text: split_text(&content),
        version: -1,
        comments: HashMap::new(),
    })
    .apply_patch(1, &changes);
    eprintln!("edited: {}", stringify_doc(&doc.text).unwrap());
    assert_eq!(
        stringify_doc(&doc.text).unwrap(),
        indoc! {"
(mod (password new_puzhash amount)
  (include *standard-cl-21*) ;; Specify chialisp-21 compilation.

  (defconstant CREATE_COIN 51)

  (defun get-real-password ()
    0x2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824
    )

  (defun check-password (password)
    (let ((password-hash (sha256 password))
      (real-hash (get-real-password)))
      (= password-hash real-hash)
      )
    )

    

  (if (check-password password)
    (list (list CREATE_COIN new_puzhash amount))
    (x)
    )
  )\n"}
    );
}

// Remove renamed scope info so we can compare.
fn chop_scopes(s: &str) -> String {
    let re = Regex::new("_\\$_[0-9]+").unwrap();
    re.replace_all(&s, "").to_string()
}

fn run_reparse_steps(
    loc: Srcloc,
    opts: Rc<dyn CompilerOpts>,
    file: &String,
    text_inputs: &[String],
) -> ParsedDoc {
    let mut doc = ParsedDoc::new(loc.clone());
    let prims: Vec<Vec<u8>> = prims::prims().iter().map(|(k, _)| k.clone()).collect();

    for content in text_inputs.iter() {
        let text = split_text(&content);
        let ranges = make_simple_ranges(&text);
        let reparsed = reparse_subset(
            &prims,
            opts.clone(),
            &text,
            &file,
            &ranges,
            &doc.compiled,
            &HashMap::new(),
        );
        doc = combine_new_with_old_parse(&file, &text, &doc, &reparsed);
    }

    doc
}

#[test]
fn test_reparse_subset_1() {
    let file = "file:///test.cl".to_string();
    let loc = Srcloc::start(&file);
    let opts = Rc::new(DefaultCompilerOpts::new(&file));
    let combined = run_reparse_steps(
        loc,
        opts,
        &file,
        &["(mod X (defun F (X) (+ X 1)) (F X))".to_string()],
    );
    assert_eq!(
        "(X (defun F (X) (+ X 1)) (F X))",
        chop_scopes(&combined.compiled.to_sexp().to_string())
    );
}

#[test]
fn test_reparse_subset_2() {
    let file = "file:///test.cl".to_string();
    let loc = Srcloc::start(&file);
    let opts = Rc::new(DefaultCompilerOpts::new(&file));
    let combined = run_reparse_steps(
        loc,
        opts,
        &file,
        &["(mod X (defun F (X) (+ X 1)) X)".to_string()],
    );
    assert_eq!(
        "(X (defun F (X) (+ X 1)) X)",
        chop_scopes(&combined.compiled.to_sexp().to_string())
    );
}

#[test]
fn test_reparse_subset_3() {
    let file = "file:///test.cl".to_string();
    let loc = Srcloc::start(&file);
    let opts = Rc::new(DefaultCompilerOpts::new(&file));
    let combined2 = run_reparse_steps(
        loc,
        opts,
        &file,
        &[
            "(mod X (defun F (X) (+ X 1)) X)".to_string(),
            "(mod X (defun G (X) (+ X 1)) X)".to_string(),
        ],
    );
    assert_eq!(
        "(X (defun G (X) (+ X 1)) X)",
        chop_scopes(&combined2.compiled.to_sexp().to_string())
    );
}

#[test]
fn test_reparse_subset_4() {
    let file = "file:///test.cl".to_string();
    let loc = Srcloc::start(&file);
    let opts = Rc::new(DefaultCompilerOpts::new(&file));
    let combined = run_reparse_steps(
        loc,
        opts,
        &file,
        &["(mod X (include test.clib) (defun F (X) (+ X 1)) X)".to_string()],
    );
    assert_eq!(
        "(X (defun F (X) (+ X 1)) X)",
        chop_scopes(&combined.compiled.to_sexp().to_string())
    );
}

// Warn on unrecognized function call.
#[test]
fn test_warn_call_of_undefined_function() {
    let file = "file:///test.cl".to_string();
    let loc = Srcloc::start(&file);
    let opts = Rc::new(DefaultCompilerOpts::new(&file));
    let combined = run_reparse_steps(loc, opts, &file, &["(mod X (F 3))".to_string()]);
    assert_eq!(!combined.errors.is_empty(), true);
}

#[test]
fn test_mod_ends_in_defun_error() {
    let file = "file:///test.cl".to_string();
    let loc = Srcloc::start(&file);
    let opts = Rc::new(DefaultCompilerOpts::new(&file));
    let combined = run_reparse_steps(
        loc,
        opts,
        &file,
        &["(mod X (defun F (X) (+ X 1)))".to_string()],
    );
    assert_eq!(combined.errors.len(), 1);
}

#[test]
fn test_list_ends_in_defun_no_error() {
    let file = "file:///test.cl".to_string();
    let loc = Srcloc::start(&file);
    let opts = Rc::new(DefaultCompilerOpts::new(&file));
    let combined = run_reparse_steps(loc, opts, &file, &["( (defun F (X) (+ X 1)) )".to_string()]);
    assert_eq!(combined.errors.is_empty(), true);
}

#[test]
fn test_mod_can_cease_reporting_wrong_function_error() {
    let file = "file:///test.cl".to_string();
    let loc = Srcloc::start(&file);
    let opts = Rc::new(DefaultCompilerOpts::new(&file));
    let combined = run_reparse_steps(
        loc,
        opts,
        &file,
        &[
            "(mod X (defun F (X) (+ X 1)))".to_string(),
            "(mod X (defun F (X) (+ X 1)) (G X))".to_string(),
            "(mod X (defun G (X) (+ X 1)) (G X))".to_string(),
        ],
    );
    assert_eq!(combined.errors.len(), 0);
}

#[test]
fn include_is_annotated() {
    let file = "file:///test.cl".to_string();
    let loc = Srcloc::start(&file);
    let opts = Rc::new(DefaultCompilerOpts::new(&file));
    let combined = run_reparse_steps(
        loc,
        opts,
        &file,
        &[indoc! {"
(mod X
  (include hithere) ()
  )"}
        .to_string()],
    );
    let includes_flat: Vec<IncludeData> =
        combined.includes.iter().map(|(_, v)| v.clone()).collect();
    assert_eq!(includes_flat[0].kw_loc.line, 2);
    assert_eq!(includes_flat[0].kw_loc.col, 4);
    assert_eq!(includes_flat[0].name_loc.line, 2);
    assert_eq!(includes_flat[0].name_loc.col, 12);
}

#[test]
fn basic_functions_are_annotated() {
    let mut lsp = LSPServiceProvider::new(
        Rc::new(FSFileReader::new()),
        Rc::new(EPrintWriter::new()),
        true,
    );
    let file = "file:test.cl".to_string();
    let open_msg = make_did_open_message(&file, 1, "((defun F () (x)))".to_string());
    let sem_tok = make_get_semantic_tokens_msg(&file, 2);
    lsp.handle_message(&open_msg)
        .expect("should be ok to take open msg");
    let r2 = lsp
        .handle_message(&sem_tok)
        .expect("should be ok to send sem tok");
    let decoded_tokens: SemanticTokens = serde_json::from_str(&get_msg_params(&r2[0])).unwrap();
    assert_eq!(
        decoded_tokens.data,
        vec![
            SemanticToken {
                delta_line: 0,
                delta_start: 2,
                length: 5,
                token_type: TK_KEYWORD_IDX,
                token_modifiers_bitset: 0,
            },
            SemanticToken {
                delta_line: 0,
                delta_start: 6,
                length: 1,
                token_type: TK_FUNCTION_IDX,
                token_modifiers_bitset: 1,
            },
            SemanticToken {
                delta_line: 0,
                delta_start: 6,
                length: 1,
                token_type: TK_FUNCTION_IDX,
                token_modifiers_bitset: 0,
            },
        ]
    );
}

#[test]
fn compile_file_is_annotated() {
    let mut lsp = LSPServiceProvider::new(
        Rc::new(FSFileReader::new()),
        Rc::new(EPrintWriter::new()),
        true,
    );
    lsp.set_workspace_root(PathBuf::from(r"."));
    lsp.set_config(ConfigJson {
        include_paths: vec!["./resources/tests".to_string()],
    });
    let file = "file:test.cl".to_string();
    let open_msg = make_did_open_message(&file, 1, "( (compile-file test t1.cl) )".to_string());
    let sem_tok = make_get_semantic_tokens_msg(&file, 2);
    lsp.handle_message(&open_msg)
        .expect("should be ok to take open msg");
    let r2 = lsp
        .handle_message(&sem_tok)
        .expect("should be ok to send sem tok");
    let decoded_tokens: SemanticTokens = serde_json::from_str(&get_msg_params(&r2[0])).unwrap();
    assert_eq!(
        decoded_tokens.data,
        vec![
            SemanticToken {
                delta_line: 0,
                delta_start: 3,
                length: 12,
                token_type: TK_KEYWORD_IDX,
                token_modifiers_bitset: 0,
            },
            SemanticToken {
                delta_line: 0,
                delta_start: 13,
                length: 4,
                token_type: TK_VARIABLE_IDX,
                token_modifiers_bitset: TK_DEFINITION_BIT,
            },
            SemanticToken {
                delta_line: 0,
                delta_start: 5,
                length: 5,
                token_type: TK_STRING_IDX,
                token_modifiers_bitset: 0,
            },
        ]
    );
}

#[test]
fn embed_file_is_annotated() {
    let mut lsp = LSPServiceProvider::new(
        Rc::new(FSFileReader::new()),
        Rc::new(EPrintWriter::new()),
        true,
    );
    lsp.set_workspace_root(PathBuf::from(r"."));
    lsp.set_config(ConfigJson {
        include_paths: vec!["./resources/tests".to_string()],
    });
    let file = "file:test.cl".to_string();
    let open_msg = make_did_open_message(&file, 1, "( (embed-file test sexp t1.cl) )".to_string());
    let sem_tok = make_get_semantic_tokens_msg(&file, 2);
    lsp.handle_message(&open_msg)
        .expect("should be ok to take open msg");
    let r2 = lsp
        .handle_message(&sem_tok)
        .expect("should be ok to send sem tok");
    let decoded_tokens: SemanticTokens = serde_json::from_str(&get_msg_params(&r2[0])).unwrap();
    assert_eq!(
        decoded_tokens.data,
        vec![
            SemanticToken {
                delta_line: 0,
                delta_start: 3,
                length: 10,
                token_type: TK_KEYWORD_IDX,
                token_modifiers_bitset: 0,
            },
            SemanticToken {
                delta_line: 0,
                delta_start: 11,
                length: 4,
                token_type: TK_VARIABLE_IDX,
                token_modifiers_bitset: TK_DEFINITION_BIT,
            },
            SemanticToken {
                delta_line: 0,
                delta_start: 5,
                length: 4,
                token_type: TK_KEYWORD_IDX,
                token_modifiers_bitset: 0,
            },
            SemanticToken {
                delta_line: 0,
                delta_start: 5,
                length: 5,
                token_type: TK_STRING_IDX,
                token_modifiers_bitset: 0,
            },
        ]
    );
}

#[test]
fn test_inner_mod_expr() {
    let mut lsp = LSPServiceProvider::new(
        Rc::new(FSFileReader::new()),
        Rc::new(EPrintWriter::new()),
        true,
    );
    let file = "file:test.cl".to_string();
    let open_msg = make_did_open_message(&file, 1, "(mod () (mod (X) (+ X 1)))".to_string());
    let sem_tok = make_get_semantic_tokens_msg(&file, 2);
    lsp.handle_message(&open_msg)
        .expect("should be ok to take open msg");
    let r2 = lsp
        .handle_message(&sem_tok)
        .expect("should be ok to send sem tok");
    let decoded_tokens: SemanticTokens = serde_json::from_str(&get_msg_params(&r2[0])).unwrap();
    assert_eq!(
        decoded_tokens.data,
        vec![
            SemanticToken {
                delta_line: 0,
                delta_start: 1,
                length: 3,
                token_type: TK_KEYWORD_IDX,
                token_modifiers_bitset: 0,
            },
            SemanticToken {
                delta_line: 0,
                delta_start: 8,
                length: 3,
                token_type: TK_KEYWORD_IDX,
                token_modifiers_bitset: 0,
            },
            SemanticToken {
                delta_line: 0,
                delta_start: 5,
                length: 1,
                token_type: TK_PARAMETER_IDX,
                token_modifiers_bitset: 1 << TK_DEFINITION_BIT,
            },
            SemanticToken {
                delta_line: 0,
                delta_start: 4,
                length: 1,
                token_type: TK_FUNCTION_IDX,
                token_modifiers_bitset: 0,
            },
            SemanticToken {
                delta_line: 0,
                delta_start: 2,
                length: 1,
                token_type: TK_PARAMETER_IDX,
                token_modifiers_bitset: 0,
            },
            SemanticToken {
                delta_line: 0,
                delta_start: 2,
                length: 1,
                token_type: TK_NUMBER_IDX,
                token_modifiers_bitset: 0,
            },
        ]
    );
}

#[test]
fn test_inner_mod_expr_with_inner_at() {
    let mut lsp = LSPServiceProvider::new(
        Rc::new(FSFileReader::new()),
        Rc::new(EPrintWriter::new()),
        true,
    );
    let file = "file:test.cl".to_string();
    let open_msg = make_did_open_message(&file, 1, "(mod () (a (mod () (a 1 @)) @))".to_string());
    let sem_tok = make_get_semantic_tokens_msg(&file, 2);
    lsp.handle_message(&open_msg)
        .expect("should be ok to take open msg");
    let r2 = lsp
        .handle_message(&sem_tok)
        .expect("should be ok to send sem tok");
    eprintln!("msg_params {}", get_msg_params(&r2[0]));
    let decoded_tokens: SemanticTokens = serde_json::from_str(&get_msg_params(&r2[0])).unwrap();
    assert_eq!(
        decoded_tokens.data,
        vec![
            SemanticToken {
                delta_line: 0,
                delta_start: 1,
                length: 3,
                token_type: TK_KEYWORD_IDX,
                token_modifiers_bitset: 0,
            },
            SemanticToken {
                delta_line: 0,
                delta_start: 8,
                length: 1,
                token_type: TK_FUNCTION_IDX,
                token_modifiers_bitset: 0,
            },
            SemanticToken {
                delta_line: 0,
                delta_start: 3,
                length: 3,
                token_type: TK_KEYWORD_IDX,
                token_modifiers_bitset: 0,
            },
            SemanticToken {
                delta_line: 0,
                delta_start: 8,
                length: 1,
                token_type: TK_FUNCTION_IDX,
                token_modifiers_bitset: 0,
            },
            SemanticToken {
                delta_line: 0,
                delta_start: 2,
                length: 1,
                token_type: TK_NUMBER_IDX,
                token_modifiers_bitset: 0,
            },
            SemanticToken {
                delta_line: 0,
                delta_start: 2,
                length: 1,
                token_type: TK_VARIABLE_IDX,
                token_modifiers_bitset: 0,
            },
            SemanticToken {
                delta_line: 0,
                delta_start: 4,
                length: 1,
                token_type: TK_VARIABLE_IDX,
                token_modifiers_bitset: 0,
            }
        ]
    );
}

#[test]
fn test_inner_mod_expr_with_inner_includes_and_comments() {
    let mut lsp = LSPServiceProvider::new(
        Rc::new(FSFileReader::new()),
        Rc::new(EPrintWriter::new()),
        true,
    );
    lsp.set_workspace_root(PathBuf::from(r"."));
    lsp.set_config(ConfigJson {
        include_paths: vec!["./resources/tests".to_string()],
    });
    let file = "file://./test.cl".to_string();
    let open_msg = make_did_open_message(
        &file,
        1,
        indoc! {"
(mod ()
  (mod ()
    (include \"condition_codes.clvm\") ;; test comment
    ()
    )
  )"}
        .to_string(),
    );
    let sem_tok = make_get_semantic_tokens_msg(&file, 2);
    lsp.handle_message(&open_msg)
        .expect("should be ok to take open msg");
    let r2 = lsp
        .handle_message(&sem_tok)
        .expect("should be ok to send sem tok");
    eprintln!("msg {}", get_msg_params(&r2[0]));
    let decoded_tokens: SemanticTokens = serde_json::from_str(&get_msg_params(&r2[0])).unwrap();
    assert_eq!(
        decoded_tokens.data,
        vec![
            SemanticToken {
                delta_line: 0,
                delta_start: 1,
                length: 3,
                token_type: TK_KEYWORD_IDX,
                token_modifiers_bitset: 0,
            },
            SemanticToken {
                delta_line: 1,
                delta_start: 3,
                length: 3,
                token_type: TK_KEYWORD_IDX,
                token_modifiers_bitset: 0,
            },
            SemanticToken {
                delta_line: 1,
                delta_start: 5,
                length: 7,
                token_type: TK_KEYWORD_IDX,
                token_modifiers_bitset: 0,
            },
            SemanticToken {
                delta_line: 0,
                delta_start: 8,
                length: 22,
                token_type: TK_STRING_IDX,
                token_modifiers_bitset: 0,
            },
            SemanticToken {
                delta_line: 0,
                delta_start: 24,
                length: 16,
                token_type: TK_COMMENT_IDX,
                token_modifiers_bitset: 0,
            },
        ]
    );
}

#[test]
fn test_inner_mod_in_defun_expr() {
    let mut lsp = LSPServiceProvider::new(
        Rc::new(FSFileReader::new()),
        Rc::new(EPrintWriter::new()),
        true,
    );
    let file = "file:test.cl".to_string();
    let open_msg = make_did_open_message(
        &file,
        1,
        "(mod () (defun F () (mod (X) (+ X 1))) (F))".to_string(),
    );
    let sem_tok = make_get_semantic_tokens_msg(&file, 2);
    lsp.handle_message(&open_msg)
        .expect("should be ok to take open msg");
    let r2 = lsp
        .handle_message(&sem_tok)
        .expect("should be ok to send sem tok");
    let decoded_tokens: SemanticTokens = serde_json::from_str(&get_msg_params(&r2[0])).unwrap();
    assert_eq!(
        decoded_tokens.data,
        vec![
            SemanticToken {
                delta_line: 0,
                delta_start: 1,
                length: 3,
                token_type: TK_KEYWORD_IDX,
                token_modifiers_bitset: 0,
            },
            SemanticToken {
                delta_line: 0,
                delta_start: 8,
                length: 5,
                token_type: TK_KEYWORD_IDX,
                token_modifiers_bitset: 0,
            },
            SemanticToken {
                delta_line: 0,
                delta_start: 6,
                length: 1,
                token_type: TK_FUNCTION_IDX,
                token_modifiers_bitset: 1 << TK_DEFINITION_BIT,
            },
            SemanticToken {
                delta_line: 0,
                delta_start: 6,
                length: 3,
                token_type: TK_KEYWORD_IDX,
                token_modifiers_bitset: 0,
            },
            SemanticToken {
                delta_line: 0,
                delta_start: 5,
                length: 1,
                token_type: TK_PARAMETER_IDX,
                token_modifiers_bitset: 1 << TK_DEFINITION_BIT,
            },
            SemanticToken {
                delta_line: 0,
                delta_start: 4,
                length: 1,
                token_type: TK_FUNCTION_IDX,
                token_modifiers_bitset: 0,
            },
            SemanticToken {
                delta_line: 0,
                delta_start: 2,
                length: 1,
                token_type: TK_PARAMETER_IDX,
                token_modifiers_bitset: 0,
            },
            SemanticToken {
                delta_line: 0,
                delta_start: 2,
                length: 1,
                token_type: TK_NUMBER_IDX,
                token_modifiers_bitset: 0,
            },
            SemanticToken {
                delta_line: 0,
                delta_start: 6,
                length: 1,
                token_type: TK_FUNCTION_IDX,
                token_modifiers_bitset: 0
            }
        ]
    );
}

#[test]
fn test_line_move() {
    let mut lsp = LSPServiceProvider::new(
        Rc::new(FSFileReader::new()),
        Rc::new(EPrintWriter::new()),
        true,
    );
    let file = "file:test.cl".to_string();
    let open_msg = make_did_open_message(
        &file,
        1,
        indoc! {"
(mod ()
  (defun F () (mod (X) (+ X 1)))
  (F) ;; Foo
  )"}
        .to_string(),
    );
    lsp.handle_message(&open_msg)
        .expect("should be ok to take open msg");
    let sem_tok = make_get_semantic_tokens_msg(&file, 3);
    let st_reply_1 = lsp
        .handle_message(&sem_tok)
        .expect("should be ok to send sem tok");
    let insert_msg = make_insert_text_msg(
        &file,
        1,
        DocRange {
            start: DocPosition {
                line: 1,
                character: 0,
            },
            end: DocPosition {
                line: 1,
                character: 0,
            },
        },
        "\n".to_string(),
    );
    lsp.handle_message(&insert_msg).expect("should take patch");
    let st_reply_2 = lsp
        .handle_message(&sem_tok)
        .expect("should be ok to send sem tok");

    let decoded_tokens_1: SemanticTokens =
        serde_json::from_str(&get_msg_params(&st_reply_1[0])).unwrap();
    assert_eq!(
        decoded_tokens_1.data,
        vec![
            SemanticToken {
                delta_line: 0,
                delta_start: 1,
                length: 3,
                token_type: TK_KEYWORD_IDX,
                token_modifiers_bitset: 0,
            },
            SemanticToken {
                delta_line: 1,
                delta_start: 3,
                length: 5,
                token_type: TK_KEYWORD_IDX,
                token_modifiers_bitset: 0,
            },
            SemanticToken {
                delta_line: 0,
                delta_start: 6,
                length: 1,
                token_type: TK_FUNCTION_IDX,
                token_modifiers_bitset: 1 << TK_DEFINITION_BIT,
            },
            SemanticToken {
                delta_line: 0,
                delta_start: 6,
                length: 3,
                token_type: TK_KEYWORD_IDX,
                token_modifiers_bitset: 0,
            },
            SemanticToken {
                delta_line: 0,
                delta_start: 5,
                length: 1,
                token_type: TK_PARAMETER_IDX,
                token_modifiers_bitset: 1 << TK_DEFINITION_BIT,
            },
            SemanticToken {
                delta_line: 0,
                delta_start: 4,
                length: 1,
                token_type: TK_FUNCTION_IDX,
                token_modifiers_bitset: 0,
            },
            SemanticToken {
                delta_line: 0,
                delta_start: 2,
                length: 1,
                token_type: TK_PARAMETER_IDX,
                token_modifiers_bitset: 0,
            },
            SemanticToken {
                delta_line: 0,
                delta_start: 2,
                length: 1,
                token_type: TK_NUMBER_IDX,
                token_modifiers_bitset: 0,
            },
            SemanticToken {
                delta_line: 1,
                delta_start: 3,
                length: 1,
                token_type: TK_FUNCTION_IDX,
                token_modifiers_bitset: 0
            },
            SemanticToken {
                delta_line: 0,
                delta_start: 3,
                length: 7,
                token_type: TK_COMMENT_IDX,
                token_modifiers_bitset: 0
            }
        ]
    );
    let decoded_tokens_2: SemanticTokens =
        serde_json::from_str(&get_msg_params(&st_reply_2[0])).unwrap();
    assert_eq!(
        decoded_tokens_2.data,
        vec![
            SemanticToken {
                delta_line: 0,
                delta_start: 1,
                length: 3,
                token_type: TK_KEYWORD_IDX,
                token_modifiers_bitset: 0,
            },
            SemanticToken {
                delta_line: 2,
                delta_start: 3,
                length: 5,
                token_type: TK_KEYWORD_IDX,
                token_modifiers_bitset: 0,
            },
            SemanticToken {
                delta_line: 0,
                delta_start: 6,
                length: 1,
                token_type: TK_FUNCTION_IDX,
                token_modifiers_bitset: 1 << TK_DEFINITION_BIT,
            },
            SemanticToken {
                delta_line: 0,
                delta_start: 6,
                length: 3,
                token_type: TK_KEYWORD_IDX,
                token_modifiers_bitset: 0,
            },
            SemanticToken {
                delta_line: 0,
                delta_start: 5,
                length: 1,
                token_type: TK_PARAMETER_IDX,
                token_modifiers_bitset: 1 << TK_DEFINITION_BIT,
            },
            SemanticToken {
                delta_line: 0,
                delta_start: 4,
                length: 1,
                token_type: TK_FUNCTION_IDX,
                token_modifiers_bitset: 0,
            },
            SemanticToken {
                delta_line: 0,
                delta_start: 2,
                length: 1,
                token_type: TK_PARAMETER_IDX,
                token_modifiers_bitset: 0,
            },
            SemanticToken {
                delta_line: 0,
                delta_start: 2,
                length: 1,
                token_type: TK_NUMBER_IDX,
                token_modifiers_bitset: 0,
            },
            SemanticToken {
                delta_line: 1,
                delta_start: 3,
                length: 1,
                token_type: TK_FUNCTION_IDX,
                token_modifiers_bitset: 0
            },
            SemanticToken {
                delta_line: 0,
                delta_start: 3,
                length: 7,
                token_type: TK_COMMENT_IDX,
                token_modifiers_bitset: 0
            }
        ]
    );
}

#[test]
fn test_lsp_heap_exhaustion_1() {
    let mut lsp = LSPServiceProvider::new(
        Rc::new(FSFileReader::new()),
        Rc::new(EPrintWriter::new()),
        true,
    );
    let file_data = fs::read_to_string("resources/tests/breaking_message_list_1.txt")
        .expect("should be able to read this file");
    let in_messages: Vec<Message> = file_data
        .lines()
        .filter(|l| !l.is_empty() && !l.starts_with("#"))
        .map(|l| {
            let parsed: Message = serde_json::from_str(l).expect("should parse");
            parsed
        })
        .collect();
    run_lsp(&mut lsp, &in_messages).expect("should be able to run this traffic");

    // If we got here, we didn't have the bug.
    assert!(true);
}

#[test]
fn test_parse_srcloc() {
    assert_eq!(
        parse_srcloc("test.foo(99):1007"),
        Some(Srcloc::new(Rc::new("test.foo".to_string()), 99, 1007))
    );
}

#[test]
fn test_assign_tokens() {
    let mut lsp = LSPServiceProvider::new(
        Rc::new(FSFileReader::new()),
        Rc::new(EPrintWriter::new()),
        true,
    );
    lsp.set_workspace_root(PathBuf::from(r"."));
    lsp.set_config(ConfigJson {
        include_paths: vec!["./resources/tests".to_string()],
    });
    let file = "file://./test.cl".to_string();
    let open_msg = make_did_open_message(
        &file,
        1,
        indoc! {"
(mod ()
  (include *standard-cl-23*)
  (assign A 3 (B . C) (c 3 4) (+ A B C))
  )"}
        .to_string(),
    );
    let sem_tok = make_get_semantic_tokens_msg(&file, 2);
    lsp.handle_message(&open_msg)
        .expect("should be ok to take open msg");
    let r2 = lsp
        .handle_message(&sem_tok)
        .expect("should be ok to send sem tok");
    eprintln!("msg {}", get_msg_params(&r2[0]));
    let decoded_tokens: SemanticTokens = serde_json::from_str(&get_msg_params(&r2[0])).unwrap();
    assert_eq!(
        decoded_tokens.data,
        vec![
            SemanticToken {
                delta_line: 0,
                delta_start: 1,
                length: 3,
                token_type: TK_KEYWORD_IDX,
                token_modifiers_bitset: 0,
            },
            SemanticToken {
                delta_line: 1,
                delta_start: 3,
                length: 7,
                token_type: TK_KEYWORD_IDX,
                token_modifiers_bitset: 0,
            },
            SemanticToken {
                delta_line: 0,
                delta_start: 8,
                length: 16,
                token_type: TK_STRING_IDX,
                token_modifiers_bitset: 0,
            },
            SemanticToken {
                delta_line: 1,
                delta_start: 3,
                length: 6,
                token_type: TK_KEYWORD_IDX,
                token_modifiers_bitset: 0,
            },
            SemanticToken {
                delta_line: 0,
                delta_start: 7,
                length: 1,
                token_type: TK_VARIABLE_IDX,
                token_modifiers_bitset: 3,
            },
            SemanticToken {
                delta_line: 0,
                delta_start: 2,
                length: 1,
                token_type: TK_NUMBER_IDX,
                token_modifiers_bitset: 0,
            },
            SemanticToken {
                delta_line: 0,
                delta_start: 3,
                length: 1,
                token_type: TK_VARIABLE_IDX,
                token_modifiers_bitset: 3,
            },
            SemanticToken {
                delta_line: 0,
                delta_start: 4,
                length: 1,
                token_type: TK_VARIABLE_IDX,
                token_modifiers_bitset: 3,
            },
            SemanticToken {
                delta_line: 0,
                delta_start: 4,
                length: 1,
                token_type: TK_FUNCTION_IDX,
                token_modifiers_bitset: 0,
            },
            SemanticToken {
                delta_line: 0,
                delta_start: 2,
                length: 1,
                token_type: TK_NUMBER_IDX,
                token_modifiers_bitset: 0,
            },
            SemanticToken {
                delta_line: 0,
                delta_start: 2,
                length: 1,
                token_type: TK_NUMBER_IDX,
                token_modifiers_bitset: 0,
            },
            SemanticToken {
                delta_line: 0,
                delta_start: 4,
                length: 1,
                token_type: TK_FUNCTION_IDX,
                token_modifiers_bitset: 0,
            },
            SemanticToken {
                delta_line: 0,
                delta_start: 2,
                length: 1,
                token_type: TK_VARIABLE_IDX,
                token_modifiers_bitset: 0,
            },
            SemanticToken {
                delta_line: 0,
                delta_start: 2,
                length: 1,
                token_type: TK_VARIABLE_IDX,
                token_modifiers_bitset: 0,
            },
            SemanticToken {
                delta_line: 0,
                delta_start: 2,
                length: 1,
                token_type: TK_VARIABLE_IDX,
                token_modifiers_bitset: 0,
            },
        ]
    );
}

#[test]
fn test_defconstant_non_expression() {
    let mut lsp = LSPServiceProvider::new(
        Rc::new(FSFileReader::new()),
        Rc::new(EPrintWriter::new()),
        true,
    );
    lsp.set_workspace_root(PathBuf::from(r"."));
    lsp.set_config(ConfigJson {
        include_paths: vec!["./resources/tests".to_string()],
    });
    let file = "file://./test.cl".to_string();
    let open_msg = make_did_open_message(
        &file,
        1,
        indoc! {"
(mod ()
  (include *standard-cl-23*)
  (defconstant ILLEGAL (()))
  (defconstant ILLEGAL2 (() \"HAHAH\"))
  (c ILLEGAL ILLEGAL2)
  )"}
        .to_string(),
    );
    let sem_tok = make_get_semantic_tokens_msg(&file, 2);
    lsp.handle_message(&open_msg)
        .expect("should be ok to take open msg");
    let r2 = lsp
        .handle_message(&sem_tok)
        .expect("should be ok to send sem tok");
    eprintln!("msg {}", get_msg_params(&r2[0]));
    let decoded_tokens: SemanticTokens = serde_json::from_str(&get_msg_params(&r2[0])).unwrap();
    assert_eq!(
        decoded_tokens.data,
        vec![
            SemanticToken {
                delta_line: 0,
                delta_start: 1,
                length: 3,
                token_type: TK_KEYWORD_IDX,
                token_modifiers_bitset: 0,
            },
            SemanticToken {
                delta_line: 1,
                delta_start: 3,
                length: 7,
                token_type: TK_KEYWORD_IDX,
                token_modifiers_bitset: 0,
            },
            SemanticToken {
                delta_line: 0,
                delta_start: 8,
                length: 16,
                token_type: TK_STRING_IDX,
                token_modifiers_bitset: 0,
            },
            SemanticToken {
                delta_line: 1,
                delta_start: 3,
                length: 11,
                token_type: TK_KEYWORD_IDX,
                token_modifiers_bitset: 0
            },
            SemanticToken {
                delta_line: 0,
                delta_start: 12,
                length: 7,
                token_type: TK_VARIABLE_IDX,
                token_modifiers_bitset: 3
            },
            SemanticToken {
                delta_line: 1,
                delta_start: 3,
                length: 11,
                token_type: TK_KEYWORD_IDX,
                token_modifiers_bitset: 0
            },
            SemanticToken {
                delta_line: 0,
                delta_start: 12,
                length: 8,
                token_type: TK_VARIABLE_IDX,
                token_modifiers_bitset: 3
            },
            SemanticToken {
                delta_line: 1,
                delta_start: 3,
                length: 1,
                token_type: TK_FUNCTION_IDX,
                token_modifiers_bitset: 0
            },
            SemanticToken {
                delta_line: 0,
                delta_start: 2,
                length: 7,
                token_type: TK_VARIABLE_IDX,
                token_modifiers_bitset: 0
            },
            SemanticToken {
                delta_line: 0,
                delta_start: 8,
                length: 8,
                token_type: TK_VARIABLE_IDX,
                token_modifiers_bitset: 0
            }
        ]
    );
}

#[test]
fn test_lambda_tokens() {
    let mut lsp = LSPServiceProvider::new(
        Rc::new(FSFileReader::new()),
        Rc::new(EPrintWriter::new()),
        true,
    );
    lsp.set_workspace_root(PathBuf::from(r"."));
    lsp.set_config(ConfigJson {
        include_paths: vec!["./resources/tests".to_string()],
    });
    let file = "file://./test.cl".to_string();
    let open_msg = make_did_open_message(
        &file,
        1,
        indoc! {"
(mod ()
  (include *standard-cl-23*)
  (lambda (X) (+ X 3))
  )"}
        .to_string(),
    );
    let sem_tok = make_get_semantic_tokens_msg(&file, 2);
    lsp.handle_message(&open_msg)
        .expect("should be ok to take open msg");
    let r2 = lsp
        .handle_message(&sem_tok)
        .expect("should be ok to send sem tok");
    eprintln!("msg {}", get_msg_params(&r2[0]));
    let decoded_tokens: SemanticTokens = serde_json::from_str(&get_msg_params(&r2[0])).unwrap();
    assert_eq!(
        decoded_tokens.data,
        vec![
            SemanticToken {
                delta_line: 0,
                delta_start: 1,
                length: 3,
                token_type: TK_KEYWORD_IDX,
                token_modifiers_bitset: 0,
            },
            SemanticToken {
                delta_line: 1,
                delta_start: 3,
                length: 7,
                token_type: TK_KEYWORD_IDX,
                token_modifiers_bitset: 0,
            },
            SemanticToken {
                delta_line: 0,
                delta_start: 8,
                length: 16,
                token_type: TK_STRING_IDX,
                token_modifiers_bitset: 0,
            },
            SemanticToken {
                delta_line: 1,
                delta_start: 3,
                length: 6,
                token_type: TK_KEYWORD_IDX,
                token_modifiers_bitset: 0,
            },
            SemanticToken {
                delta_line: 0,
                delta_start: 8,
                length: 1,
                token_type: TK_VARIABLE_IDX,
                token_modifiers_bitset: 3
            },
            SemanticToken {
                delta_line: 0,
                delta_start: 4,
                length: 1,
                token_type: TK_FUNCTION_IDX,
                token_modifiers_bitset: 0
            },
            SemanticToken {
                delta_line: 0,
                delta_start: 2,
                length: 1,
                token_type: TK_VARIABLE_IDX,
                token_modifiers_bitset: 0
            },
            SemanticToken {
                delta_line: 0,
                delta_start: 2,
                length: 1,
                token_type: TK_NUMBER_IDX,
                token_modifiers_bitset: 0
            }
        ]
    );
}

#[test]
fn test_first_line_comment() {
    let mut lsp = LSPServiceProvider::new(
        Rc::new(FSFileReader::new()),
        Rc::new(EPrintWriter::new()),
        true,
    );
    lsp.set_workspace_root(PathBuf::from(r"."));
    lsp.set_config(ConfigJson {
        include_paths: vec!["./resources/tests".to_string()],
    });
    let file = "file://./test.cl".to_string();
    let open_msg = make_did_open_message(
        &file,
        1,
        indoc! {"
;; Test comment
(mod () ())"}
        .to_string(),
    );
    let sem_tok = make_get_semantic_tokens_msg(&file, 2);
    lsp.handle_message(&open_msg)
        .expect("should be ok to take open msg");
    let r2 = lsp
        .handle_message(&sem_tok)
        .expect("should be ok to send sem tok");
    eprintln!("msg {}", get_msg_params(&r2[0]));
    let decoded_tokens: SemanticTokens = serde_json::from_str(&get_msg_params(&r2[0])).unwrap();
    assert_eq!(
        decoded_tokens.data,
        vec![
            SemanticToken {
                delta_line: 0,
                delta_start: 0,
                length: 16,
                token_type: TK_COMMENT_IDX,
                token_modifiers_bitset: 0,
            },
            SemanticToken {
                delta_line: 1,
                delta_start: 1,
                length: 3,
                token_type: TK_KEYWORD_IDX,
                token_modifiers_bitset: 0,
            },
        ]
    );
}
