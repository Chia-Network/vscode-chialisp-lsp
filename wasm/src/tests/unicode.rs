use super::*;
use crate::lsp::parse::{find_ident, get_positional_text};
use crate::lsp::patch::compute_comment_lines;
use crate::lsp::positions::position_to_byte;

#[test]
fn incremental_edits_use_utf16_code_units() {
    let text = "(mod () (list \"é😀\" 123))";
    let doc = DocData {
        fullname: "file:///unicode.clsp".to_string(),
        text: split_text(text),
        comments: Default::default(),
        version: 1,
    };
    let start = text[..text.find("123").unwrap()].encode_utf16().count() as u32;
    let edited = doc.apply_patch(
        2,
        &[TextDocumentContentChangeEvent {
            range: Some(Range::new(
                Position::new(0, start),
                Position::new(0, start + 3),
            )),
            range_length: None,
            text: "42".to_string(),
        }],
    );
    assert_eq!(
        stringify_doc(&edited.text).unwrap().trim_end(),
        text.replace("123", "42")
    );
    let position = position_to_byte(&doc.text, Position::new(0, start));
    assert_eq!(
        get_positional_text(&doc.text, &position),
        Some(b"123".to_vec())
    );
}

#[test]
fn empty_lines_and_end_of_line_do_not_panic() {
    assert_eq!(find_ident(Rc::new(vec![]), 0), None);
    assert_eq!(find_ident(Rc::new(b"A".to_vec()), 1), None);
    assert_eq!(find_ident(Rc::new(b"A".to_vec()), 99), None);
}

#[test]
fn parentheses_and_semicolons_in_strings_are_not_code() {
    let source = "(mod () (list \"a; ( ) \\\" b\" ';)(' 123)) ; actual";
    let lines = split_text(source);
    let comments = compute_comment_lines(&lines);
    assert_eq!(comments.get(&0), Some(&source.find("; actual").unwrap()));
    let (module, ranges) = make_simple_ranges(&lines);
    assert!(!module);
    assert_eq!(ranges.len(), 2); // args and the entire list expression
    assert_eq!(
        ranges[1].end.character as usize,
        source.find(" ; actual").unwrap() - 1
    );
}

#[test]
fn strings_keep_lexical_state_across_lines_and_edits() {
    let source = "(mod () (list \"first\n; second ( )\nlast\" 123)) ; actual";
    let lines = split_text(source);
    assert_eq!(
        compute_comment_lines(&lines)
            .keys()
            .copied()
            .collect::<Vec<_>>(),
        vec![2]
    );
    let (_, ranges) = make_simple_ranges(&lines);
    assert_eq!(ranges.len(), 2);
    assert_eq!(ranges[1].end.line, 2);
}

#[test]
fn semantic_tokens_use_utf16_positions_and_lengths() {
    let mut lsp = LSPServiceProvider::new(
        Rc::new(FSFileReader::new()),
        Rc::new(EPrintWriter::new()),
        true,
    );
    let file = "file:///unicode.clsp".to_string();
    let source = "(mod () (list \"é😀\" 123))";
    lsp.handle_message(&make_did_open_message(&file, 1, source.to_string()))
        .unwrap();
    let messages = lsp
        .handle_message(&make_get_semantic_tokens_msg(&file, 2))
        .unwrap();
    let response = messages
        .iter()
        .find_map(|m| match m {
            Message::Response(response) => response.result.clone(),
            _ => None,
        })
        .unwrap();
    let tokens: SemanticTokens = serde_json::from_value(response).unwrap();
    let mut line = 0;
    let mut character = 0;
    let mut string_found = false;
    let mut number_found = false;
    for token in tokens.data {
        if token.delta_line > 0 {
            character = 0;
        }
        line += token.delta_line;
        character += token.delta_start;
        assert_eq!(line, 0);
        if token.token_type == TK_STRING_IDX {
            assert_eq!(character, 14);
            assert_eq!(token.length, 5); // quotes, é, and a surrogate pair
            string_found = true;
        }
        if token.token_type == TK_NUMBER_IDX {
            assert_eq!(character, 20);
            assert_eq!(token.length, 3);
            number_found = true;
        }
    }
    assert!(string_found && number_found);
}

#[test]
fn multiline_semantic_tokens_are_split_at_line_boundaries() {
    let mut lsp = LSPServiceProvider::new(
        Rc::new(FSFileReader::new()),
        Rc::new(EPrintWriter::new()),
        true,
    );
    let file = "file:///multiline.clsp".to_string();
    let source = "(mod () (list \"é\n😀\" 123))";
    lsp.handle_message(&make_did_open_message(&file, 1, source.to_string()))
        .unwrap();
    let messages = lsp
        .handle_message(&make_get_semantic_tokens_msg(&file, 2))
        .unwrap();
    let response = messages
        .iter()
        .find_map(|m| match m {
            Message::Response(response) => response.result.clone(),
            _ => None,
        })
        .unwrap();
    let tokens: SemanticTokens = serde_json::from_value(response).unwrap();
    let strings: Vec<_> = tokens
        .data
        .iter()
        .filter(|token| token.token_type == TK_STRING_IDX)
        .collect();
    assert_eq!(strings.len(), 2);
    assert_eq!(strings[0].length, 2);
    assert_eq!(strings[1].length, 3);
}

#[test]
fn diagnostics_use_utf16_positions_after_non_ascii_text() {
    let mut lsp = LSPServiceProvider::new(
        Rc::new(FSFileReader::new()),
        Rc::new(EPrintWriter::new()),
        true,
    );
    let file = "file:///unicode-diagnostic.clsp".to_string();
    let source = "(mod () (list \"é😀\" (MISSING)))";
    let mut messages = lsp
        .handle_message(&make_did_open_message(&file, 1, source.to_string()))
        .unwrap();
    messages.extend(
        lsp.handle_message(&make_get_semantic_tokens_msg(&file, 2))
            .unwrap(),
    );
    let diagnostics: Vec<PublishDiagnosticsParams> = messages
        .iter()
        .filter_map(|message| match message {
            Message::Notification(notification)
                if notification.method == "textDocument/publishDiagnostics" =>
            {
                serde_json::from_value(notification.params.clone()).ok()
            }
            _ => None,
        })
        .collect();
    let diagnostic = diagnostics
        .iter()
        .flat_map(|params| &params.diagnostics)
        .find(|diagnostic| diagnostic.message.contains("MISSING"))
        .expect("undefined function must be diagnosed");
    let character = source[..source.find("MISSING").unwrap()]
        .encode_utf16()
        .count() as u32;
    assert_eq!(diagnostic.range.start, Position::new(0, character));
}

#[test]
fn definition_request_and_target_ranges_use_utf16() {
    let mut lsp = LSPServiceProvider::new(
        Rc::new(FSFileReader::new()),
        Rc::new(EPrintWriter::new()),
        true,
    );
    let file = "file:///unicode-definition.clsp".to_string();
    let source = "(mod (é😀) (list \"é😀\" é😀))";
    lsp.handle_message(&make_did_open_message(&file, 1, source.to_string()))
        .unwrap();
    let character = source[..source.rfind("é😀").unwrap()]
        .encode_utf16()
        .count() as u32;
    let messages = lsp.handle_message(&Message::Request(Request {
        id: RequestId::from(3),
        method: "textDocument/definition".to_string(),
        params: serde_json::json!({"textDocument": {"uri": file}, "position": {"line": 0, "character": character}}),
    })).unwrap();
    let target: lsp_types::GotoDefinitionResponse = messages
        .iter()
        .find_map(|message| match message {
            Message::Response(response) => response
                .result
                .clone()
                .and_then(|value| serde_json::from_value(value).ok()),
            _ => None,
        })
        .expect("definition must resolve before requesting semantic highlighting");
    let location = match target {
        lsp_types::GotoDefinitionResponse::Scalar(location) => location,
        other => panic!("unexpected definition: {:?}", other),
    };
    assert_eq!(
        location.range,
        Range::new(Position::new(0, 6), Position::new(0, 9))
    );
}
