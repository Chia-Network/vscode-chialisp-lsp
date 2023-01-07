use std::rc::Rc;

use lsp_server::{Message, RequestId, Response};
use lsp_types::{CompletionItem, CompletionList, CompletionParams, CompletionResponse, Position};

use crate::lsp::parse::{
    find_scope_stack, get_positional_text, is_first_in_list, is_identifier, ParseScope
};
use crate::lsp::types::{DocData, ScopeKind};
use crate::lsp::LSPServiceProvider;
use clvm_tools_rs::compiler::prims::prims;
use clvm_tools_rs::compiler::sexp::{decode_string, SExp};
use clvm_tools_rs::compiler::srcloc::Srcloc;

lazy_static! {
    pub static ref PRIM_NAMES: Vec<Vec<u8>> = {
        let mut p: Vec<Vec<u8>> = prims().iter().map(|p| p.0.clone()).collect();
        p.push(b"if".to_vec());
        p.push(b"function".to_vec());
        p.push(b"list".to_vec());
        p
    };
}

pub trait LSPCompletionRequestHandler {
    fn handle_completion_request(
        &mut self,
        id: RequestId,
        params: &CompletionParams,
    ) -> Result<Vec<Message>, String>;
}

fn complete_variable_name(
    res: &mut Vec<Message>,
    id: RequestId,
    doc: &DocData,
    found_scopes: &[ParseScope],
    cpl: &[u8],
) {
    let mut result_items = Vec::new();

    for s in found_scopes {
        let viable_completions = s
            .variables
            .iter()
            .filter_map(|sym| {
                if let SExp::Atom(l, n) = sym {
                    Some((l, n))
                } else {
                    None
                }
            })
            .filter_map(|(l, n)| {
                if l.line > 0 && l.col > 0 {
                    get_positional_text(
                        doc,
                        &Position {
                            line: (l.line - 1) as u32,
                            character: l.col as u32,
                        },
                    )
                    .filter(|l| is_identifier(l))
                    .map(Some)
                    .unwrap_or_else(|| Some(n.clone()))
                } else {
                    Some(n.clone())
                }
            })
            .filter(|real_name| real_name.starts_with(cpl));

        for real_name in viable_completions {
            result_items.push(CompletionItem {
                label: decode_string(&real_name),
                ..Default::default()
            });
        }

        // Break if we reached a function boundary since there
        // are no closures here.
        if matches!(s.kind, ScopeKind::Function) {
            break;
        }
    }

    let result = CompletionResponse::List(CompletionList {
        is_incomplete: false,
        items: result_items,
    });
    let result = serde_json::to_value(&result).unwrap();
    let resp = Response {
        id,
        result: Some(result),
        error: None,
    };
    res.push(Message::Response(resp));
}

fn complete_function_name(
    res: &mut Vec<Message>,
    id: RequestId,
    scopes: &[ParseScope],
    cpl: &[u8],
) {
    let mut result_items = Vec::new();

    if scopes.is_empty() {
        return;
    }

    let mut viable_completions: Vec<Vec<u8>> = scopes[scopes.len() - 1]
        .functions
        .iter()
        .filter_map(|sexp| {
            if let SExp::Atom(_, name) = sexp {
                Some(name.clone())
            } else {
                None
            }
        })
        .collect();

    viable_completions.append(&mut PRIM_NAMES.clone());
    viable_completions.retain(|real_name| real_name.starts_with(cpl));

    for real_name in viable_completions {
        result_items.push(CompletionItem {
            label: decode_string(&real_name),
            ..Default::default()
        });
    }

    let result = CompletionResponse::List(CompletionList {
        is_incomplete: false,
        items: result_items,
    });
    let result = serde_json::to_value(&result).unwrap();
    let resp = Response {
        id,
        result: Some(result),
        error: None,
    };
    res.push(Message::Response(resp));
}

// Completions:
//
// Functions:
//
// - Top level: show top level functions
// - Inside macro: Show functions at this level
// - Inside function: Show top level functions
//
// Variables:
//
// - Top level: show top level variables
// - Inside macro: Show variables at this level
// - Inside function: Show top level constants and
//   variables at this level.
//
// If at the head of a list: show function completions
// Otherwise show variable completions.
impl LSPCompletionRequestHandler for LSPServiceProvider {
    fn handle_completion_request(
        &mut self,
        id: RequestId,
        params: &CompletionParams,
    ) -> Result<Vec<Message>, String> {
        let uristring = params.text_document_position.text_document.uri.to_string();
        self.parse_document_and_store_errors(&uristring);
        let mut res = self.produce_error_list();

        self.with_doc_and_parsed(&uristring, |doc, output| {
            if let Some(cpl) = get_positional_text(doc, &params.text_document_position.position) {
                let mut found_scopes = Vec::new();
                let pos = params.text_document_position.position;
                let want_position = Srcloc::new(
                    Rc::new(uristring.clone()),
                    (pos.line + 1) as usize,
                    (pos.character + 1) as usize,
                );
                find_scope_stack(&mut found_scopes, &output.scopes, &want_position);

                // Handle variable completions.
                if is_first_in_list(doc, &params.text_document_position.position) {
                    complete_function_name(&mut res, id, &found_scopes, &cpl);
                } else {
                    complete_variable_name(&mut res, id, doc, &found_scopes, &cpl);
                }
            }
            Some(res)
        })
        .map(Ok)
        .unwrap_or_else(|| Ok(vec![]))
    }
}
