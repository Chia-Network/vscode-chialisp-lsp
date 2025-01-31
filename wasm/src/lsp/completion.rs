use std::borrow::Borrow;
use std::collections::HashMap;
use std::rc::Rc;

use lsp_server::{Message, RequestId, Response};
use lsp_types::{CompletionItem, CompletionList, CompletionParams, CompletionResponse, Position};

use crate::lsp::compopts::get_file_content;
use crate::lsp::parse::{
    find_scope_stack, get_positional_text, is_first_in_list, is_identifier, ParsedDoc,
};
use crate::lsp::types::{urlify, DocData, ParseScope, ReparsedExport, ScopeKind};
use crate::lsp::LSPServiceProvider;
use clvm_tools_rs::compiler::comptypes::{
    Export, HelperForm, ImportLongName, LongNameTranslation, ModuleImportListedName,
    ModuleImportSpec,
};
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

pub struct ModuleCompletionResult {
    pub uri: String,
    pub name: Vec<u8>,
    pub loc: Srcloc,
}

pub trait LSPCompletionRequestHandler {
    fn handle_completion_request(
        &mut self,
        id: RequestId,
        params: &CompletionParams,
    ) -> Result<Vec<Message>, String>;

    fn find_external_name(
        &mut self,
        doc: &ParsedDoc,
        name: &[u8],
    ) -> Option<ModuleCompletionResult>;

    fn find_external_completion(
        &mut self,
        result: &mut Vec<CompletionItem>,
        doc: &ParsedDoc,
        name: &[u8],
    );

    fn update_single_completion_cache(&mut self, import_name: &ImportLongName) -> Option<String>;
}

fn complete_variable_name(
    result_items: &mut Vec<CompletionItem>,
    id: RequestId,
    doc: &DocData,
    found_scopes: &[ParseScope],
    cpl: &[u8],
) {
    for s in found_scopes {
        let viable_completions = s
            .variables
            .iter()
            .filter_map(|sym| {
                if let SExp::Atom(l, n) = sym.borrow() {
                    Some((l, n))
                } else {
                    None
                }
            })
            .filter_map(|(l, n)| {
                if l.line > 0 && l.col > 1 {
                    get_positional_text(
                        doc,
                        &Position {
                            line: (l.line - 1) as u32,
                            character: (l.col - 1) as u32,
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
}

fn complete_function_name(
    result_items: &mut Vec<CompletionItem>,
    id: RequestId,
    scopes: &[ParseScope],
    cpl: &[u8],
) {
    if scopes.is_empty() {
        return;
    }

    let mut viable_completions: Vec<Vec<u8>> = scopes[scopes.len() - 1]
        .functions
        .iter()
        .filter_map(|sexp| {
            if let SExp::Atom(_, name) = sexp.borrow() {
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
}

fn find_match_name_in_exposing(
    exposing: &[ModuleImportListedName],
    short_name: &[u8],
) -> Option<(Vec<u8>, Vec<u8>)> {
    for n in exposing.iter() {
        let n_target = n.alias.as_ref().unwrap_or_else(|| &n.name);
        if n_target.starts_with(short_name) {
            return Some((n_target.to_vec(), n.name.clone()));
        }
    }

    None
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
        let mut completion_items = Vec::new();
        let log = self.log.clone();

        let cpl = self.with_doc_and_parsed(&uristring, |doc, output| {
            let on_previous_character = if params.text_document_position.position.character > 0 {
                Position {
                    line: params.text_document_position.position.line,
                    character: params.text_document_position.position.character - 1,
                }
            } else {
                params.text_document_position.position
            };
            log.log(&format!(
                "doing completion with position {:?}",
                on_previous_character
            ));

            if let Some(cpl) = get_positional_text(doc, &on_previous_character) {
                let mut found_scopes = Vec::new();
                let want_position = Srcloc::new(
                    Rc::new(uristring.clone()),
                    (on_previous_character.line + 1) as usize,
                    (on_previous_character.character + 1) as usize,
                );
                find_scope_stack(&mut found_scopes, &output.scopes, &want_position);

                // Handle variable completions.
                if is_first_in_list(doc, &on_previous_character) {
                    complete_function_name(&mut completion_items, id.clone(), &found_scopes, &cpl);
                } else {
                    complete_variable_name(
                        &mut completion_items,
                        id.clone(),
                        doc,
                        &found_scopes,
                        &cpl,
                    );
                }

                return Some(cpl);
            }

            None
        });

        if completion_items.is_empty() {
            if let (Some(cpl), Some(output)) = (cpl, self.get_parsed(&uristring)) {
                // If the name matches an import, then report that.
                self.find_external_completion(&mut completion_items, &output, &cpl);
            }
        }

        if !completion_items.is_empty() {
            let result = CompletionResponse::List(CompletionList {
                is_incomplete: false,
                items: completion_items,
            });
            let result = serde_json::to_value(result).unwrap();
            let resp = Response {
                id,
                result: Some(result),
                error: None,
            };
            res.push(Message::Response(resp));
        }

        Ok(res)
    }

    fn find_external_name(
        &mut self,
        doc: &ParsedDoc,
        name: &[u8],
    ) -> Option<ModuleCompletionResult> {
        self.log.log(&format!("find_external_completion {}", decode_string(name)));
        let (_, long_of_name) = ImportLongName::parse(name);
        let (prefix, short_name) = long_of_name.parent_and_name();
        let mut filename = None;
        let mut match_name = short_name.clone();
        let mut output_name = short_name.clone();

        for helper in doc.helpers.values() {
            if let Ok(HelperForm::Defnsref(nsref)) = &helper.parsed {
                self.log.log(&format!(
                    "found an import {}",
                    HelperForm::Defnsref(nsref.clone()).to_sexp()
                ));
                filename = self.update_single_completion_cache(&nsref.longname);
                self.log.log(&format!("filename {:?}", filename));

                // Try to get the parse of the target.
                let output =
                    if let Some(output) = filename.as_ref().and_then(|f| self.get_parsed(&f)) {
                        output
                    } else {
                        continue;
                    };

                let matching_prefix = match &nsref.specification {
                    ModuleImportSpec::Qualified(qmi) => {
                        // The qualification requires that either the full prefix of the
                        // import or the given override combined with an export name component
                        // gives the target name.
                        if let Some(prefix) = &prefix {
                            if let Some(t) = &qmi.target {
                                t.name == *prefix
                            } else {
                                qmi.name == *prefix
                            }
                        } else {
                            false
                        }
                    }
                    ModuleImportSpec::Exposing(_, names) => {
                        if let Some((oname, mname)) =
                            find_match_name_in_exposing(names, &short_name)
                        {
                            match_name = mname;
                            output_name = oname;
                            self.log.log(&format!("match name in module: {}", decode_string(&match_name)));
                            true
                        } else {
                            false
                        }
                    }
                    ModuleImportSpec::Hiding(_, names) => {
                        if let Some((oname, mname)) =
                            find_match_name_in_exposing(names, &short_name)
                        {
                            false
                        } else {
                            true
                        }
                    }
                };

                self.log.log(&format!("module completion {} matching_prefix {}", decode_string(&match_name), matching_prefix));
                if matching_prefix {
                    // Check exports in case it's a module.
                    for export in output.exports.values() {
                        if let (Some(filename), Ok(Export::MainProgram(desc))) =
                            (filename.as_ref(), &export.parsed)
                        {
                            if match_name == b"program" {
                                return Some(ModuleCompletionResult {
                                    uri: filename.clone(),
                                    loc: desc.loc.clone(),
                                    name: output_name.clone(),
                                });
                            }
                        }
                    }

                    for helper in output.helpers.values() {
                        if let (Some(filename), Ok(h)) = (filename.as_ref(), &helper.parsed) {
                            self.log.log(&format!("module completion: looking at helper {}", h.to_sexp()));
                            if match_name == *h.name() {
                                return Some(ModuleCompletionResult {
                                    uri: filename.clone(),
                                    loc: h.loc(),
                                    name: output_name.clone(),
                                });
                            }
                        } else {
                            self.log.log(&format!("helper error {helper:?}"));
                        }
                    }
                }
            }
        }

        None
    }

    fn find_external_completion(
        &mut self,
        res: &mut Vec<CompletionItem>,
        doc: &ParsedDoc,
        name: &[u8],
    ) {
        let result = if let Some(result) = self.find_external_name(doc, name) {
            result
        } else {
            return;
        };

        res.push(CompletionItem {
            label: decode_string(&result.name),
            ..Default::default()
        });
    }

    fn update_single_completion_cache(&mut self, import_name: &ImportLongName) -> Option<String> {
        for ext in [".clinc", ".clsp"].iter() {
            let partial_filename = decode_string(
                &import_name.as_u8_vec(LongNameTranslation::Filename(ext.to_string())),
            );
            self.log.log(&format!("try import file name {partial_filename:?}"));

            let (true_filename, content) = if let Ok((filename, content)) = get_file_content(
                self.log.clone(),
                self.fs.clone(),
                self.get_workspace_root(),
                &self.config.include_paths,
                &partial_filename,
            ) {
                (filename, content)
            } else {
                self.log.log(&format!("couldn't find {partial_filename} in path"));
                continue;
            };

            let url_of_filename = urlify(&true_filename);
            self.log.log(&format!("url_of_filename {url_of_filename}"));

            self.save_doc(url_of_filename.clone(), content);

            self.ensure_parsed_document(&url_of_filename, Some(false));

            return Some(url_of_filename);
        }

        None
    }
}
