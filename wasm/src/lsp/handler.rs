use std::borrow::Borrow;
use std::path::Path;
use std::rc::Rc;

use lsp_types::{
    request::CodeActionRequest, request::Completion, request::GotoDefinition, request::Initialize,
    request::SemanticTokensFullRequest, CodeAction, CodeActionKind, CodeActionOrCommand,
    CodeActionParams, Command, DidChangeTextDocumentParams, DidChangeWatchedFilesParams,
    DidOpenTextDocumentParams, GotoDefinitionParams, GotoDefinitionResponse, Location, Position,
    Range, Url,
};

use lsp_server::{ErrorCode, Message, RequestId, Response};

use crate::lsp::completion::LSPCompletionRequestHandler;
use crate::lsp::parse::get_positional_text;
use crate::lsp::patch::{compute_comment_lines, split_text, LSPServiceProviderApplyDocumentPatch};
use crate::lsp::semtok::LSPSemtokRequestHandler;
use crate::lsp::types::{
    cast, ConfigJson, DocData, DocRange, IncludeData, InitState, LSPServiceProvider,
};
use clvm_tools_rs::compiler::sexp::decode_string;
use clvm_tools_rs::compiler::srcloc::Srcloc;

pub trait LSPServiceMessageHandler {
    fn handle_message(&mut self, msg: &Message) -> Result<Vec<Message>, String>;
    fn handle_message_from_string(&mut self, msg: String) -> Result<Vec<Message>, String>;
}

fn is_real_include(l: &Srcloc) -> bool {
    let borrowed_file: &String = l.file.borrow();
    !borrowed_file.starts_with('*')
}

impl LSPServiceProvider {
    // Adding support for imported modules:
    //
    // start with no match to checked name
    //
    // for each unread module referenced by an import, read and cache a list of defined names
    // (can be a quick operation) by module name.
    //
    // If the import defines the name, add that fact to the cache.
    //
    // If we find a helper defining a specific name, we can also stop.
    pub fn goto_definition(
        &mut self,
        id: RequestId,
        params: &GotoDefinitionParams,
    ) -> Result<Vec<Message>, String> {
        let mut res = Vec::new();
        let mut goto_response = None;
        let docname = params
            .text_document_position_params
            .text_document
            .uri
            .to_string();
        let docpos = params.text_document_position_params.position;
        let wantloc = Srcloc::new(
            Rc::new(docname.clone()),
            (docpos.line + 1) as usize,
            (docpos.character + 1) as usize,
        );
        if let Some(defs) = self.goto_defs.get(&docname) {
            for kv in defs.iter() {
                if is_real_include(kv.1) && kv.0.loc.overlap(&wantloc) {
                    let filename: &String = kv.1.file.borrow();
                    goto_response = Some(Location {
                        uri: Url::parse(filename).unwrap(),
                        range: Range {
                            start: Position {
                                line: (kv.1.line - 1) as u32,
                                character: (kv.1.col - 1) as u32,
                            },
                            end: Position {
                                line: (kv.1.line - 1) as u32,
                                character: (kv.1.col + kv.1.len().unwrap_or(1) - 1) as u32,
                            },
                        },
                    });
                    break;
                }
            }
        }
        if goto_response.is_none() {
            if let (Some(doc), Some(parsed)) = (self.get_doc(&docname), self.get_parsed(&docname)) {
                let on_previous_character =
                    if params.text_document_position_params.position.character > 0 {
                        Position {
                            line: params.text_document_position_params.position.line,
                            character: params.text_document_position_params.position.character - 1,
                        }
                    } else {
                        params.text_document_position_params.position
                    };
                if let Some(cpl) = get_positional_text(&doc, &on_previous_character) {
                    if let Some(result) = self.find_external_name(&parsed, &cpl) {
                        let pos = Position {
                            line: (result.loc.line - 1) as u32,
                            character: (result.loc.col - 1) as u32,
                        };
                        goto_response = Some(Location {
                            uri: Url::parse(&result.uri).unwrap(),
                            range: Range {
                                start: pos,
                                end: pos,
                            },
                        });
                    }
                }
            }
        }

        let result = goto_response.map(GotoDefinitionResponse::Scalar);
        let result = serde_json::to_value(result).unwrap();
        let resp = Response {
            id,
            result: Some(result),
            error: None,
        };
        res.push(Message::Response(resp));

        Ok(res)
    }

    // Update include state
    fn update_include_state(&mut self, parsed_file: &str, file_name: &[u8], file_found: bool) {
        if let Some(found) = self.parsed_documents.get_mut(parsed_file) {
            let mut found_hash = None;
            for (h, inc) in found.includes.iter() {
                if inc.filename == file_name {
                    found_hash = Some(h.clone());
                    break;
                }
            }

            if let Some(h) = &found_hash {
                if let Some(inc) = found.includes.get_mut(h) {
                    inc.found = Some(file_found);
                }
            }
        }
    }

    // Return the includes that couldn't be resolved.
    pub fn check_for_missing_include_files(&mut self, uristring: &str) -> Vec<IncludeData> {
        let mut to_resolve = Vec::new();

        // Find includes we need to resolve
        if let Some(doc) = self.parsed_documents.get(uristring) {
            for (_, i) in doc.includes.iter() {
                if i.found != Some(true) {
                    to_resolve.push((uristring, i.clone()));
                }
            }
        }

        // Errors is specifically the ones we tried to resolve and failed.
        let mut ask_ui_for_resolution = Vec::new();

        let to_read_files: Vec<(String, IncludeData)> = to_resolve
            .iter()
            .map(|(parsed, i)| (parsed.to_string(), i.clone()))
            .collect();
        for (parsed, i) in to_read_files.iter() {
            if i.filename.is_empty() || i.filename[0] == b'*' || i.found == Some(true) {
                continue;
            }

            let mut found_include = false;
            for path in self.config.include_paths.iter() {
                let target_name = Path::new(&path)
                    .join(&decode_string(&i.filename))
                    .to_str()
                    .map(|o| o.to_owned());
                if let Some(target) = target_name {
                    if self.fs.read_content(&target).is_ok() {
                        found_include = true;
                        self.update_include_state(parsed, &i.filename, true);
                        break;
                    }
                }
            }

            if !found_include {
                ask_ui_for_resolution.push(i.clone());
                self.update_include_state(parsed, &i.filename, false);
            }
        }

        ask_ui_for_resolution
    }

    pub fn reconfigure(&mut self) -> Option<ConfigJson> {
        self.get_config_path()
            .and_then(|config_path| self.fs.read_content(&config_path).ok())
            .and_then(|config_data| serde_json::from_str(&config_data).ok())
            .map(|config: ConfigJson| {
                let mut result = config.clone();
                result.include_paths.clear();

                for p in config.include_paths.iter() {
                    if p.starts_with('.') {
                        if let Some(path_str) = self.get_relative_path(p) {
                            result.include_paths.push(path_str.to_owned());
                        }
                    } else if let Some(ps) = Path::new(p).to_str() {
                        result.include_paths.push(ps.to_owned());
                    }
                }

                result
            })
    }

    fn handle_code_action_request(
        &mut self,
        id: RequestId,
        params: &CodeActionParams,
    ) -> Result<Vec<Message>, String> {
        let uristring = params.text_document.uri.to_string();
        let mut result_messages = Vec::new();

        // Double check parsed state.
        self.ensure_parsed_document(&uristring, None);

        if let Some(doc) = self.parsed_documents.get(&uristring) {
            for (_, inc) in doc.includes.iter() {
                if DocRange::from_srcloc(inc.name_loc.clone()).to_range() == params.range {
                    let code_action = vec![CodeActionOrCommand::CodeAction(CodeAction {
                        title: "Locate include path".to_string(),
                        kind: Some(CodeActionKind::QUICKFIX),
                        diagnostics: None,
                        edit: None,
                        command: Some(Command {
                            title: "Locate include path".to_string(),
                            command: "chialisp.locateIncludePath".to_string(),
                            arguments: Some(vec![serde_json::to_value(&decode_string(
                                &inc.filename,
                            ))
                            .unwrap()]),
                        }),
                        is_preferred: None,
                        disabled: None,
                        data: None,
                    })];
                    result_messages.push(Message::Response(Response {
                        id: id.clone(),
                        result: Some(serde_json::to_value(code_action).unwrap()),
                        error: None,
                    }));
                }
            }
        }

        if result_messages.is_empty() {
            // Return a reply regardless of what happened.
            let code_action: Vec<CodeActionOrCommand> = vec![];
            result_messages.push(Message::Response(Response {
                id: id.clone(),
                result: Some(serde_json::to_value(code_action).unwrap()),
                error: None,
            }));
        }

        Ok(result_messages)
    }
}

impl LSPServiceMessageHandler for LSPServiceProvider {
    fn handle_message(&mut self, msg: &Message) -> Result<Vec<Message>, String> {
        // Handle initialization.
        if self.init.is_none() {
            if let Message::Request(req) = msg {
                if req.method == "initialize" {
                    if let Ok((_, params)) = cast::<Initialize>(req.clone()) {
                        self.init = Some(InitState::Initialized(Rc::new(params)));
                        // Try to read the config data
                        if let Some(config) = self.reconfigure() {
                            // We have a config file and can read the filesystem.
                            self.config = config;
                        }

                        let server_capabilities = LSPServiceProvider::get_capabilities();

                        let initialize_data = serde_json::json!({
                            "capabilities": server_capabilities,
                            "serverInfo": {
                                "name": "chialisp-lsp",
                                "version": "0.1"
                            }
                        });

                        let resp = Response::new_ok(req.id.clone(), initialize_data);

                        return Ok(vec![Message::Response(resp)]);
                    }
                } else {
                    let resp = Response::new_err(
                        req.id.clone(),
                        ErrorCode::ServerNotInitialized as i32,
                        format!("expected initialize request, got {:?}", req),
                    );
                    return Ok(vec![Message::Response(resp)]);
                }
            }
        }

        match msg {
            Message::Request(req) => {
                if let Ok((id, params)) = cast::<SemanticTokensFullRequest>(req.clone()) {
                    return self.handle_semantic_tokens(id, &params);
                } else if let Ok((id, params)) = cast::<GotoDefinition>(req.clone()) {
                    return self.goto_definition(id, &params);
                } else if let Ok((id, params)) = cast::<Completion>(req.clone()) {
                    return self.handle_completion_request(id, &params);
                } else if let Ok((id, params)) = cast::<CodeActionRequest>(req.clone()) {
                    return self.handle_code_action_request(id, &params);
                } else {
                    self.log.log(&format!("unknown request {:?}", req));
                };
                // ...
            }
            Message::Response(resp) => {
                self.log.log(&format!("got response: {:?}", resp));
            }
            Message::Notification(not) => {
                self.log.log(&format!("got notification: {:?}", not));
                if not.method == "textDocument/didOpen" {
                    let stringified_params = serde_json::to_string(&not.params).unwrap();
                    if let Ok(params) =
                        serde_json::from_str::<DidOpenTextDocumentParams>(&stringified_params)
                    {
                        let doc_data = split_text(&params.text_document.text);
                        let comments = compute_comment_lines(&doc_data);
                        let fullname = params.text_document.uri.to_string();
                        self.save_doc(
                            fullname.clone(),
                            DocData {
                                fullname,
                                text: doc_data,
                                version: params.text_document.version,
                                comments,
                            },
                        );
                    } else {
                        self.log.log("cast failed in didOpen");
                    }

                    return Ok(self.produce_error_list());
                } else if not.method == "workspace/didChangeWatchedFiles" {
                    let stringified_params = serde_json::to_string(&not.params).unwrap();
                    let mut matching_file_for_resync = false;
                    if let Ok(params) =
                        serde_json::from_str::<DidChangeWatchedFilesParams>(&stringified_params)
                    {
                        for change in params.changes.iter() {
                            let doc_id = change.uri.to_string();

                            if doc_id.ends_with("chialisp.json") {
                                matching_file_for_resync = true;
                                if let Some(config) = self.reconfigure() {
                                    // We have a config file and can read the filesystem.
                                    self.log.log("reconfigured");
                                    self.config = config;
                                    self.parsed_documents.clear();
                                    self.goto_defs.clear();
                                }
                            } else if self
                                .workspace_file_extensions_to_resync_for
                                .iter()
                                .any(|e| doc_id.ends_with(e))
                            {
                                matching_file_for_resync = true;
                            }
                        }
                    }

                    // Parse all documents and reproduce diagnostics
                    // This is expensive but I think i may need to do it to
                    // ensure that after a reparse we completely dismiss all
                    // errors, but only when selected files are changed.
                    if matching_file_for_resync {
                        let to_reparse_docs = self.get_doc_keys().to_vec();

                        for docname in to_reparse_docs.iter() {
                            self.parse_document_and_store_errors(docname);
                        }
                    }

                    let error_msgs = self.produce_error_list();
                    return Ok(error_msgs);
                } else if not.method == "textDocument/didChange" {
                    let stringified_params = serde_json::to_string(&not.params).unwrap();
                    if let Ok(params) =
                        serde_json::from_str::<DidChangeTextDocumentParams>(&stringified_params)
                    {
                        let doc_id = params.text_document.uri.to_string();

                        self.apply_document_patch(
                            &doc_id,
                            params.text_document.version,
                            &params.content_changes,
                        );

                        let error_msgs = self.produce_error_list();
                        return Ok(error_msgs);
                    } else {
                        self.log.log("case failed in didChange");
                    }
                } else {
                    self.log.log(&format!("not sure what we got: {:?}", not));
                }
            }
        }

        Ok(vec![])
    }

    fn handle_message_from_string(&mut self, msg: String) -> Result<Vec<Message>, String> {
        if let Ok(input_msg) = serde_json::from_str::<Message>(&msg) {
            self.handle_message(&input_msg)
        } else {
            Err("Could not decode as json message".to_string())
        }
    }
}
