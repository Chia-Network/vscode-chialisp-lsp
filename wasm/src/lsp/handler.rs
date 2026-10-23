use std::borrow::Borrow;
use std::path::Path;
use std::rc::Rc;

use lsp_types::{
    request::CodeActionRequest, request::Completion, request::GotoDefinition, request::Initialize,
    request::SemanticTokensFullRequest, CodeAction, CodeActionKind, CodeActionOrCommand,
    CodeActionParams, Command, DidChangeTextDocumentParams, DidChangeWatchedFilesParams,
    DidCloseTextDocumentParams, DidOpenTextDocumentParams, GotoDefinitionParams,
    GotoDefinitionResponse, Location, Url,
};

use lsp_server::{ErrorCode, Message, Notification, RequestId, Response};

use crate::lsp::completion::LSPCompletionRequestHandler;
use crate::lsp::parse::IncludedFileSpec;
use crate::lsp::patch::{compute_comment_lines, split_text, LSPServiceProviderApplyDocumentPatch};
use crate::lsp::semtok::LSPSemtokRequestHandler;
use crate::lsp::types::{cast, ConfigJson, DocData, DocRange, InitState, LSPServiceProvider};
use chialisp::compiler::comptypes::LongNameTranslation;
use chialisp::compiler::sexp::decode_string;
use chialisp::compiler::srcloc::Srcloc;

pub trait LSPServiceMessageHandler {
    fn handle_message(&mut self, msg: &Message) -> Result<Vec<Message>, String>;
    fn handle_message_from_string(&mut self, msg: String) -> Result<Vec<Message>, String>;
}

fn is_real_include(l: &Srcloc) -> bool {
    let borrowed_file: &String = l.file.borrow();
    !borrowed_file.starts_with('*')
}

impl LSPServiceProvider {
    fn goto_definition(
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
        self.ensure_definition_index(&docname);
        let docpos = self
            .get_doc(&docname)
            .map(|doc| crate::lsp::positions::position_to_byte(&doc.text, docpos))
            .unwrap_or(docpos);
        let wantloc = Srcloc::new(
            Rc::new(docname.clone()),
            (docpos.line + 1) as usize,
            (docpos.character + 1) as usize,
        );
        if let Some(defs) = self.goto_defs.get(&docname) {
            for kv in defs.iter() {
                if is_real_include(kv.1) && kv.0.loc.overlap(&wantloc) {
                    let filename: &String = kv.1.file.borrow();
                    let uri = crate::lsp::types::urlify(filename);
                    let range = DocRange::from_srcloc(kv.1.clone()).to_range();
                    let range = self
                        .get_doc(&uri)
                        .map(|doc| crate::lsp::positions::range_to_utf16(&doc.text, range))
                        .unwrap_or(range);
                    if let Ok(uri) = Url::parse(&uri) {
                        goto_response = Some(Location { uri, range });
                    }
                    break;
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
                match inc {
                    IncludedFileSpec::Include(inc) => {
                        if inc.filename == file_name {
                            found_hash = Some(h.clone());
                            break;
                        }
                    }
                    IncludedFileSpec::Import(_found, imp) => {
                        // The import could resolve to a .clinc or a .clsp.  We have the parsed
                        // filename here so we can get the extension.
                        if let Some(ext_start) = file_name
                            .iter()
                            .copied()
                            .enumerate()
                            .rev()
                            .find(|(_i, c)| *c == b'.')
                            .map(|(i, _)| i)
                        {
                            let string_of_ext = decode_string(&file_name[ext_start..]);
                            let import_name = imp
                                .longname
                                .as_u8_vec(LongNameTranslation::Filename(string_of_ext));
                            if import_name == file_name {
                                found_hash = Some(h.clone());
                                break;
                            }
                        }
                    }
                }
            }

            if let Some(h) = found_hash {
                match found.includes.get_mut(&h) {
                    Some(IncludedFileSpec::Include(inc)) => {
                        inc.found = Some(file_found);
                    }
                    Some(IncludedFileSpec::Import(found, _)) => {
                        *found = Some(file_found);
                    }
                    _ => {}
                }
            }
        }
    }

    // Return the includes that couldn't be resolved.
    pub fn check_for_missing_include_files(&mut self, uristring: &str) -> Vec<IncludedFileSpec> {
        let includes: Vec<_> = self
            .get_parsed(uristring)
            .map(|doc| doc.includes.into_values().collect())
            .unwrap_or_default();
        let mut missing = Vec::new();
        for include in includes {
            let filename = match &include {
                IncludedFileSpec::Include(inc) => {
                    if inc.filename.starts_with(b"*") {
                        continue;
                    }
                    inc.filename.clone()
                }
                IncludedFileSpec::Import(_, imp) => self.get_filename_of_import(&imp.longname),
            };
            let found = self.resolve_file_uri(&decode_string(&filename)).is_some();
            self.update_include_state(uristring, &filename, found);
            if !found {
                missing.push(include);
            }
        }
        missing
    }

    pub fn reconfigure(&mut self) -> Option<ConfigJson> {
        self.log
            .log(&format!("config path {:?}", self.get_config_path()));
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
        let doc = self.get_doc(&uristring);
        let emit_quick_fix_action =
            |result_messages: &mut Vec<Message>, name_loc, filename: Vec<u8>| {
                let range = DocRange::from_srcloc(name_loc).to_range();
                let range = doc
                    .as_ref()
                    .map(|doc| crate::lsp::positions::range_to_utf16(&doc.text, range))
                    .unwrap_or(range);
                if range == params.range {
                    let code_action = vec![CodeActionOrCommand::CodeAction(CodeAction {
                        title: "Locate include path".to_string(),
                        kind: Some(CodeActionKind::QUICKFIX),
                        diagnostics: None,
                        edit: None,
                        command: Some(Command {
                            title: "Locate include path".to_string(),
                            command: "chialisp.locateIncludePath".to_string(),
                            arguments: Some(vec![
                                serde_json::to_value(decode_string(&filename)).unwrap()
                            ]),
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
            };

        self.ensure_parsed_document(&uristring);
        for include in self.check_for_missing_include_files(&uristring) {
            match include {
                IncludedFileSpec::Include(inc) => {
                    emit_quick_fix_action(&mut result_messages, inc.name_loc, inc.filename)
                }
                IncludedFileSpec::Import(_, imp) => {
                    let filename = self.get_filename_of_import(&imp.longname);
                    emit_quick_fix_action(&mut result_messages, imp.nl, filename);
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
        if let Message::Request(req) = msg {
            let error = |code: ErrorCode, text: &str| {
                Ok(vec![Message::Response(Response::new_err(
                    req.id.clone(),
                    code as i32,
                    text.to_owned(),
                ))])
            };
            if self.shutdown_requested {
                return error(ErrorCode::InvalidRequest, "Server is shutting down");
            }
            if req.method == "initialize" {
                if self.init.is_some() {
                    return error(ErrorCode::InvalidRequest, "Server is already initialized");
                }
                let Ok((_, params)) = cast::<Initialize>(req.clone()) else {
                    return error(ErrorCode::InvalidParams, "Invalid initialize parameters");
                };
                self.init = Some(InitState::Initialized(Rc::new(params)));
                if let Some(config) = self.reconfigure() {
                    self.config = config;
                }
                return Ok(vec![Message::Response(Response::new_ok(
                    req.id.clone(),
                    serde_json::json!({
                        "capabilities": Self::get_capabilities(),
                        "serverInfo": {"name": "chialisp-lsp", "version": env!("CARGO_PKG_VERSION")}
                    }),
                ))]);
            }
            if self.init.is_none() {
                return error(
                    ErrorCode::ServerNotInitialized,
                    "Expected initialize request",
                );
            }
            macro_rules! dispatch {
                ($kind:ty, $handler:ident) => {
                    match cast::<$kind>(req.clone()) {
                        Ok((id, params)) => self.$handler(id, &params),
                        Err(_) => error(ErrorCode::InvalidParams, "Invalid request parameters"),
                    }
                };
            }
            return match req.method.as_str() {
                "shutdown" => {
                    self.shutdown_requested = true;
                    Ok(vec![Message::Response(Response::new_ok(
                        req.id.clone(),
                        serde_json::Value::Null,
                    ))])
                }
                "textDocument/semanticTokens/full" => {
                    dispatch!(SemanticTokensFullRequest, handle_semantic_tokens)
                }
                "textDocument/definition" => dispatch!(GotoDefinition, goto_definition),
                "textDocument/completion" => dispatch!(Completion, handle_completion_request),
                "textDocument/codeAction" => {
                    dispatch!(CodeActionRequest, handle_code_action_request)
                }
                _ => error(ErrorCode::MethodNotFound, "Unknown request method"),
            };
        }
        let Message::Notification(not) = msg else {
            return Ok(vec![]);
        };
        if self.init.is_none() || self.shutdown_requested {
            return Ok(vec![]);
        }
        match not.method.as_str() {
            "textDocument/didOpen" => {
                let params: DidOpenTextDocumentParams =
                    serde_json::from_value(not.params.clone()).map_err(|e| e.to_string())?;
                let uri = params.text_document.uri.to_string();
                let text = split_text(&params.text_document.text);
                let comments = compute_comment_lines(&text);
                self.open_documents.insert(uri.clone());
                self.resolved_files.clear();
                self.save_doc(
                    uri.clone(),
                    DocData {
                        fullname: uri,
                        text,
                        comments,
                        version: params.text_document.version,
                    },
                );
            }
            "textDocument/didChange" => {
                let params: DidChangeTextDocumentParams =
                    serde_json::from_value(not.params.clone()).map_err(|e| e.to_string())?;
                let uri = params.text_document.uri.to_string();
                if self
                    .get_doc(&uri)
                    .is_some_and(|doc| params.text_document.version <= doc.version)
                {
                    return Ok(vec![]);
                }
                self.apply_document_patch(
                    &uri,
                    params.text_document.version,
                    &params.content_changes,
                );
            }
            "textDocument/didClose" => {
                let params: DidCloseTextDocumentParams =
                    serde_json::from_value(not.params.clone()).map_err(|e| e.to_string())?;
                let uri = params.text_document.uri.to_string();
                self.open_documents.remove(&uri);
                self.invalidate_document(&uri);
                self.document_collection.borrow_mut().remove(&uri);
                self.resolved_files.clear();
                self.analyze_open_documents();
                let mut replies = vec![Message::Notification(Notification::new(
                    "textDocument/publishDiagnostics".to_owned(),
                    serde_json::json!({"uri": uri, "diagnostics": []}),
                ))];
                replies.extend(self.produce_error_list());
                return Ok(replies);
            }
            "workspace/didChangeWatchedFiles" => {
                let params: DidChangeWatchedFilesParams =
                    serde_json::from_value(not.params.clone()).map_err(|e| e.to_string())?;
                for change in params.changes {
                    let uri = change.uri.to_string();
                    if uri.ends_with("/chialisp.json") {
                        self.config = self.reconfigure().unwrap_or_default();
                        self.parsed_documents.clear();
                        self.goto_defs.clear();
                        self.thrown_errors.clear();
                    } else {
                        self.invalidate_document(&uri);
                        if !self.open_documents.contains(&uri) {
                            self.document_collection.borrow_mut().remove(&uri);
                        }
                    }
                }
                self.resolved_files.clear();
            }
            _ => return Ok(vec![]),
        }
        self.analyze_open_documents();
        Ok(self.produce_error_list())
    }

    fn handle_message_from_string(&mut self, msg: String) -> Result<Vec<Message>, String> {
        if let Ok(input_msg) = serde_json::from_str::<Message>(&msg) {
            self.handle_message(&input_msg)
        } else {
            Err("Could not decode as json message".to_string())
        }
    }
}
