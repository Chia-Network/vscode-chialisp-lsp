use serde::{Deserialize, Serialize};
use std::borrow::Borrow;
use std::cell::{Ref, RefCell};
use std::cmp::{PartialOrd, min};
use std::collections::{BTreeMap, HashMap, HashSet};
use std::default::Default;
use std::mem::swap;
use std::path::{Path, PathBuf};
#[cfg(test)]
use std::str::FromStr;
use std::rc::Rc;

use lsp_server::{ExtractError, Message, Notification, Request, RequestId};

use lsp_types::{
    CodeActionKind, CodeActionProviderCapability, CodeActionOptions, CompletionOptions, Diagnostic, InitializeParams, OneOf, Position, PublishDiagnosticsParams,
    Range, SemanticTokenModifier, SemanticTokenType, SemanticTokensFullOptions,
    SemanticTokensLegend, SemanticTokensOptions, SemanticTokensServerCapabilities,
    ServerCapabilities, TextDocumentSyncCapability, TextDocumentSyncKind, WorkDoneProgressOptions
};

use percent_encoding::percent_decode;
use url::{Host, Url};

use crate::interfaces::{IFileReader, ILogWriter};
use crate::lsp::compopts::{get_file_content, LSPCompilerOpts};
use crate::lsp::parse::{make_simple_ranges, ParsedDoc};
use crate::lsp::patch::stringify_doc;
use crate::lsp::reparse::{combine_new_with_old_parse, reparse_subset};
use crate::lsp::semtok::SemanticTokenSortable;
use clvm_tools_rs::compiler::comptypes::{BodyForm, CompileErr, CompilerOpts, HelperForm};
use clvm_tools_rs::compiler::prims::prims;
use clvm_tools_rs::compiler::sexp::{decode_string, SExp};
use clvm_tools_rs::compiler::srcloc::Srcloc;

lazy_static! {
    pub static ref TOKEN_TYPES: Vec<SemanticTokenType> = {
        vec![
            SemanticTokenType::PARAMETER,
            SemanticTokenType::VARIABLE,
            SemanticTokenType::FUNCTION,
            SemanticTokenType::MACRO,
            SemanticTokenType::KEYWORD,
            SemanticTokenType::COMMENT,
            SemanticTokenType::STRING,
            SemanticTokenType::NUMBER,
        ]
    };
    pub static ref TOKEN_MODIFIERS: Vec<SemanticTokenModifier> = {
        vec![
            SemanticTokenModifier::DEFINITION,
            SemanticTokenModifier::READONLY,
            SemanticTokenModifier::DOCUMENTATION,
        ]
    };
}

pub const TK_PARAMETER_IDX: u32 = 0;
pub const TK_VARIABLE_IDX: u32 = 1;
pub const TK_FUNCTION_IDX: u32 = 2;
pub const TK_MACRO_IDX: u32 = 3;
pub const TK_KEYWORD_IDX: u32 = 4;
pub const TK_COMMENT_IDX: u32 = 5;
pub const TK_STRING_IDX: u32 = 6;
pub const TK_NUMBER_IDX: u32 = 7;

pub const TK_DEFINITION_BIT: u32 = 0;
pub const TK_READONLY_BIT: u32 = 1;
pub const HASH_SIZE: usize = 32;

pub struct ToFilePathErr;

#[derive(Clone, Debug, Hash, PartialOrd, PartialEq, Ord, Eq)]
pub struct Hash {
    data: [u8; HASH_SIZE]
}

impl Hash {
    pub fn new(v: &Vec<u8>) -> Self {
        let len = min(v.len(), HASH_SIZE);
        let mut data: [u8; HASH_SIZE] = [0; 32];
        for i in 0..len {
            data[i] = if i >= v.len() { 0 } else { v[i] }
        }
        Hash { data }
    }
}

// Note: to_file_path is only present on native builds, but we're building to
// wasm.
//
// The code isn't ungeneric, so we'll express it here.
pub trait HasFilePath {
    fn our_to_file_path(&self) -> Result<PathBuf, ToFilePathErr>;
}

fn file_url_segments_to_pathbuf(
    host: Option<Vec<u8>>,
    segments: std::str::Split<'_, char>,
) -> Result<PathBuf, ToFilePathErr> {
    if host.is_some() {
        return Err(ToFilePathErr);
    }

    let mut bytes = Vec::new();

    for segment in segments {
        bytes.push(b'/');
        bytes.extend(percent_decode(segment.as_bytes()));
    }

    // A windows drive letter must end with a slash.
    if bytes.len() > 2
        && matches!(bytes[bytes.len() - 2], b'a'..=b'z' | b'A'..=b'Z')
        && matches!(bytes[bytes.len() - 1], b':' | b'|')
    {
        bytes.push(b'/');
    }

    let path = PathBuf::from(decode_string(&bytes));

    debug_assert!(
        path.is_absolute(),
        "to_file_path() failed to produce an absolute Path"
    );

    Ok(path)
}

impl HasFilePath for Url {
    fn our_to_file_path(&self) -> Result<PathBuf, ToFilePathErr> {
        if let Some(segments) = self.path_segments() {
            let host = match self.host() {
                None | Some(Host::Domain("localhost")) => None,
                Some(h) if self.scheme() == "file" => Some(h.to_string().as_bytes().to_owned()),
                _ => return Err(ToFilePathErr),
            };

            return file_url_segments_to_pathbuf(host, segments);
        }
        Err(ToFilePathErr)
    }
}

#[cfg(test)]
fn uniterr<A>(_: A) -> () { () }

#[test]
fn test_file_segments_to_pathbuf_1() {
    assert_eq!(
        Url::parse("fink:::::not/good").map_err(uniterr).and_then(|uri| {
            uri.our_to_file_path().map_err(uniterr)
        }),
        Err(())
    );
}

#[test]
fn test_file_segments_to_pathbuf_2() {
    assert_eq!(
        Url::parse("file:///home/person/stuff.txt").map_err(uniterr).and_then(|uri| {
            uri.our_to_file_path().map_err(uniterr)
        }),
        PathBuf::from_str("/home/person/stuff.txt").map_err(uniterr)
    );
}

#[test]
fn test_file_segments_to_pathbuf_3() {
    assert_eq!(
        Url::parse("").map_err(uniterr).and_then(|uri| {
            uri.our_to_file_path().map_err(uniterr)
        }),
        Err(())
    );
}

#[test]
fn test_file_segments_to_pathbuf_4() {
    assert_eq!(
        Url::parse("file:").map_err(uniterr).and_then(|uri| {
            uri.our_to_file_path().map_err(uniterr)
        }),
        PathBuf::from_str("/").map_err(uniterr)
    );
}

#[test]
fn test_file_segments_to_pathbuf_5() {
    assert_eq!(
        Url::parse("file:///").map_err(uniterr).and_then(|uri| {
            uri.our_to_file_path().map_err(uniterr)
        }),
        PathBuf::from_str("/").map_err(uniterr)
    );
}

pub fn cast<R>(req: Request) -> Result<(RequestId, R::Params), ExtractError<Request>>
where
    R: lsp_types::request::Request,
    R::Params: serde::de::DeserializeOwned,
{
    req.extract(R::METHOD)
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
// DocPosition is 0-based
pub struct DocPosition {
    pub line: u32,
    pub character: u32,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct DocRange {
    pub start: DocPosition,
    pub end: DocPosition,
}

#[test]
fn test_docrange_overlap_no() {
    assert_eq!(
        DocRange {
            start: DocPosition { line: 2, character: 5 },
            end: DocPosition { line: 3, character: 4 },
        }.overlap(&DocRange {
            start: DocPosition { line: 1, character: 2 },
            end: DocPosition { line: 2, character: 3 }
        }),
        false
    );
}

#[test]
fn test_docrange_overlap_yes() {
    assert_eq!(
        DocRange {
            start: DocPosition { line: 2, character: 5 },
            end: DocPosition { line: 3, character: 4 },
        }.overlap(&DocRange {
            start: DocPosition { line: 3, character: 2 },
            end: DocPosition { line: 3, character: 8 }
        }),
        true
    );
}

#[test]
fn test_docrange_overlap_same_line_no() {
    assert_eq!(
        DocRange {
            start: DocPosition { line: 2, character: 5 },
            end: DocPosition { line: 2, character: 7 },
        }.overlap(&DocRange {
            start: DocPosition { line: 2, character: 1 },
            end: DocPosition { line: 2, character: 4 }
        }),
        false
    );
}

#[test]
fn test_docrange_overlap_same_line_yes() {
    assert_eq!(
        DocRange {
            start: DocPosition { line: 2, character: 5 },
            end: DocPosition { line: 2, character: 7 },
        }.overlap(&DocRange {
            start: DocPosition { line: 2, character: 1 },
            end: DocPosition { line: 2, character: 5 }
        }),
        true
    );
}

#[test]
fn test_invalid_zero_srcloc_leads_to_zero_position() {
    assert_eq!(
        DocRange::from_srcloc(Srcloc::new(Rc::new("file.txt".to_owned()), 0, 0)),
        DocRange {
            start: DocPosition { line: 0, character: 0 },
            end: DocPosition { line: 0, character: 0 }
        }
    );
}

#[test]
fn test_doc_range_overlap_at_zero() {
    assert!(DocRange {
        start: DocPosition { line: 0, character: 0 },
        end: DocPosition { line: 0, character: 2 }
    }.overlap(&DocRange {
        start: DocPosition { line: 0, character: 1 },
        end: DocPosition { line: 0, character: 3 }
    }));
}

#[derive(Clone, Debug)]
pub struct DocPatch {
    pub range: DocRange,
    pub text: String,
}

impl DocPosition {
    // This is not dead code, but due to the variation in build configuration
    // not all ways of building this module require it.
    // among test, rustlib and wasm32.
    #[allow(dead_code)]
    pub fn from_position(pos: &Position) -> Self {
        DocPosition {
            line: pos.line,
            character: pos.character,
        }

    }

    pub fn to_position(&self) -> Position {
        Position {
            line: self.line,
            character: self.character,
        }
    }
}

impl DocRange {
    // Not currently used, therefore causing a clippy violation if uncommented.
    // This can potentially be useful.
    /*
    pub fn from_range(r: &Range) -> Self {
        DocRange {
            start: DocPosition::from_position(&r.start),
            end: DocPosition::from_position(&r.end),
        }
    }
    */

    // DocPosition is 0 based.  Srcloc is 1 based.
    pub fn from_srcloc(l: Srcloc) -> Self {
        let e = l.ending();
        DocRange {
            start: DocPosition {
                line: if l.line > 0 { (l.line - 1) as u32 } else { 0 },
                character: if l.col > 0 { (l.col - 1) as u32 } else { 0 },
            },
            end: DocPosition {
                line: if e.line > 0 { (e.line - 1) as u32 } else { 0 },
                character: if e.col > 0 { (e.col - 1) as u32 } else { 0 },
            },
        }
    }

    pub fn to_srcloc(&self, file: &str) -> Srcloc {
        let file_rc = Rc::new(file.to_owned());
        Srcloc::new(
            file_rc.clone(),
            (self.start.line + 1) as usize,
            (self.start.character + 1) as usize,
        )
        .ext(&Srcloc::new(
            file_rc,
            (self.end.line + 1) as usize,
            (self.end.character + 1) as usize,
        ))
    }

    pub fn to_range(&self) -> Range {
        Range {
            start: self.start.to_position(),
            end: self.end.to_position(),
        }
    }
}

impl DocRange {
    // This is likewise used in some of the configurations, but not all.
    #[allow(dead_code)]
    pub fn overlap(&self, other: &DocRange) -> bool {
        let mut sortable = vec![
            (self.start.clone(), 0),
            (self.end.clone(), 0),
            (other.start.clone(), 1),
            (other.end.clone(), 1),
        ];
        sortable.sort();

        // Not overlapping if both points are on the same side of the other 2
        sortable[0].1 != sortable[1].1
    }
}

// An object that contains the literal text of a document we're working with in
// the LSP.
#[derive(Debug, Clone)]
pub struct DocData {
    pub fullname: String,
    pub text: Vec<Rc<Vec<u8>>>,
    pub version: i32,
    // Zero based.
    pub comments: HashMap<usize, usize>,
}

impl DocData {
    // Return a reference to the nth line's data.
    pub fn nth_line_ref(&self, line: usize) -> Option<&Vec<u8>> {
        if line < self.text.len() {
            let borrowed: &Vec<u8> = self.text[line].borrow();
            Some(borrowed)
        } else {
            None
        }
    }

    // Given a position go back one character, returning the character
    // and the new position if they exist.
    //
    // This only visits characters in lines, so it will skip blank lines.
    pub fn get_prev_position(&self, position: &Position) -> Option<(u8, Position)> {
        if position.character == 0
            && position.line > 0
            && ((position.line - 1) as usize) < self.text.len()
        {
            let nextline = position.line - 1;
            self.get_prev_position(&Position {
                line: nextline,
                character: self.text[nextline as usize].len() as u32,
            })
        } else {
            self.nth_line_ref(position.line as usize).and_then(|line| {
                if position.character > 0 && (position.character as usize) <= line.len() {
                    let prev_char = position.character - 1;
                    let the_char = line[prev_char as usize];
                    Some((
                        the_char,
                        Position {
                            line: position.line,
                            character: prev_char,
                        },
                    ))
                } else {
                    None
                }
            })
        }
    }

    // Given a position, get the pointed-to character.
    // Not currently used.
    /*
    pub fn get_at_position(&self, position: &Position) -> Option<u8> {
        self.nth_line_ref(position.line as usize).and_then(|line| {
            if (position.character as usize) < line.len() {
                Some(line[position.character as usize])
            } else {
                None
            }
        })
    }
    */
}

pub struct LSPServiceProvider {
    // Init params.
    pub fs: Rc<dyn IFileReader>,
    pub log: Rc<dyn ILogWriter>,
    pub init: Option<InitState>,
    pub workspace_root_override: Option<PathBuf>,
    pub config: ConfigJson,

    // Let document collection be sharable due to the need to capture it for
    // use in compiler opts.
    pub document_collection: Rc<RefCell<HashMap<String, DocData>>>,

    // These aren't shared.
    pub parsed_documents: HashMap<String, ParsedDoc>,
    pub goto_defs: HashMap<String, BTreeMap<SemanticTokenSortable, Srcloc>>,

    // Collection of all known errors we're throwing.
    pub thrown_errors: HashMap<String, ErrorSet>,

    // Prim list so we can tell if the first atom is a clvm atom.
    pub prims: Vec<Vec<u8>>,

    // Set of file extensions we should consider changing the world.
    pub workspace_file_extensions_to_resync_for: Vec<String>,
}

pub fn urlify(u: &str) -> String {
    if !u.starts_with("file://") {
        format!("file://{}", u)
    } else {
        u.to_owned()
    }
}

impl LSPServiceProvider {
    pub fn produce_error_list(&self) -> Vec<Message> {
        let mut all_errors = Vec::new();
        let tour_documents: Vec<String> = self.parsed_documents.keys().cloned().collect();

        for uristring in tour_documents.iter() {
            if let Some(err) = self.thrown_errors.get(uristring) {
                if !err.preprocessing.is_empty() {
                    all_errors.push(Message::Notification(Notification {
                        method: "textDocument/publishDiagnostics".to_string(),
                        params: serde_json::to_value(PublishDiagnosticsParams {
                            uri: Url::parse(uristring).unwrap(),
                            version: None,
                            diagnostics: err.preprocessing.clone(),
                        })
                            .unwrap(),
                    }));
                } else if !err.semantic.is_empty() {
                    all_errors.push(Message::Notification(Notification {
                        method: "textDocument/publishDiagnostics".to_string(),
                        params: serde_json::to_value(PublishDiagnosticsParams {
                            uri: Url::parse(uristring).unwrap(),
                            version: None,
                            diagnostics: err.semantic.clone(),
                        })
                            .unwrap(),
                    }));
                } else {
                    all_errors.push(Message::Notification(Notification {
                        method: "textDocument/publishDiagnostics".to_string(),
                        params: serde_json::to_value(PublishDiagnosticsParams {
                            uri: Url::parse(uristring).unwrap(),
                            version: None,
                            diagnostics: vec![]
                        })
                            .unwrap(),
                    }));
                }
            }
        }
        all_errors
    }

    // Set errors of a given kind for the indicated source file.
    pub fn set_error_list(&mut self, uristring: &str, preprocessing: bool, errors: Vec<Diagnostic>) {
        if preprocessing {
            // Remove semantic errors if errors is not empty, otherwise, clear
            // preprocessing errors.
            if let Some(eset) = self.thrown_errors.get_mut(uristring) {
                if !errors.is_empty() {
                    eset.semantic.clear();
                }

                eset.preprocessing = errors;
            } else {
                // no previous entry, so just set it.
                self.thrown_errors.insert(
                    uristring.to_owned(),
                    ErrorSet::from_preprocessing(errors)
                );
            }
        } else {
            if let Some(eset) = self.thrown_errors.get_mut(uristring) {
                eset.semantic = errors;
            } else {
                self.thrown_errors.insert(
                    uristring.to_owned(),
                    ErrorSet::from_semantic(errors)
                );
            }
        }
    }

    // For a given document refresh preprocessing errors.
    pub fn parse_document_and_store_errors(&mut self, uristring: &str) {
        self.ensure_parsed_document(uristring);

        let missing_includes = self.check_for_missing_include_files(uristring);
        let errors = missing_includes.iter().map(|i| {
            Diagnostic {
                range: DocRange::from_srcloc(i.name_loc.clone()).to_range(),
                severity: None,
                code: None,
                code_description: None,
                source: Some("chialisp".to_string()),
                message: format!("missing include file {} or path not set", decode_string(&i.filename)),
                tags: None,
                related_information: None,
                data: None,
            }
        }).collect();

        self.set_error_list(uristring, true, errors);
    }

    pub fn with_doc_and_parsed<F, G>(&mut self, uristring: &str, f: F) -> Option<G>
    where
        F: FnOnce(&DocData, &ParsedDoc) -> Option<G>,
    {
        if let (Some(d), Some(p)) = (self.get_doc(uristring), self.get_parsed(uristring)) {
            f(&d, &p)
        } else {
            None
        }
    }

    pub fn get_doc(&self, uristring: &str) -> Option<DocData> {
        let cell: &RefCell<HashMap<String, DocData>> = self.document_collection.borrow();
        let coll: Ref<HashMap<String, DocData>> = cell.borrow();
        coll.get(uristring).cloned()
    }

    pub fn get_doc_keys(&self) -> Vec<String> {
        let cell: &RefCell<HashMap<String, DocData>> = self.document_collection.borrow();
        let coll: Ref<HashMap<String, DocData>> = cell.borrow();
        coll.keys().cloned().collect()
    }

    pub fn get_parsed(&self, uristring: &str) -> Option<ParsedDoc> {
        self.parsed_documents.get(uristring).cloned()
    }

    pub fn save_doc(&mut self, uristring: String, dd: DocData) {
        let cell: &RefCell<HashMap<String, DocData>> = self.document_collection.borrow();
        self.parsed_documents.remove(&uristring);
        cell.replace_with(|coll| {
            let mut repl = HashMap::new();
            swap(&mut repl, coll);
            repl.insert(uristring.clone(), dd);
            repl
        });
    }

    fn save_parse(&mut self, uristring: String, p: ParsedDoc) {
        self.parsed_documents.insert(uristring, p);
    }

    pub fn ensure_parsed_document(&mut self, uristring: &str) {
        let opts = Rc::new(LSPCompilerOpts::new(
            self.log.clone(),
            self.fs.clone(),
            self.get_workspace_root(),
            uristring,
            &self.config.include_paths,
            self.document_collection.clone(),
        )).set_frontend_check_live(false);

        if let Some(doc) = self.get_doc(uristring) {
            let startloc = Srcloc::start(uristring);
            let output = self
                .parsed_documents
                .get(uristring)
                .cloned()
                .unwrap_or_else(|| ParsedDoc::new(startloc));
            let ranges = make_simple_ranges(&doc.text);
            let mut new_helpers = reparse_subset(
                &self.prims,
                opts,
                &doc.text,
                uristring,
                &ranges,
                &output.compiled,
                &output.helpers,
            );

            for (_, incfile) in new_helpers.includes.iter() {
                if incfile.kind != IncludeKind::Include
                    || incfile.filename == b"*standard-cl-21*"
                    || incfile.filename == b"*standard-cl-22*"
                {
                    continue;
                }

                if let Ok((filename, file_body)) = get_file_content(
                    self.log.clone(),
                    self.fs.clone(),
                    self.get_workspace_root(),
                    &self.config.include_paths,
                    &decode_string(&incfile.filename),
                ) {
                    if let Some(file_uri) = self
                        .get_workspace_root()
                        .and_then(|r| r.join(filename).to_str().map(urlify))
                    {
                        self.save_doc(file_uri.clone(), file_body);
                        if let Some(p) = self.get_parsed(&file_uri) {
                            for (hash, helper) in p.helpers.iter() {
                                new_helpers.helpers.insert(hash.clone(), helper.clone());
                            }
                        }
                    }
                }
            }

            let new_parse =
                combine_new_with_old_parse(uristring, &doc.text, &output, &new_helpers);
            self.set_error_list(uristring, false, new_parse.errors.iter().map(|error| {
                Diagnostic {
                    range: DocRange::from_srcloc(error.0.clone()).to_range(),
                    severity: None,
                    code: None,
                    code_description: None,
                    source: Some("chialisp".to_string()),
                    message: error.1.clone(),
                    tags: None,
                    related_information: None,
                    data: None,
                }
            }).collect());

            self.save_parse(
                uristring.to_owned(),
                new_parse,
            );
        }
    }

    pub fn get_capabilities() -> ServerCapabilities {
        ServerCapabilities {
            // Specify capabilities from the set:
            // https://docs.rs/lsp-types/latest/lsp_types/struct.ServerCapabilities.html
            definition_provider: Some(OneOf::Left(true)),
            semantic_tokens_provider: Some(
                SemanticTokensServerCapabilities::SemanticTokensOptions(SemanticTokensOptions {
                    work_done_progress_options: WorkDoneProgressOptions {
                        work_done_progress: Some(false),
                    },
                    legend: SemanticTokensLegend {
                        token_types: TOKEN_TYPES.clone(),
                        token_modifiers: TOKEN_MODIFIERS.clone(),
                    },
                    range: None,
                    full: Some(SemanticTokensFullOptions::Delta { delta: Some(true) }),
                }),
            ),
            text_document_sync: Some(TextDocumentSyncCapability::Kind(
                TextDocumentSyncKind::INCREMENTAL,
            )),
            completion_provider: Some(CompletionOptions {
                resolve_provider: Some(true),
                //             trigger_characters: Some(completion_start),
                ..Default::default()
            }),
            code_action_provider: Some(CodeActionProviderCapability::Options(CodeActionOptions {
                code_action_kinds: Some(vec![
                    CodeActionKind::QUICKFIX
                ]),
                work_done_progress_options: WorkDoneProgressOptions {
                    work_done_progress: None
                },
                resolve_provider: None
            })),
            ..Default::default()
        }
    }

    pub fn new(fs: Rc<dyn IFileReader>, log: Rc<dyn ILogWriter>, configured: bool) -> Self {
        let clvm_prims = prims().iter().map(|(p,_)| p.clone()).collect();

        LSPServiceProvider {
            fs,
            log,
            init: if configured {
                Some(InitState::Preconfig)
            } else {
                None
            },
            workspace_root_override: None,
            config: Default::default(),

            document_collection: Rc::new(RefCell::new(HashMap::new())),

            parsed_documents: HashMap::new(),
            goto_defs: HashMap::new(),
            thrown_errors: HashMap::new(),

            workspace_file_extensions_to_resync_for: vec![
                ".clsp",
                ".cl",
                ".clvm",
                ".clib",
                ".clinc"
            ].iter().map(|s| s.to_string()).collect(),
            prims: clvm_prims
        }
    }

    #[cfg(test)]
    pub fn set_workspace_root(&mut self, root: PathBuf) {
        self.workspace_root_override = Some(root);
    }

    #[cfg(test)]
    pub fn set_config(&mut self, cfg: ConfigJson) {
        self.config = cfg;
    }

    pub fn get_workspace_root(&self) -> Option<PathBuf> {
        self.workspace_root_override
            .as_ref()
            .map(|x| Some(x.clone()))
            .unwrap_or_else(|| {
                if let Some(InitState::Initialized(init)) = &self.init {
                    init.root_uri
                        .as_ref()
                        .and_then(|uri| Url::parse(uri.as_str()).ok())
                        .and_then(|uri| uri.our_to_file_path().ok())
                } else {
                    None
                }
            })
    }

    pub fn get_relative_path(&self, target: &str) -> Option<String> {
        if let Some(r) = self.get_workspace_root() {
            if target == "." {
                return Path::new(&r).to_str().map(|o| o.to_owned());
            } else if target.len() < 2 {
                return Path::new(&r).join(target).to_str().map(|o| o.to_owned());
            }

            let target_suffix: Vec<u8> = target.as_bytes().iter().skip(2).copied().collect();
            Path::new(&r)
                .join(decode_string(&target_suffix))
                .to_str()
                .map(|o| o.to_owned())
        } else {
            None
        }
    }

    pub fn get_config_path(&self) -> Option<String> {
        self.get_workspace_root().and_then(|r| {
            let p = Path::new(&r).join("chialisp.json");
            p.to_str().map(|s| s.to_owned())
        })
    }

    // Used, but not in all configurations.
    #[allow(dead_code)]
    pub fn get_file(&self, filename: &str) -> Result<String, String> {
        self.get_doc(filename)
            .map(|d| stringify_doc(&d.text))
            .unwrap_or_else(|| Err(format!("don't have file {}", filename)))
    }
}

// Note: This is using a directive that ensures that this code is only included
// in the test build.  It is not necessary to be concerned that it will be
// included in another configuration.  Although it's name contains test, it will
// not be used as a test in this language.
#[cfg(test)]
fn make_test_doc_data_object_for_the_subsequent_test_code_1() -> DocData {
    // There is a comment at line 2, column 24 and at line 3, column 8
    let comment_hashmap = HashMap::from([
        (1, 23),
        (2, 7)
    ]);
    // vr vec Rc
    let vr = |s: &str| {
        let bv: Vec<u8> = s.as_bytes().to_vec();
        Rc::new(bv)
    };
    DocData {
        // Note: this structure contains owned objects such as strings and vecs.
        // The ownership is intended.
        fullname: "test_name.clsp".to_string(),
        // Note: This vec contains reference counted lines so patches can move
        // them without cloning them.  The Rc here, as well as the string
        // ownership are intended.
        text: vec![
            vr("(mod (X)"),
            vr(" (defun F (A) (+ A 1)) ;; A function"),
            vr(" (F X) ;; Call"),
            vr(" )")
        ],
        version: 1,
        comments: comment_hashmap
    }
}

// Note: This is a test in this language.  Readers find non-segregated tests an
// easier read so tests are inline.
#[test]
fn test_doc_data_nth_line_ref_1() {
    let dd = make_test_doc_data_object_for_the_subsequent_test_code_1();
    assert_eq!(dd.nth_line_ref(2), Some(&" (F X) ;; Call".as_bytes().to_vec()));
}

#[test]
fn test_doc_data_nth_line_ref_2() {
    let dd = make_test_doc_data_object_for_the_subsequent_test_code_1();
    assert_eq!(dd.nth_line_ref(5), None);
}

#[test]
fn test_doc_data_get_prev_position_1() {
    let dd = make_test_doc_data_object_for_the_subsequent_test_code_1();
    // Zero based.
    let mut position = Some(Position { line: 3, character: 2 });
    let mut all_expected_characters = Vec::new();
    let mut got_characters = Vec::new();
    let mut have_line_jumps = Vec::new();
    let want_line_jumps = vec![2, 16, 52];

    for l in dd.text.iter().rev() {
        let borrowed_line: &Vec<u8> = l.borrow();
        let mut reversed_line = borrowed_line.iter().rev().copied().collect();
        all_expected_characters.append(&mut reversed_line);
    }

    while let Some(p) = &position {
        if let Some((ch, new_position)) = dd.get_prev_position(p) {
            if new_position.line != p.line {
                have_line_jumps.push(got_characters.len());
            }
            position = Some(new_position);
            got_characters.push(ch);
        } else {
            position = None;
        }
    }

    assert_eq!(want_line_jumps, have_line_jumps);
    assert_eq!(got_characters, all_expected_characters);
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord)]
struct HelperWithDocRange {
    pub loc: DocRange,
}

#[test]
fn test_helper_with_doc_range() {
    let sl1 = DocRange {
        start: DocPosition { line: 0, character: 1 },
        end: DocPosition { line: 0, character: 2 },
    };
    let sl2 = DocRange {
        start: DocPosition { line: 0, character: 2 },
        end: DocPosition { line: 0, character: 4 },
    };
    let hw1 = HelperWithDocRange { loc: sl1 };
    let hw2 = HelperWithDocRange { loc: sl2 };
    assert_eq!(hw1, hw1);
    assert!(hw1 != hw2);
    assert!(hw1 < hw2);
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct ConfigJson {
    pub include_paths: Vec<String>,
}

pub enum InitState {
    Preconfig,
    Initialized(Rc<InitializeParams>),
}

#[derive(Default)]
pub struct ErrorSet {
    preprocessing: Vec<Diagnostic>,
    semantic: Vec<Diagnostic>,
}

impl ErrorSet {
    pub fn from_preprocessing(v: Vec<Diagnostic>) -> Self {
        ErrorSet { preprocessing: v, semantic: vec![] }
    }

    pub fn from_semantic(v: Vec<Diagnostic>) -> Self {
        ErrorSet { preprocessing: vec![], semantic: v }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
// What kind of scope form led to this binding.
pub enum ScopeKind {
    Module,
    Macro,
    Function,
    Let,
}

#[derive(Debug, Clone, PartialEq, Eq)]
// Various ways chialisp can include a file (compile and embed pending in
// https://github.com/Chia-Network/clvm_tools_rs/pull/71 )
pub enum IncludeKind {
    Include,
    CompileFile(Srcloc),
    EmbedFile(Srcloc, Srcloc),
}

#[derive(Debug, Clone)]
// Part of a scope stack showing all bindings available in the given region.
pub struct ParseScope {
    pub region: Srcloc,
    pub kind: ScopeKind,
    pub variables: HashSet<SExp>,
    pub functions: HashSet<SExp>,
    pub containing: Vec<ParseScope>,
}

#[derive(Debug, Clone)]
// Information about how we produce semantic tokens for include directives.
// Found is None if we haven't tried to resolve this include yet, Some(true)
// if we found it, and Some(false) if we tried and it didn't exist.
pub struct IncludeData {
    pub loc: Srcloc,
    pub name_loc: Srcloc,
    pub kw_loc: Srcloc,
    pub kind: IncludeKind,
    pub filename: Vec<u8>,
    pub found: Option<bool>
}

#[derive(Debug, Clone)]
// A helper (defmacro, defun, etc)  that we parsed alone via document range.
pub struct ReparsedHelper {
    pub hash: Hash,
    pub range: DocRange,
    pub parsed: Result<HelperForm, CompileErr>,
}

#[derive(Debug, Clone)]
// Information about the main expression in a chialisp program, parsed from a
// document range.
pub struct ReparsedExp {
    pub hash: Hash,
    pub parsed: Result<BodyForm, CompileErr>,
}
