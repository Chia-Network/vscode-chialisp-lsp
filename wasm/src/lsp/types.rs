use std::borrow::Borrow;
use std::cell::{Ref, RefCell};
use std::cmp::{Ordering, PartialOrd};
use std::collections::{BTreeMap, HashMap};
use std::default::Default;
use std::mem::swap;
use std::path::{Path, PathBuf};
use std::rc::Rc;

use lsp_server::{ExtractError, Message, Notification, Request, RequestId};

use lsp_types::{
    CodeActionKind, CodeActionProviderCapability, CodeActionOptions, CompletionOptions, Diagnostic, ExecuteCommandParams, InitializeParams, OneOf, Position, PublishDiagnosticsParams,
    Range, SemanticTokenModifier, SemanticTokenType, SemanticTokensFullOptions,
    SemanticTokensLegend, SemanticTokensOptions, SemanticTokensServerCapabilities,
    ServerCapabilities, TextDocumentSyncCapability, TextDocumentSyncKind, WorkDoneProgressOptions, WorkDoneProgressParams
};

use percent_encoding::percent_decode;
use serde::{Deserialize, Serialize};
use url::{Host, Url};

use crate::lsp::compopts::{get_file_content, LSPCompilerOpts};
use crate::lsp::parse::{make_simple_ranges, IncludeKind, ParsedDoc};
use crate::lsp::patch::stringify_doc;
use crate::lsp::reparse::{combine_new_with_old_parse, reparse_subset};
use crate::lsp::semtok::SemanticTokenSortable;
use clvm_tools_rs::compiler::comptypes::CompilerOpts;
use clvm_tools_rs::compiler::prims::prims;
use clvm_tools_rs::compiler::sexp::decode_string;
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

pub struct ToFilePathErr;

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

pub trait IFileReader {
    fn read(&self, name: &str) -> Result<Vec<u8>, String>;
}

pub trait ILogWriter {
    fn write(&self, text: &str);
}

#[derive(Default)]
pub struct FSFileReader {}

impl IFileReader for FSFileReader {
    fn read(&self, name: &str) -> Result<Vec<u8>, String> {
        std::fs::read(name).map_err(|e| format!("{:?}", e))
    }
}

impl FSFileReader {
    #[cfg(test)]
    pub fn new() -> Self {
        Default::default()
    }
}

#[derive(Default)]
pub struct EPrintWriter {}

impl ILogWriter for EPrintWriter {
    fn write(&self, text: &str) {
        eprintln!("{}", text);
    }
}

impl EPrintWriter {
    #[cfg(test)]
    pub fn new() -> Self {
        Default::default()
    }
}

pub fn cast<R>(req: Request) -> Result<(RequestId, R::Params), ExtractError<Request>>
where
    R: lsp_types::request::Request,
    R::Params: serde::de::DeserializeOwned,
{
    req.extract(R::METHOD)
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct DocPosition {
    pub line: u32,
    pub character: u32,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct DocRange {
    pub start: DocPosition,
    pub end: DocPosition,
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
    // Not currently used
    /*
    pub fn from_range(r: &Range) -> Self {
        DocRange {
            start: DocPosition::from_position(&r.start),
            end: DocPosition::from_position(&r.end),
        }
    }
    */

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

#[derive(Debug, Clone)]
pub struct DocData {
    pub fullname: String,
    pub text: Vec<Rc<Vec<u8>>>,
    pub version: i32,
    pub comments: HashMap<usize, usize>,
}

impl DocData {
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

#[derive(Debug, Clone)]
struct HelperWithDocRange {
    pub loc: DocRange,
}

impl PartialEq for HelperWithDocRange {
    fn eq(&self, other: &Self) -> bool {
        self.loc == other.loc
    }
}

impl PartialOrd for HelperWithDocRange {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.loc.cmp(&other.loc))
    }
}

impl Eq for HelperWithDocRange {}
impl Ord for HelperWithDocRange {
    fn cmp(&self, other: &Self) -> Ordering {
        self.loc.cmp(&other.loc)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct ConfigJson {
    pub include_paths: Vec<String>,
}

pub enum InitState {
    Preconfig,
    Initialized(Rc<InitializeParams>),
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

    // Prim list so we can tell if the first atom is a clvm atom.
    pub prims: Vec<Vec<u8>>,

    // Set of file extensions we should consider changing the world.
    pub workspace_file_extensions_to_resync_for: Vec<String>,
}

impl LSPServiceProvider {
    pub fn parse_document_and_output_errors(&mut self, uristring: &str) -> Vec<Message> {
        let mut res = Vec::new();

        self.ensure_parsed_document(uristring);
        if let (Some(d), Some(p)) = (self.get_doc(uristring), self.get_parsed(uristring)) {
            let mut errors = Vec::new();

            let missing_includes = self.check_for_missing_include_files(uristring);
            let mut include_diagnostics: Vec<Diagnostic> = missing_includes.iter().map(|i| {
                Diagnostic {
                    range: DocRange::from_srcloc(i.nl.clone()).to_range(),
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

            for mi in missing_includes.iter() {
                // Send command to show the status bar item.
                res.push(Message::Notification(Notification {
                    method: "workspace/executeCommand".to_string(),
                    params: serde_json::to_value(ExecuteCommandParams {
                        command: "chialisp.locateIncludeFile".to_string(),
                        arguments: vec![serde_json::to_value(decode_string(&mi.filename)).unwrap()],
                        work_done_progress_params: WorkDoneProgressParams {
                            work_done_token: None
                        }
                    }).unwrap()
                }));
            }

            errors.append(&mut include_diagnostics);

            for error in p.errors.iter() {
                errors.push(Diagnostic {
                    range: DocRange::from_srcloc(error.0.clone()).to_range(),
                    severity: None,
                    code: None,
                    code_description: None,
                    source: Some("chialisp".to_string()),
                    message: error.1.clone(),
                    tags: None,
                    related_information: None,
                    data: None,
                });
            }

            if !errors.is_empty() || d.version > 1 {
                res.push(Message::Notification(Notification {
                    method: "textDocument/publishDiagnostics".to_string(),
                    params: serde_json::to_value(PublishDiagnosticsParams {
                        uri: Url::parse(uristring).unwrap(),
                        version: None,
                        diagnostics: errors,
                    })
                    .unwrap(),
                }));
            }
        }

        res
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
                        .and_then(|r| r.join(filename).to_str().map(|f| f.to_owned()))
                    {
                        self.save_doc(file_uri.clone(), file_body);
                        self.ensure_parsed_document(&file_uri);
                        if let Some(p) = self.get_parsed(&file_uri) {
                            for (hash, helper) in p.helpers.iter() {
                                new_helpers.helpers.insert(hash.clone(), helper.clone());
                            }
                        }
                    }
                }
            }

            self.save_parse(
                uristring.to_owned(),
                combine_new_with_old_parse(uristring, &doc.text, &output, &new_helpers),
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
