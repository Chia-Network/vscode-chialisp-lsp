use std::borrow::Borrow;
use std::collections::{BTreeMap, HashMap};
use std::path::PathBuf;
use std::rc::Rc;

use debug_types::types::SourceBreakpoint;

use clvm_tools_rs::compiler::comptypes::CompileForm;
use clvm_tools_rs::compiler::sexp::{decode_string, SExp};
use clvm_tools_rs::compiler::srcloc::Srcloc;

use crate::interfaces::ILogWriter;
use crate::lsp::types::{DocPosition, DocRange};

const LARGE_COLUMN: u32 = 100000;

#[derive(Clone, Debug)]
pub enum SrclocParseAction {
    ReadingFileName,
    ReadingLineNumber(usize, usize),
    AfterLineNumber(usize, usize),
    ReadingColumn(usize, usize, usize, Option<usize>),
}

struct ParsedSrclocPart {
    file: Vec<u8>,
    line: usize,
    col: usize,
    ext: Option<usize>,
}

/// A structure containing what we know about the code as we passed the entry into
/// this scope.  Since CLVM doesn't model the stack in the same way as a
/// traditional vm, we synthesize a stack as we go.  cldh_hierarchy will tell us
/// that the shape of the stack changed, and we'll push and pop frames as needed
/// to match.  This contains our understanding of the state of execution when
/// each frame was pushed.
#[derive(Clone, Debug)]
pub struct StoredScope {
    pub scope_id: u32,

    pub name: String,
    pub named_args: HashMap<String, Rc<SExp>>,

    pub rundata: Option<BTreeMap<String, String>>,

    pub source: Srcloc,
}

/// Given symbols, possibly a CompileForm and a SourceBreakpoint, try a few tricks
/// to figure out where we should stop in the source when a breakpoint is set.
///
/// This can certainly be improved.
pub fn find_location(
    symbols: Rc<HashMap<String, String>>,
    compiled: &Option<CompileForm>,
    log: Rc<dyn ILogWriter>,
    file: &str,
    b: &SourceBreakpoint,
) -> Option<(String, Srcloc)> {
    let whole_line = DocRange {
        start: DocPosition {
            line: b.line - 1,
            character: 0,
        },
        end: DocPosition {
            line: b.line - 1,
            character: LARGE_COLUMN,
        },
    };
    let breakpoint_range = b
        .column
        .map(|_col| DocRange {
            start: DocPosition {
                line: b.line - 1,
                character: 0,
            },
            end: DocPosition {
                line: b.line - 1,
                character: LARGE_COLUMN,
            },
        })
        .unwrap_or_else(|| whole_line.clone());

    let breakpoint_loc = breakpoint_range.to_srcloc(file);
    for (k, v) in symbols.iter() {
        if let Some(parsed_srcloc) = parse_srcloc(v) {
            let borrowed_filename: &String = parsed_srcloc.file.borrow();
            if !fuzzy_file_match(file, borrowed_filename) {
                continue;
            }
            let normalized_loc = Srcloc::new(
                breakpoint_loc.file.clone(),
                parsed_srcloc.line,
                parsed_srcloc.col,
            );
            if normalized_loc.overlap(&breakpoint_loc) {
                return Some((k.clone(), parsed_srcloc.clone()));
            }
        }
    }

    let whole_line_loc = whole_line.to_srcloc(file);
    compiled.as_ref().and_then(|c| {
        for h in c.helpers.iter() {
            let original_loc = h.loc();
            let original_loc_file: &String = original_loc.file.borrow();
            if !fuzzy_file_match(original_loc_file, file) {
                continue;
            }
            let normalized_loc = Srcloc::new(
                whole_line_loc.file.clone(),
                original_loc.line,
                original_loc.col,
            );
            log.log(&format!("{normalized_loc} vs target loc {whole_line_loc}"));
            if whole_line_loc.overlap(&normalized_loc) {
                log.log(&format!("found function {}", decode_string(h.name())));
                return resolve_function(symbols, &decode_string(h.name()))
                    .map(|funhash| (funhash, original_loc));
            }
        }

        None
    })
}

/// A simple parser for srcloc for recovering them from messages and symbols.
pub fn parse_srcloc(s: &str) -> Option<Srcloc> {
    let parse_one_loc = |skip| {
        let mut parse_state = SrclocParseAction::ReadingFileName;
        for (i, ch) in s.as_bytes().iter().skip(skip).copied().enumerate() {
            match (&parse_state, ch) {
                (SrclocParseAction::ReadingFileName, b'(') => {
                    parse_state = SrclocParseAction::ReadingLineNumber(i, 0);
                }
                (SrclocParseAction::ReadingFileName, _) => {}
                (SrclocParseAction::ReadingLineNumber(eof, l), b')') => {
                    parse_state = SrclocParseAction::AfterLineNumber(*eof, *l);
                }
                (SrclocParseAction::ReadingLineNumber(eof, l), ch) => {
                    if !ch.is_ascii_digit() {
                        return None;
                    }
                    parse_state = SrclocParseAction::ReadingLineNumber(
                        *eof,
                        10 * *l + ((ch - b'0') as usize),
                    );
                }
                (SrclocParseAction::AfterLineNumber(eof, l), b':') => {
                    parse_state = SrclocParseAction::ReadingColumn(*eof, *l, 0, None);
                }
                (SrclocParseAction::AfterLineNumber(_, _), _) => {
                    return None;
                }
                (SrclocParseAction::ReadingColumn(eof, l, c, _), b'-') => {
                    parse_state = SrclocParseAction::ReadingColumn(*eof, *l, *c, Some(i + 1));
                }
                (SrclocParseAction::ReadingColumn(eof, l, c, e), ch) => {
                    if !ch.is_ascii_digit() {
                        return None;
                    }
                    parse_state = SrclocParseAction::ReadingColumn(
                        *eof,
                        *l,
                        10 * *c + ((ch - b'0') as usize),
                        *e,
                    );
                }
            }
        }

        if let SrclocParseAction::ReadingColumn(f, line, col, ext) = parse_state {
            Some(ParsedSrclocPart {
                file: s.as_bytes().iter().copied().take(f).collect(),
                line,
                col,
                ext,
            })
        } else {
            None
        }
    };

    parse_one_loc(0).and_then(|parsed| {
        let filename_rc = Rc::new(decode_string(&parsed.file));
        let loc = Srcloc::new(filename_rc.clone(), parsed.line, parsed.col);
        if let Some(ext) = parsed.ext {
            parse_one_loc(ext).map(|second| {
                if second.file != parsed.file {
                    // Incomplete range, treat the head as a marker.
                    return loc;
                }
                loc.ext(&Srcloc::new(filename_rc, second.line, second.col))
            })
        } else {
            Some(loc)
        }
    })
}

fn fuzzy_file_match(location_filename: &str, want_filename: &str) -> bool {
    if let (Some(location_fn), Some(want_fn)) = (
        PathBuf::from(location_filename).file_name(),
        PathBuf::from(want_filename).file_name(),
    ) {
        location_fn == want_fn
    } else {
        false
    }
}

#[test]
fn test_fuzzy_file_match_1() {
    assert!(fuzzy_file_match("test/foo/bar.clsp", "bar/baz/bar.clsp"));
}

fn resolve_function(symbols: Rc<HashMap<String, String>>, name: &str) -> Option<String> {
    for (k, v) in symbols.iter() {
        if v == name {
            return Some(k.clone());
        }
    }

    None
}

#[test]
fn test_resolve_function_1() {
    let symbols_map = HashMap::from([(
        "de3687023fa0a095d65396f59415a859dd46fc84ed00504bf4c9724fca08c9de".to_string(),
        "fact".to_string(),
    )]);
    let symbols = Rc::new(symbols_map);
    assert_eq!(
        resolve_function(symbols, "fact"),
        Some("de3687023fa0a095d65396f59415a859dd46fc84ed00504bf4c9724fca08c9de".to_string())
    );
}
