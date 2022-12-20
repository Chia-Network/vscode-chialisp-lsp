use std::borrow::Borrow;
use std::cmp::PartialOrd;
use std::collections::HashMap;
use std::path::PathBuf;
use std::str::FromStr;
use std::rc::Rc;

use lsp_server::{ExtractError, Request, RequestId};

use lsp_types::{
    Position,
    Range, SemanticTokenModifier, SemanticTokenType
};

use percent_encoding::percent_decode;
use url::{Host, Url};

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
}

// Note: This is using a directive that ensures that this code is only included
// in the test build.  It is not necessary to be concerned that it will be
// included in another configuration.  Although it's name contains test, it will
// not be used as a test in this language.
#[cfg(test)]
fn make_test_doc_data_object_for_the_subsequent_test_code_1() -> DocData {
    let mut comment_hashmap = HashMap::new();
    // There is a comment at line 2, column 24 and at line 3, column 8
    comment_hashmap.insert(1, 23);
    comment_hashmap.insert(2, 7);
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
                eprintln!("jump from {:?} to {:?} char {}", p, new_position, ch as char);
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
