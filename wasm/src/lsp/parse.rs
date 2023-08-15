use std::borrow::Borrow;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use lsp_types::Position;

#[cfg(test)]
use clvm_tools_rs::compiler::compiler::DefaultCompilerOpts;
use clvm_tools_rs::compiler::comptypes::{
    BodyForm, CompileErr, CompileForm, HelperForm, LetData, LetFormKind,
};
#[cfg(test)]
use clvm_tools_rs::compiler::frontend::frontend;
use clvm_tools_rs::compiler::sexp::SExp;
#[cfg(test)]
use clvm_tools_rs::compiler::sexp::{decode_string, parse_sexp};
use clvm_tools_rs::compiler::srcloc::Srcloc;

use crate::lsp::types::{
    DocData, DocPosition, DocRange, Hash, IncludeData, ParseScope, ReparsedExp, ReparsedHelper,
    ScopeKind,
};

#[derive(Debug, Clone)]
// A parsed document.
pub struct ParsedDoc {
    // Ignored means that it didn't look like chialisp language, possibly it's
    // clvm or another sexp language that is semi-parsable as chialisp.
    pub ignored: bool,
    pub mod_kw: Option<Srcloc>,
    // If we were able to run a frontend pass (even partially), compiled contains
    // it.  CompileForm is the result of frontend and is used for analyzing
    // chialisp in many different tools deriving from clvm_tools_rs.
    pub compiled: CompileForm,
    // The scope stack for this file.
    pub scopes: ParseScope,
    // Helpers in ReparsedHelper form.  We pulled these indiviually by identifying
    // their ranges in the source.
    pub helpers: HashMap<Hash, ReparsedHelper>,
    // If present, the main expression in ReparsedExp form.
    pub exp: Option<ReparsedExp>,
    // Includes in the various files, indexed by included file.
    pub includes: HashMap<Hash, IncludeData>,
    // Index of hashed ranges (as Reparsed* data) to the names they bind.
    pub hash_to_name: HashMap<Hash, Vec<u8>>,
    // Chialisp frontend errors encountered while parsing.
    pub errors: Vec<CompileErr>,
}

impl ParsedDoc {
    pub fn new(startloc: Srcloc) -> Self {
        let nil = SExp::Nil(startloc.clone());
        ParsedDoc {
            ignored: false,
            mod_kw: None,
            // CompileForm doesn't have a Default impl, but we start from the
            // assumption of an empty program here so we can build it up
            // incrementally.
            compiled: CompileForm {
                loc: startloc.clone(),
                include_forms: Default::default(),
                args: Rc::new(nil.clone()),
                helpers: Default::default(),
                exp: Rc::new(BodyForm::Quoted(nil)),
            },
            scopes: ParseScope {
                region: startloc,
                kind: ScopeKind::Module,
                variables: Default::default(),
                functions: Default::default(),
                containing: Default::default(),
            },
            helpers: Default::default(),
            exp: None,
            includes: Default::default(),
            hash_to_name: Default::default(),
            errors: Default::default(),
        }
    }
}

pub struct DocVecByteIter<'a> {
    line: usize,
    offs: usize,
    target: &'a [Rc<Vec<u8>>],
}

impl<'a> Iterator for DocVecByteIter<'a> {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        if self.line >= self.target.len() {
            None
        } else if self.offs >= self.target[self.line].len() {
            self.line += 1;
            self.offs = 0;
            Some(b'\n')
        } else {
            let res = self.target[self.line][self.offs];
            self.offs += 1;
            Some(res)
        }
    }
}

impl<'a> DocVecByteIter<'a> {
    pub fn new(target: &'a [Rc<Vec<u8>>]) -> Self {
        DocVecByteIter {
            line: 0,
            offs: 0,
            target,
        }
    }
}

pub fn recover_scopes(ourfile: &str, text: &[Rc<Vec<u8>>], fe: &CompileForm) -> ParseScope {
    let mut toplevel_args = HashSet::new();
    let mut toplevel_funs = HashSet::new();
    let mut contained = Vec::new();

    make_arg_set(&mut toplevel_args, fe.args.clone());

    for h in fe.helpers.iter() {
        match h {
            HelperForm::Defun(_, d) => {
                toplevel_funs.insert(SExp::Atom(d.loc.clone(), d.name.clone()));
            }
            HelperForm::Defmacro(m) => {
                toplevel_funs.insert(SExp::Atom(m.loc.clone(), m.name.clone()));
            }
            HelperForm::Defconstant(c) => {
                toplevel_args.insert(SExp::Atom(c.loc.clone(), c.name.clone()));
            }
        }

        let f = h.loc().file.clone();
        let filename: &String = f.borrow();
        if filename == ourfile {
            if let Some(scope) = make_helper_scope(h) {
                contained.push(scope);
            }
        }
    }

    ParseScope {
        kind: ScopeKind::Module,
        region: Srcloc::start(ourfile).ext(&Srcloc::new(
            Rc::new(ourfile.to_string()),
            text.len() + 1,
            0,
        )),
        variables: toplevel_args,
        functions: toplevel_funs,
        containing: contained,
    }
}

// Not a "test", but a function used in tests.  Tests in this language use #[test]
// to designate tests.  #[cfg(test)] below designates this function as one compiled
// in test configuration, so if one was concerned that it'd be present in the final
// artifact, it won't.  Functions can have "test" in their names and not be test
// entrypoints.
//
// Note: for readers, I put a few such notes in each commit that uses these
// features because they have been frequently asked questions in the past.
#[cfg(test)]
fn vec_ref(s: &str) -> Rc<Vec<u8>> {
    let bv: Vec<u8> = s.as_bytes().to_vec();
    Rc::new(bv)
}

#[test]
fn test_doc_vec_byte_iter_0() {
    let text = vec![
        vec_ref("(mod (X)"),
        vec_ref("  (defun F (A) (* A 2))"),
        vec_ref("  (F X)"),
        vec_ref("  )"),
    ];

    let expected_bytes = b"(mod (X)\n  (defun F (A) (* A 2))\n  (F X)\n  )\n";

    for (i, b) in DocVecByteIter::new(&text).enumerate() {
        assert_eq!(expected_bytes[i], b);
    }
}

fn is_identifier_char(ch: u8) -> bool {
    ch != b'(' && ch != b')' && ch != b';' && !ch.is_ascii_whitespace()
}

#[test]
fn test_is_identifier_char_open_paren() {
    assert!(!is_identifier_char(b'('));
}

#[test]
fn test_is_identifier_char_close_paren() {
    assert!(!is_identifier_char(b')'));
}

#[test]
fn test_is_identifier_char_space() {
    assert!(!is_identifier_char(b' '));
}

#[test]
fn test_is_identifier_char_letter() {
    assert!(is_identifier_char(b'b'));
}

#[test]
fn test_is_identifier_char_symbol() {
    assert!(is_identifier_char(b'@'));
}

// Given a refcounted line reference and a "point"
// (@see https://www.gnu.org/software/emacs/manual/html_node/elisp/Point.html )
// Search for the bounds of a lisp like identifier at the point.
pub fn find_ident(line: Rc<Vec<u8>>, char_at: u32) -> Option<Vec<u8>> {
    let ca_size = char_at as usize;
    let ll = line.len();

    if ca_size > ll {
        return None;
    }

    let borrowed: &Vec<u8> = line.borrow();
    let mut lb = ca_size;
    let mut ub = ca_size;

    if !is_identifier_char(borrowed[lb]) {
        return None;
    }

    while lb > 0 && is_identifier_char(borrowed[lb - 1]) {
        lb -= 1;
    }
    while ub < borrowed.len() && is_identifier_char(borrowed[ub]) {
        ub += 1;
    }

    let ident_vec: Vec<u8> = borrowed[lb..].iter().take(ub - lb).copied().collect();

    Some(ident_vec)
}

#[test]
fn test_find_ident_single_char() {
    assert_eq!(find_ident(vec_ref("(x y z)"), 1), Some(b"x".to_vec()));
}

#[test]
fn test_find_ident_multi_char() {
    assert_eq!(find_ident(vec_ref("(x yyy z)"), 4), Some(b"yyy".to_vec()));
}

#[test]
fn test_find_ident_strange() {
    assert_eq!(
        find_ident(vec_ref("(x !!!$$$$2234234 z)"), 6),
        Some(b"!!!$$$$2234234".to_vec())
    );
}

#[test]
fn test_find_ident_between_identifiers() {
    assert_eq!(find_ident(vec_ref("(x yyy z)"), 2), None);
}

#[test]
fn test_find_ident_first_char() {
    assert_eq!(
        find_ident(vec_ref("ALONE_IDENT"), 0),
        Some(b"ALONE_IDENT".to_vec())
    );
}

#[test]
fn test_find_ident_first_char_paren() {
    assert_eq!(find_ident(vec_ref("(x yyy z)"), 0), None);
}

#[test]
fn test_find_ident_last_paren() {
    assert_eq!(find_ident(vec_ref("(x yyy z)"), 8), None);
}

// A position points to "first in list" if reversing past all alphanumerics
// then all spaces yields a '('.
pub fn is_first_in_list(lines: &DocData, position: &Position) -> bool {
    let mut current_char = b' ';
    let mut pos_walk = *position;

    // Reverse past this identifier's start.
    while let Some((ch, p)) = lines.get_prev_position(&pos_walk) {
        current_char = ch;

        if !is_identifier_char(ch) {
            break;
        }

        pos_walk = p;
    }

    // We ran into ( early.
    if current_char == b'(' {
        return true;
    }

    // Reverse past spaces.
    while let Some((ch, p)) = lines.get_prev_position(&pos_walk) {
        current_char = ch;

        if !ch.is_ascii_whitespace() {
            break;
        }

        pos_walk = p;
    }

    current_char == b'('
}

#[cfg(test)]
fn make_simple_test_doc_data_from_lines(filename: &str, lines: &[&str]) -> DocData {
    DocData {
        fullname: filename.to_string(),
        text: lines.iter().copied().map(vec_ref).collect(),
        version: 1,
        comments: Default::default(),
    }
}

#[test]
fn test_is_first_in_list_simple() {
    let simple_doc = make_simple_test_doc_data_from_lines("test.clsp", &["(hi there)"]);
    assert!(is_first_in_list(
        &simple_doc,
        &Position {
            line: 0,
            character: 2
        }
    ));
}

#[test]
fn test_is_first_in_list_with_spaces() {
    let simple_doc = make_simple_test_doc_data_from_lines("test.clsp", &["(    hi there)"]);
    assert!(is_first_in_list(
        &simple_doc,
        &Position {
            line: 0,
            character: 6
        }
    ));
}

#[test]
fn test_is_first_in_list_diff_line() {
    let simple_doc = make_simple_test_doc_data_from_lines("test.clsp", &["(", "  hi there", ")"]);
    assert!(is_first_in_list(
        &simple_doc,
        &Position {
            line: 1,
            character: 2
        }
    ));
}

#[test]
fn test_is_first_in_list_one_after() {
    let simple_doc = make_simple_test_doc_data_from_lines("test.clsp", &["(hi there)"]);
    assert!(is_first_in_list(
        &simple_doc,
        &Position {
            line: 0,
            character: 3
        }
    ));
}

#[test]
fn test_not_is_first_in_list_next_word() {
    let simple_doc = make_simple_test_doc_data_from_lines("test.clsp", &["(hi there)"]);
    assert!(!is_first_in_list(
        &simple_doc,
        &Position {
            line: 0,
            character: 4
        }
    ));
}

// Given a position, return the identifier at that position.  Relies on find_ident.
pub fn get_positional_text(lines: &DocData, position: &Position) -> Option<Vec<u8>> {
    let pl = position.line as usize;
    if pl < lines.text.len() {
        let line = lines.text[pl].clone();
        find_ident(line, position.character)
    } else {
        None
    }
}

#[test]
fn test_get_positional_text() {
    let simple_doc = make_simple_test_doc_data_from_lines("test.clsp", &["(", "  hi there", ")"]);
    assert_eq!(
        get_positional_text(
            &simple_doc,
            &Position {
                line: 1,
                character: 3
            }
        ),
        Some(b"hi".to_vec())
    );
}

// Determine whether the character is an identifier char.
pub fn is_identifier(v: &[u8]) -> bool {
    v.iter().copied().all(is_identifier_char)
}

#[test]
fn test_is_identifier() {
    assert!(is_identifier(b"test"));
}

#[test]
fn test_is_not_identifier() {
    assert!(!is_identifier(b"; comment instead"));
}

fn make_inner_function_scopes(scopes: &mut Vec<ParseScope>, body: &BodyForm) {
    match body {
        BodyForm::Let(LetFormKind::Sequential, letdata) => {
            if letdata.bindings.is_empty() {
                make_inner_function_scopes(scopes, letdata.body.borrow());
                return;
            }

            let binding = &letdata.bindings[0];
            let new_location = if letdata.bindings.len() == 1 {
                letdata.body.loc()
            } else {
                letdata.bindings[1].loc().ext(&letdata.loc.ending())
            };

            let mut variables = HashSet::new();
            variables.insert(SExp::Atom(binding.nl.clone(), binding.name.clone()));

            let mut inner_scopes = Vec::new();

            make_inner_function_scopes(
                &mut inner_scopes,
                &BodyForm::Let(
                    LetFormKind::Sequential,
                    LetData {
                        loc: new_location.clone(),
                        kw: letdata.kw.clone(),
                        bindings: letdata.bindings.iter().skip(1).cloned().collect(),
                        body: letdata.body.clone(),
                    },
                ),
            );

            scopes.push(ParseScope {
                region: new_location,
                kind: ScopeKind::Let,
                variables,
                functions: HashSet::new(),
                containing: inner_scopes,
            });
        }
        BodyForm::Let(LetFormKind::Parallel, letdata) => {
            if letdata.bindings.is_empty() {
                make_inner_function_scopes(scopes, letdata.body.borrow());
                return;
            }

            let mut name_set = HashSet::new();
            for b in letdata.bindings.iter() {
                let new_name = SExp::Atom(b.nl.clone(), b.name.clone());
                name_set.insert(new_name);
            }

            let mut inner_scopes = Vec::new();
            make_inner_function_scopes(&mut inner_scopes, letdata.body.borrow());

            let new_scope = ParseScope {
                region: letdata.bindings[0].loc.ext(&letdata.body.loc().ending()),
                kind: ScopeKind::Let,
                variables: name_set,
                functions: HashSet::new(),
                containing: inner_scopes,
            };
            scopes.push(new_scope);
        }

        BodyForm::Call(_, v, rest_args) => {
            for elt in v.iter() {
                make_inner_function_scopes(scopes, elt);
            }

            if let Some(tail) = rest_args {
                make_inner_function_scopes(scopes, tail)
            }
        }
        _ => {}
    }
}

fn make_helper_scope(h: &HelperForm) -> Option<ParseScope> {
    let loc = h.loc();

    let mut kind = None;
    let mut args = HashSet::new();
    let mut inner_scopes = Vec::new();

    match h {
        HelperForm::Defun(_, d) => {
            kind = Some(ScopeKind::Function);
            make_arg_set(&mut args, d.args.clone());
            make_inner_function_scopes(&mut inner_scopes, &d.body);
        }
        HelperForm::Defmacro(m) => {
            kind = Some(ScopeKind::Macro);
            make_arg_set(&mut args, m.args.clone());
        }
        _ => {}
    }

    kind.map(|k| ParseScope {
        kind: k,
        region: loc,
        variables: args,
        functions: HashSet::new(),
        containing: inner_scopes,
    })
}

#[cfg(test)]
fn get_test_program_for_scope_tests(file: &str, prog: &[Rc<Vec<u8>>]) -> CompileForm {
    let sl = Srcloc::start(file);
    let parsed = parse_sexp(sl, DocVecByteIter::new(prog)).expect("should parse");
    let opts = Rc::new(DefaultCompilerOpts::new(file));
    frontend(opts, &parsed).expect("should compile")
}

#[cfg(test)]
fn make_test_program_scope(file: &str, prog: &[Rc<Vec<u8>>]) -> (CompileForm, ParseScope) {
    let compiled = get_test_program_for_scope_tests(file, prog);
    (
        compiled.clone(),
        ParseScope {
            region: compiled.loc.clone(),
            kind: ScopeKind::Module,
            variables: Default::default(),
            functions: compiled
                .helpers
                .iter()
                .filter_map(|h| {
                    if let HelperForm::Defun(_, d) = &h {
                        return Some(SExp::atom_from_string(
                            d.nl.clone(),
                            &decode_string(h.name()),
                        ));
                    }
                    None
                })
                .collect(),
            containing: compiled
                .helpers
                .iter()
                .filter_map(|h| make_helper_scope(&h))
                .collect(),
        },
    )
}

#[test]
fn make_scope_stack_simple() {
    let file = "test.clsp";
    let prog = &[
        "(mod ()",
        "  (defun test1 (X) (+ X 1))",
        "  (defconstant RRR 3)",
        "  (defun-inline test2 (A B) (let ((C 5)) (+ A B C)))",
        "  (- (test1 8) (test2 9 7)))",
    ];
    let doc = make_simple_test_doc_data_from_lines(file, prog);
    let (compiled, program_scope) = make_test_program_scope(file, &doc.text);
    assert_eq!(program_scope.functions.len(), 2);
    assert_eq!(program_scope.containing.len(), 2);
    assert!(program_scope
        .functions
        .contains(&SExp::atom_from_string(compiled.loc.clone(), "test1")));
    assert!(program_scope
        .functions
        .contains(&SExp::atom_from_string(compiled.loc.clone(), "test2")));
    let filename_rc = compiled.loc.file.clone();
    assert_eq!(
        program_scope.containing[0].region,
        Srcloc::new(filename_rc.clone(), 2, 3).ext(&Srcloc::new(filename_rc.clone(), 2, 25))
    );
    assert_eq!(
        program_scope.containing[1].region,
        Srcloc::new(filename_rc.clone(), 4, 3).ext(&Srcloc::new(filename_rc.clone(), 4, 50))
    );
    assert_eq!(program_scope.containing[1].containing.len(), 1);
    assert_eq!(
        program_scope.containing[1].containing[0].kind,
        ScopeKind::Let
    );
    for v in program_scope.containing[1].containing[0].variables.iter() {
        let vrange = DocRange::from_srcloc(v.loc()).to_range();
        assert_eq!(
            get_positional_text(&doc, &vrange.start),
            Some(b"C".to_vec())
        );
    }
}

// Given a position in the source, find scopes that overlap.
pub fn find_scope_stack(out_scopes: &mut Vec<ParseScope>, scope: &ParseScope, position: &Srcloc) {
    if scope.region.overlap(position) {
        for s in scope.containing.iter() {
            find_scope_stack(out_scopes, s, position);
        }
        out_scopes.push(scope.clone());
    }
}

// Given a range, make some new text that positions the text of that range in the
// same location it's at in the original, and reproduces the text in the range.
// We will add spaces and carriage return appropriately so the requested range
// locates as in the original doc if space_for_range is true, otherwise it'll be
// isolated.
pub fn grab_scope_doc_range(
    text: &[Rc<Vec<u8>>],
    range: &DocRange,
    space_for_range: bool,
) -> Vec<u8> {
    let mut res = Vec::new();

    let loc = &range.start;
    let eloc = &range.end;

    if space_for_range {
        res.append(&mut vec![b'\n'; loc.line as usize]);
        res.append(&mut vec![b' '; loc.character as usize]);
    }

    // First line
    let tline = text[loc.line as usize].clone();
    let text_borrowed: &Vec<u8> = tline.borrow();

    if eloc.line == loc.line {
        // Only line
        for ch in text_borrowed
            .iter()
            .take(eloc.character as usize)
            .skip(loc.character as usize)
        {
            res.push(*ch);
        }

        return res;
    }

    for ch in text_borrowed.iter().skip(loc.character as usize) {
        res.push(*ch);
    }

    // Inside lines.
    let end_line = if (eloc.line as usize) > text.len() {
        text.len()
    } else {
        eloc.line as usize
    };

    for l in text.iter().take(end_line).skip((loc.line + 1) as usize) {
        res.push(b'\n');
        let iline = l.clone();
        let il: &Vec<u8> = iline.borrow();
        for ch in il.iter() {
            res.push(*ch);
        }
    }

    let eline = if end_line < text.len() {
        text[end_line].clone()
    } else {
        Rc::new(vec![])
    };

    let end_borrowed: &Vec<u8> = eline.borrow();

    res.push(b'\n');
    for ch in end_borrowed.iter().take(eloc.character as usize) {
        res.push(*ch);
    }

    res
}

#[test]
fn test_grab_scope_doc_range_spaces() {
    let file = "test.clsp";
    let prog = &[
        "(mod ()",
        "  (defun test1 (X) (+ X 1))",
        "  (defconstant RRR 3)",
        "  (defun-inline test2 (A B) (let ((C 5)) (+ A B C)))",
        "  (- (test1 8) (test2 9 7)))",
    ];
    let doc = make_simple_test_doc_data_from_lines(file, prog);
    let captured = grab_scope_doc_range(
        &doc.text,
        &DocRange {
            start: DocPosition {
                line: 1,
                character: 2,
            },
            end: DocPosition {
                line: 3,
                character: 0,
            },
        },
        true,
    );

    assert_eq!(
        captured,
        b"\n  (defun test1 (X) (+ X 1))\n  (defconstant RRR 3)\n".to_vec()
    );
}

#[test]
fn test_grab_scope_doc_range_no_spaces() {
    let file = "test.clsp";
    let prog = &[
        "(mod ()",
        "  (defun test1 (X) (+ X 1))",
        "  (defconstant RRR 3)",
        "  (defun-inline test2 (A B) (let ((C 5)) (+ A B C)))",
        "  (- (test1 8) (test2 9 7)))",
    ];
    let doc = make_simple_test_doc_data_from_lines(file, prog);
    let captured = grab_scope_doc_range(
        &doc.text,
        &DocRange {
            start: DocPosition {
                line: 1,
                character: 2,
            },
            end: DocPosition {
                line: 3,
                character: 0,
            },
        },
        false,
    );

    assert_eq!(
        captured,
        b"(defun test1 (X) (+ X 1))\n  (defconstant RRR 3)\n".to_vec()
    );
}

pub fn make_arg_set(set: &mut HashSet<SExp>, args: Rc<SExp>) {
    match args.borrow() {
        SExp::Atom(l, a) => {
            set.insert(SExp::Atom(l.clone(), a.clone()));
        }
        SExp::Cons(_, a, b) => {
            make_arg_set(set, a.clone());
            make_arg_set(set, b.clone());
        }
        _ => {}
    }
}

#[test]
fn test_make_arg_set() {
    let sl = Srcloc::start("test.clsp");
    let parsed = parse_sexp(
        sl.clone(),
        b"(arg1 arg2 (hi . there) () 999)".iter().copied(),
    )
    .expect("should parse");
    let mut arg_set = HashSet::new();
    make_arg_set(&mut arg_set, parsed[0].clone());
    assert_eq!(arg_set.len(), 4);
    assert!(arg_set.contains(&SExp::atom_from_string(sl.clone(), "arg1")));
    assert!(arg_set.contains(&SExp::atom_from_string(sl.clone(), "arg2")));
    assert!(arg_set.contains(&SExp::atom_from_string(sl.clone(), "hi")));
    assert!(arg_set.contains(&SExp::atom_from_string(sl.clone(), "there")));
}

// Given a document, give the ranges of the second parenthesis level.
// Source files in chialisp are for legacy reasons all expected to contain only
// one toplevel element, so the areas of interest that may move or change are at
// the second level.
pub fn make_simple_ranges(srctext: &[Rc<Vec<u8>>]) -> Vec<DocRange> {
    let mut ranges = Vec::new();
    let mut in_comment = false;
    let mut start = None;
    let mut level = 0;
    let mut line = 0;
    let mut character = 0;

    for i in DocVecByteIter::new(srctext) {
        if i == b';' {
            character += 1;
            in_comment = true;
        } else if i == b'\n' {
            line += 1;
            character = 0;
            in_comment = false;
        } else if i == b'(' {
            if !in_comment {
                if level == 1 && start.is_none() {
                    start = Some(DocPosition { line, character });
                }
                level += 1;
            }
            character += 1;
        } else if i == b')' {
            // We expect to contain only one toplevel list, so other ends
            // are probably a misparse.
            if !in_comment && level > 0 {
                level -= 1;

                if level == 1 {
                    if let Some(s) = start.clone() {
                        ranges.push(DocRange {
                            start: s,
                            end: DocPosition {
                                line,
                                character: character + 1,
                            },
                        });
                        start = None;
                    }
                }
            }
            character += 1;
        } else {
            character += 1;
        }
    }

    ranges
}

#[test]
fn test_make_simple_ranges_1() {
    let test_data = &[
        vec_ref("( (test) (test2)"),
        vec_ref(""),
        vec_ref("   (here and there (inner)"),
        vec_ref("     (inner inner) () and more and more)"),
        vec_ref("(and here)"),
        vec_ref("  ()"),
        vec_ref(")"),
    ];
    assert_eq!(
        make_simple_ranges(test_data),
        vec![
            DocRange {
                start: DocPosition {
                    line: 0,
                    character: 2
                },
                end: DocPosition {
                    line: 0,
                    character: 8
                }
            },
            DocRange {
                start: DocPosition {
                    line: 0,
                    character: 9
                },
                end: DocPosition {
                    line: 0,
                    character: 16
                }
            },
            DocRange {
                start: DocPosition {
                    line: 2,
                    character: 3
                },
                end: DocPosition {
                    line: 3,
                    character: 40
                }
            },
            DocRange {
                start: DocPosition {
                    line: 4,
                    character: 0
                },
                end: DocPosition {
                    line: 4,
                    character: 10
                }
            },
            DocRange {
                start: DocPosition {
                    line: 5,
                    character: 2
                },
                end: DocPosition {
                    line: 5,
                    character: 4
                }
            }
        ]
    );
}
