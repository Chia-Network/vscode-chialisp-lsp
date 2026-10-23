use super::*;
use crate::lsp::parse::{find_ident, is_first_in_list};
use crate::lsp::patch::compute_comment_lines;
use chialisp::compiler::sexp::{parse_sexp, SExp};

#[test]
fn compiler_and_scanner_agree_on_adjacent_semicolons() {
    for atom in ["1;comment", "foo;bar", "foo;(bar", "foo'bar", "foo\"bar"] {
        let source = format!("(mod () (list {} 1)) ; actual comment", atom);
        let parsed = parse_sexp(Srcloc::start("adjacent.clsp"), source.bytes()).unwrap();
        let module = parsed[0].proper_list().unwrap();
        let call = module[2].proper_list().unwrap();
        assert!(matches!(&call[1], SExp::Atom(_, value) if value == atom.as_bytes()));
        let lines = split_text(&source);
        let comments = compute_comment_lines(&lines);
        assert_eq!(comments.get(&0), Some(&source.find("; actual").unwrap()));
        let (_, ranges) = make_simple_ranges(&lines);
        assert_eq!(ranges.len(), 2);
        assert_eq!(
            ranges[1].end.character as usize,
            source.find(" ; actual").unwrap() - 1
        );
        let position = source.find(atom).unwrap();
        assert_eq!(
            find_ident(lines[0].clone(), position as u32),
            Some(atom.as_bytes().to_vec())
        );
        let doc = DocData {
            fullname: "adjacent.clsp".into(),
            text: lines,
            version: 1,
            comments,
        };
        assert!(!is_first_in_list(
            &doc,
            &Position::new(0, (position + atom.len() - 1) as u32)
        ));
    }
}

#[test]
fn quoted_semicolons_and_adjacent_real_comments_match_the_compiler() {
    let source = "(mod () (list \"foo;bar\" 'foo;bar'; real comment\n 1))";
    let parsed = parse_sexp(Srcloc::start("strings.clsp"), source.bytes()).unwrap();
    let module = parsed[0].proper_list().unwrap();
    let call = module[2].proper_list().unwrap();
    assert!(matches!(&call[1], SExp::QuotedString(_, _, value) if value == b"foo;bar"));
    assert!(matches!(&call[2], SExp::QuotedString(_, _, value) if value == b"foo;bar"));
    let lines = split_text(source);
    assert_eq!(
        compute_comment_lines(&lines).get(&0),
        Some(&source.find("; real").unwrap())
    );
    assert_eq!(
        find_ident(lines[0].clone(), source.find("real").unwrap() as u32),
        None
    );
}

#[test]
fn structured_list_prefix_remains_structural() {
    let source = "(mod () (list #(1 2) 3))";
    assert!(parse_sexp(Srcloc::start("structured.clsp"), source.bytes()).is_ok());
    let (_, ranges) = make_simple_ranges(&split_text(source));
    assert_eq!(ranges.len(), 2);
    assert_eq!(ranges[1].end.character as usize, source.len() - 1);
}

#[test]
fn module_macros_can_generate_names_containing_semicolons() {
    use crate::lsp::completion::PRIM_NAMES;
    use crate::lsp::reparse::{combine_new_with_old_parse, reparse_subset};
    let filename = "file:///generated.clsp";
    let source = "(defmac define (NAME) (c (q . defun) (c NAME (q . (() 1)))))\n(define foo;generated)\n(export () (foo;generated))";
    let text = split_text(source);
    let opts = Rc::new(DefaultCompilerOpts::new(filename)).set_stdenv(false);
    let old = ParsedDoc::new(Srcloc::start(filename));
    let (module_style, ranges) = make_simple_ranges(&text);
    let reparsed = reparse_subset(
        &PRIM_NAMES,
        opts,
        &text,
        filename,
        module_style,
        &ranges,
        &old.compiled,
        &old.helpers,
    );
    let parsed = combine_new_with_old_parse(filename, &text, &old, &reparsed);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    assert!(parsed
        .compiled
        .helpers
        .iter()
        .any(|helper| helper.name() == b"foo;generated"));
    assert!(compute_comment_lines(&text).is_empty());
    assert_eq!(
        find_ident(text[2].clone(), 13),
        Some(b"foo;generated".to_vec())
    );
}
