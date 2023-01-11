use std::borrow::Borrow;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use clvm_tools_rs::compiler::clvm::sha256tree_from_atom;
use clvm_tools_rs::compiler::comptypes::{BodyForm, CompileErr, CompileForm, CompilerOpts, HelperForm};
use clvm_tools_rs::compiler::frontend::{compile_bodyform, compile_helperform};
use crate::lsp::completion::PRIM_NAMES;
use crate::lsp::parse::{
    grab_scope_doc_range, recover_scopes, ParsedDoc,
};
use crate::lsp::types::{DocPosition, DocRange, Hash, IncludeData, IncludeKind, ParseScope, ReparsedHelper, ReparsedExp};
use clvm_tools_rs::compiler::sexp::{parse_sexp, SExp};
use clvm_tools_rs::compiler::srcloc::Srcloc;

lazy_static! {
    static ref END_TAG: Vec<u8> = b"__chia_end".to_vec();
}

pub struct ReparsedModule {
    pub ignored: bool,
    pub mod_kw: Option<Srcloc>,
    pub args: Rc<SExp>,
    pub helpers: HashMap<Hash, ReparsedHelper>,
    pub exp: Option<ReparsedExp>,
    pub unparsed: HashMap<Hash, DocRange>,
    pub includes: HashMap<Hash, IncludeData>,
    pub errors: Vec<CompileErr>,
}

pub fn parse_include(sexp: Rc<SExp>) -> Option<IncludeData> {
    // Match include with quoted or unquoted argument.
    let matches_include = |l: &[SExp]| {
        if let (SExp::Atom(kl, incl), SExp::Atom(nl, fname)) = (l[0].borrow(), l[1].borrow()) {
            Some((kl.clone(), incl.clone(), nl.clone(), fname.clone()))
        } else if let (SExp::Atom(kl, incl), SExp::QuotedString(nl, _, fname)) =
            (l[0].borrow(), l[1].borrow())
        {
            Some((kl.clone(), incl.clone(), nl.clone(), fname.clone()))
        } else {
            None
        }
    };

    sexp.proper_list().and_then(|l| {
        if l.len() == 2 {
            if let Some((kl, incl, nl, fname)) = matches_include(&l) {
                if incl == b"include" {
                    return Some(IncludeData {
                        loc: sexp.loc(),
                        kw_loc: kl,
                        name_loc: nl,
                        kind: IncludeKind::Include,
                        filename: fname,
                        found: None
                    });
                }
            }
        } else if l.len() == 3 {
            if let (SExp::Atom(kl, incl), SExp::Atom(il, _), SExp::Atom(nl, fname)) =
                (l[0].borrow(), l[1].borrow(), l[2].borrow())
            {
                if incl == b"compile-file" {
                    return Some(IncludeData {
                        loc: sexp.loc(),
                        kw_loc: kl.clone(),
                        name_loc: nl.clone(),
                        kind: IncludeKind::CompileFile(il.clone()),
                        filename: fname.clone(),
                        found: None
                    });
                }
            }
        } else if l.len() == 4 {
            if let (
                SExp::Atom(kl, incl),
                SExp::Atom(il, _),
                SExp::Atom(tl, _),
                SExp::Atom(nl, fname),
            ) = (l[0].borrow(), l[1].borrow(), l[2].borrow(), l[3].borrow())
            {
                if incl == b"embed-file" {
                    return Some(IncludeData {
                        loc: sexp.loc(),
                        kw_loc: kl.clone(),
                        name_loc: nl.clone(),
                        kind: IncludeKind::EmbedFile(il.clone(), tl.clone()),
                        filename: fname.clone(),
                        found: None
                    });
                }
            }
        }

        None
    })
}

pub fn reparse_subset(
    prims: &[Vec<u8>],
    opts: Rc<dyn CompilerOpts>,
    doc: &[Rc<Vec<u8>>],
    uristring: &str,
    simple_ranges: &[DocRange],
    compiled: &CompileForm,
    prev_helpers: &HashMap<Hash, ReparsedHelper>,
) -> ReparsedModule {
    let mut result = ReparsedModule {
        ignored: false,
        mod_kw: None,
        args: compiled.args.clone(),
        helpers: HashMap::new(),
        exp: None,
        includes: HashMap::new(),
        unparsed: HashMap::new(),
        errors: Vec::new(),
    };

    // if it's a module, we can patch the prefix in, otherwise make a (mod ()
    // prefix for it.
    // We can take the last phrase and if it's not a helper, we can use it as
    // the end of the document.
    let mut took_args = false;
    let mut took_exp = false;
    let mut have_mod = false;

    if simple_ranges.is_empty() {
        // There's nothing to be gained by trying to do incremental.
        return result;
    }

    // Find out if there's a single atom before the first identified
    // expression.
    let docstart = Srcloc::start(uristring);
    let prefix_start = DocPosition {
        line: 0,
        character: 0,
    };
    let prefix_range = DocRange {
        start: prefix_start,
        end: simple_ranges[0].start.clone(),
    };
    let mut prefix_text = grab_scope_doc_range(doc, &prefix_range, false);
    // TODO hash prefix to prevent reparsing.
    prefix_text.push(b')');

    if let Some(prefix_parse) = parse_sexp(docstart, prefix_text.iter().copied())
        .ok()
        .and_then(|s| {
            if s.is_empty() {
                None
            } else {
                s[0].proper_list()
            }
        })
    {
        let mut form_error_start = 0;
        if !prefix_parse.is_empty() {
            if let SExp::Atom(l, m) = prefix_parse[0].borrow() {
                have_mod = m == b"mod";

                if !have_mod {
                    if let Some(_) = prims.iter().position(|prim| {
                        prim == m
                    }) {
                        result.ignored = true;
                        return result;
                    }
                }

                form_error_start = 2;
                result.mod_kw = Some(l.clone());
            } else if let SExp::Integer(_, _) = prefix_parse[0].borrow() {
                result.ignored = true;
                return result;
            }

            if have_mod && prefix_parse.len() == 2 {
                took_args = true;
                result.args = Rc::new(prefix_parse[prefix_parse.len() - 1].clone());
            }
        }

        for p in prefix_parse.iter().skip(form_error_start) {
            result
                .errors
                .push(CompileErr(p.loc(), "bad form".to_string()));
        }
    }

    // Find out of there's a single atom after the last identified atom.
    let suffix_start = simple_ranges[simple_ranges.len() - 1].end.clone();
    let doc_end = DocPosition {
        line: doc.len() as u32,
        character: 0,
    };
    let suffix_range = DocRange {
        start: suffix_start.clone(),
        end: doc_end.clone(),
    };
    let mut suffix_text = grab_scope_doc_range(doc, &suffix_range, false);

    let mut break_end = suffix_text.len();

    // Ensure we can parse to the right locations in the source file.
    // Since our parser can handle a list of parsed objects, remove the
    // final paren.

    // Find last )
    for (i, ch) in suffix_text.iter().enumerate() {
        if *ch == b')' {
            break_end = i;
            break;
        }
    }

    if break_end == suffix_text.len() {
        result.errors.push(CompileErr(
            DocRange {
                start: suffix_start.clone(),
                end: doc_end,
            }
            .to_srcloc(uristring),
            "Missing end paren for enclosing list form".to_string(),
        ));
    }

    suffix_text = suffix_text.iter().take(break_end).copied().collect();
    // Collect hash of prefix and suffix so we can reparse everything if
    // they change.
    let suffix_hash = Hash::new(&sha256tree_from_atom(&suffix_text));

    if let Ok(suffix_parse) = parse_sexp(
        Srcloc::new(
            Rc::new(uristring.to_owned()),
            (suffix_start.line + 1) as usize,
            (suffix_start.character + 1) as usize,
        ),
        suffix_text.iter().copied(),
    ) {
        if !suffix_parse.is_empty() {
            took_exp = true;
            result.exp = Some(ReparsedExp {
                hash: suffix_hash,
                parsed: compile_bodyform(
                    opts.clone(),
                    suffix_parse[suffix_parse.len() - 1].clone(),
                ),
            });
        }
    }

    // Capture the simple ranges, then check each one's hash
    // if the hash isn't present in the helpers we have, we need to run the
    // frontend on it.
    let start_parsing_forms = if took_args {
        0
    } else {
        // 1 additional if this is a mod (not an include) and the arguments were
        // not captured by the prefix pre-parse, which places a closing paren
        // before the first parenthesized form and parses just that (to save time).
        // That'll be true if the arguments were a destructured cons form of some
        // kind as opposed to an un-destructured atom.
        have_mod as usize
    };
    let parse_as_body = if have_mod && !took_exp {
        simple_ranges.len() - 1
    } else {
        simple_ranges.len()
    };

    for (i, r) in simple_ranges.iter().enumerate() {
        let text = grab_scope_doc_range(doc, r, false);
        let hash = Hash::new(&sha256tree_from_atom(&text));

        // Always reparse the body for convenience.  It's one form so it won't
        // accumulate.
        let same_range = prev_helpers
            .get(&hash)
            .map(|earlier| r == &earlier.range)
            .unwrap_or(false);
        if !prev_helpers.contains_key(&hash) || i == parse_as_body || !same_range {
            let loc = Srcloc::new(
                Rc::new(uristring.to_owned()),
                (r.start.line + 1) as usize,
                (r.start.character + 1) as usize,
            );
            match parse_sexp(loc.clone(), text.iter().copied()) {
                Ok(parsed) => {
                    if i < start_parsing_forms {
                        result.args = parsed[0].clone();
                        continue;
                    } else if i == parse_as_body {
                        result.exp = Some(ReparsedExp {
                            hash,
                            parsed: compile_bodyform(opts.clone(), parsed[0].clone()),
                        });
                        continue;
                    } else if let Some(include) = parse_include(parsed[0].clone()) {
                        result.includes.insert(hash, include.clone());
                        continue;
                    }

                    result.helpers.insert(
                        hash.clone(),
                        ReparsedHelper {
                            hash,
                            range: r.clone(),
                            parsed: compile_helperform(opts.clone(), parsed[0].clone()).and_then(
                                |mh| {
                                    if let Some(h) = mh {
                                        Ok(h)
                                    } else {
                                        Err(CompileErr(loc, "must be a helper form".to_string()))
                                    }
                                },
                            ),
                        },
                    );
                }
                Err((l, s)) => {
                    result.helpers.insert(
                        hash.clone(),
                        ReparsedHelper {
                            hash,
                            range: r.clone(),
                            parsed: Err(CompileErr(l, s)),
                        },
                    );
                }
            }
        } else {
            result.unparsed.insert(hash, r.clone());
        }
    }

    result
}

// Only the top scope is relevant for now.
fn find_function_in_scopes(prims: &[Vec<u8>], scopes: &ParseScope, name: &SExp) -> bool {
    if let SExp::Atom(_, a) = name {
        scopes.functions.contains(name) || prims.iter().any(|p| p == a)
    } else {
        false
    }
}

// Add errors for unrecognized calls.
pub fn check_live_helper_calls(
    prims: &[Vec<u8>],
    scopes: &ParseScope,
    exp: &BodyForm,
) -> Option<CompileErr> {
    match exp {
        BodyForm::Call(l, v) => {
            if v.is_empty() {
                return Some(CompileErr(l.clone(), "Empty function call".to_string()));
            }

            // Try to make sense of the list head
            if let BodyForm::Value(s) = v[0].borrow() {
                if !find_function_in_scopes(prims, scopes, s) {
                    return Some(CompileErr(
                        s.loc(),
                        format!("No such function found: {}", s),
                    ));
                }
            } else {
                return Some(CompileErr(
                    l.clone(),
                    "Inappropriate function name".to_string(),
                ));
            }

            for b in v.iter().skip(1) {
                if let Some(e) = check_live_helper_calls(prims, scopes, b) {
                    return Some(e);
                }
            }
        }
        BodyForm::Let(_kind, letdata) => {
            return check_live_helper_calls(prims, scopes, letdata.body.borrow());
        }
        _ => {}
    }

    None
}

fn determine_same_path(uristring: &str, query_file: &str) -> bool {
    // Iterate in reverse through path components and match them with case folded.
    let uristring_comps: Vec<String> = uristring.split('/').map(|s| s.to_owned()).collect();
    let query_file_comps: Vec<String> = query_file.split('/').map(|s| s.to_owned()).collect();

    // no match if the queried file has a longer path than the uristring.
    if uristring_comps.is_empty() || query_file_comps.is_empty() ||
        query_file_comps.len() > uristring.len() {
        return false;
    }

    let mut uristring_idx = uristring_comps.len();
    let mut query_file_idx = query_file_comps.len();

    while uristring_idx > 0 && query_file_idx > 0 {
        let uristring_comp = &uristring_comps[uristring_idx - 1];
        let query_file_comp = &query_file_comps[query_file_idx - 1];

        // Ignore single dots that may be joined in.
        if uristring_comp.is_empty() || uristring_comp == "." {
            uristring_idx -= 1;
            continue;
        }

        if query_file_comp.is_empty() || query_file_comp == "." {
            query_file_idx -= 1;
            continue;
        }

        if uristring_comp.to_lowercase() != query_file_comp.to_lowercase() {
            return false;
        }

        uristring_idx -= 1;
        query_file_idx -= 1;
    }

    // Same, this should do.
    return true;
}

pub fn combine_new_with_old_parse(
    uristring: &str,
    text: &[Rc<Vec<u8>>],
    parsed: &ParsedDoc,
    reparse: &ReparsedModule,
) -> ParsedDoc {
    let new_includes = reparse.includes.clone();
    let mut new_helpers = parsed.helpers.clone();
    let mut extracted_helpers = Vec::new();
    let mut to_remove = HashSet::new();
    let mut remove_names = HashSet::new();
    let mut hash_to_name = parsed.hash_to_name.clone();
    let mut out_errors: Vec<CompileErr> = reparse.errors.iter().filter(|e| {
        let borrowed_file: &String = e.0.file.borrow();
        determine_same_path(uristring, borrowed_file)
    }).cloned().collect();

    // Collect to-delete set.
    for (h, _) in new_helpers.iter() {
        if !reparse.unparsed.contains_key(&h) && !reparse.helpers.contains_key(h) {
            if let Some(name) = parsed.hash_to_name.get(h) {
                remove_names.insert(name.clone());
            }
            to_remove.insert(h.clone());
        }
    }

    for h in to_remove.iter() {
        hash_to_name.remove(h);
        new_helpers.remove(h);
    }

    // Iterate new helpers.
    for (h, p) in reparse.helpers.iter() {
        match &p.parsed {
            Err(e) => {
                out_errors.push(e.clone());
            }
            Ok(parsed) => {
                hash_to_name.insert(h.clone(), parsed.name().clone());
                extracted_helpers.push(parsed.clone());
            }
        }
        new_helpers.insert(h.clone(), p.clone());
    }

    // For helpers that parsed, replace them in the compile.
    let mut new_compile = parsed.compiled.replace_helpers(&extracted_helpers);

    // Handle args and body, which are optional since the file can represent
    // a list (include file).
    new_compile.args = reparse.args.clone();
    let empty_body = BodyForm::Quoted(SExp::Nil(reparse.args.loc()));
    match &reparse.exp {
        None => {
            new_compile.exp = Rc::new(empty_body);
        }
        Some(exp) => match &exp.parsed {
            Ok(body) => {
                new_compile.exp = Rc::new(body.clone());
            }
            Err(e) => {
                new_compile.exp = Rc::new(empty_body);
                out_errors.push(e.clone());
            }
        },
    }

    let compile_with_dead_helpers_removed = new_compile.remove_helpers(&remove_names);
    let scopes = recover_scopes(uristring, text, &new_compile);

    for h in compile_with_dead_helpers_removed.helpers.iter() {
        if let HelperForm::Defun(_, d) = h {
            if let Some(error) = check_live_helper_calls(&PRIM_NAMES, &scopes, &d.body) {
                out_errors.push(error);
            }
        }
    }

    // Check whether functions called in exp are live
    if let Some(error) =
        check_live_helper_calls(&PRIM_NAMES, &scopes, &compile_with_dead_helpers_removed.exp)
    {
        out_errors.push(error);
    }

    ParsedDoc {
        ignored: reparse.ignored,
        mod_kw: reparse.mod_kw.clone(),
        compiled: compile_with_dead_helpers_removed,
        errors: out_errors,
        scopes,
        includes: new_includes,
        helpers: new_helpers,
        hash_to_name,
        exp: reparse.exp.clone(),
    }
}
