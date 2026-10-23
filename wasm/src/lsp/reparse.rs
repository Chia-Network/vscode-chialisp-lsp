use std::borrow::Borrow;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use crate::lsp::completion::PRIM_NAMES;
use crate::lsp::parse::{grab_scope_doc_range, recover_scopes, IncludedFileSpec, ParsedDoc};
use crate::lsp::patch::get_bytes;
use crate::lsp::types::{
    DocPosition, DocRange, Hash, IncludeData, IncludeKind, ParseScope, ParsedForm, ReparsedExp,
    ReparsedHelper,
};
use chialisp::compiler::clvm::sha256tree_from_atom;
use chialisp::compiler::comptypes::{
    BodyForm, CompileErr, CompileForm, CompilerOpts, Export, FrontendOutput, HelperForm,
    NamespaceRefData,
};
use chialisp::compiler::frontend::{
    compile_bodyform, compile_helperform, compile_helperform_mm, compile_nsref,
    compute_live_helpers, match_export_form, HelperFormResult,
};
use chialisp::compiler::preprocessor::{detect_chialisp_module, Preprocessor};
use chialisp::compiler::prims::primquote;
use chialisp::compiler::sexp::{enlist, parse_sexp, SExp};
use chialisp::compiler::srcloc::Srcloc;

lazy_static! {
    static ref END_TAG: Vec<u8> = b"__chia_end".to_vec();
}

pub struct ReparsedModule {
    /// Compiler preprocessing is authoritative for complete module-style input.
    /// Raw forms below remain available for tolerant source highlighting.
    pub module_semantics: Option<Result<FrontendOutput, CompileErr>>,
    pub ignored: bool,
    pub mod_kw: Option<Srcloc>,
    pub args: Rc<SExp>,
    pub helpers: HashMap<Hash, ReparsedHelper>,
    pub exp: Option<ReparsedExp>,
    pub unparsed: HashMap<Hash, DocRange>,
    pub includes: HashMap<Hash, IncludedFileSpec>,
    pub errors: Vec<CompileErr>,
}

pub fn parse_include(sexp: Rc<SExp>) -> Option<IncludedFileSpec> {
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
                    return Some(IncludedFileSpec::Include(IncludeData {
                        loc: sexp.loc(),
                        kw_loc: kl,
                        name_loc: nl,
                        kind: IncludeKind::Include,
                        filename: fname,
                        found: None,
                    }));
                }
            }
        } else if l.len() == 3 {
            if let (SExp::Atom(kl, incl), SExp::Atom(il, _), SExp::Atom(nl, fname)) =
                (l[0].borrow(), l[1].borrow(), l[2].borrow())
            {
                if incl == b"compile-file" {
                    return Some(IncludedFileSpec::Include(IncludeData {
                        loc: sexp.loc(),
                        kw_loc: kl.clone(),
                        name_loc: nl.clone(),
                        kind: IncludeKind::CompileFile(il.clone()),
                        filename: fname.clone(),
                        found: None,
                    }));
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
                    return Some(IncludedFileSpec::Include(IncludeData {
                        loc: sexp.loc(),
                        kw_loc: kl.clone(),
                        name_loc: nl.clone(),
                        kind: IncludeKind::EmbedFile(il.clone(), tl.clone()),
                        filename: fname.clone(),
                        found: None,
                    }));
                }
            }
        } else if let Ok(HelperForm::Defnsref(imp)) = compile_nsref(sexp.loc(), &l) {
            return Some(IncludedFileSpec::Import(None, *imp.clone()));
        }

        None
    })
}

// Compiler 0.4.4 indexes the argument list before diagnosing an incomplete
// nested `(mod)`. Editor buffers routinely contain this intermediate form.
// Validate the expression shape before entering that non-recoverable WASM path.
fn validate_incomplete_mods(form: &SExp) -> Result<(), CompileErr> {
    if let SExp::Cons(_, head, tail) = form {
        if let SExp::Atom(_, name) = head.as_ref() {
            if name == b"q" || name == b"quote" || name == b"qq" {
                return Ok(());
            }
            if name == b"mod" && matches!(tail.as_ref(), SExp::Nil(_)) {
                return Err(CompileErr(form.loc(), "incomplete mod form".to_string()));
            }
            if [
                b"defun".as_slice(),
                b"defun-inline",
                b"defmac",
                b"defmacro",
                b"mod",
            ]
            .contains(&name.as_slice())
            {
                if let Some(parts) = form.proper_list() {
                    let first_body = if name == b"mod" { 2 } else { 3 };
                    for body in parts.iter().skip(first_body) {
                        validate_incomplete_mods(body)?;
                    }
                    return Ok(());
                }
            }
        } else if matches!(head.as_ref(), SExp::Integer(_, n) if n.to_string() == "1") {
            return Ok(());
        }
        validate_incomplete_mods(head)?;
        let mut rest = tail.as_ref();
        while let SExp::Cons(_, argument, next) = rest {
            validate_incomplete_mods(argument)?;
            rest = next.as_ref();
        }
    }
    Ok(())
}

fn validate_definition_forms(forms: &[Rc<SExp>]) -> Result<(), CompileErr> {
    // Definitions are compiled while macros are registered. Leave invocation
    // trees opaque until preprocessing, because they may be quoted macro data.
    for form in forms {
        if let Some(parts) = form.proper_list() {
            // Legacy include files enclose their helper forms in an outer
            // list. The compiler unwraps that container before registering
            // definitions, so validate those same definitions here as well.
            if matches!(parts.first(), Some(SExp::Cons(_, _, _))) {
                let helpers: Vec<_> = parts.into_iter().map(Rc::new).collect();
                validate_definition_forms(&helpers)?;
                continue;
            }
            if matches!(parts.first(), Some(SExp::Atom(_, name)) if [b"mod".as_slice(), b"defun", b"defun-inline", b"defmac", b"defmacro"].contains(&name.as_slice()))
            {
                validate_incomplete_mods(form)?;
            }
        }
    }
    Ok(())
}

/// Protect compiler-internal reads as well as editor parsing from the bundled
/// compiler's incomplete nested-mod panic.
pub fn validate_compiler_input(filename: &str, contents: &[u8]) -> Result<(), CompileErr> {
    // Readers are also used for opaque embed-file payloads. Ordinary syntax
    // errors remain the responsibility of the compiler's actual parse path.
    match parse_sexp(Srcloc::start(filename), contents.iter().copied()) {
        Ok(forms) => validate_definition_forms(&forms),
        Err(_) => Ok(()),
    }
}

fn safe_compile_bodyform(
    opts: Rc<dyn CompilerOpts>,
    form: Rc<SExp>,
) -> Result<BodyForm, CompileErr> {
    validate_incomplete_mods(&form)?;
    compile_bodyform(opts, form)
}

fn compile_helperform_with_loose_defconstant_and_module_forms(
    opts: Rc<dyn CompilerOpts>,
    parsed: Rc<SExp>,
) -> Result<Option<ParsedForm<HelperFormResult>>, CompileErr> {
    validate_incomplete_mods(&parsed)?;
    let is_defconstant = |sexp: &SExp| {
        if let SExp::Atom(_, name) = sexp {
            return name == b"defconstant";
        }

        false
    };
    if let Some(listed) = parsed.proper_list() {
        // Check for a defconstant keyword.
        if listed.len() == 3 && is_defconstant(&listed[0]) {
            let result = compile_helperform(opts.clone(), parsed.clone());

            // We got an error evaluating a defconstant form.  The body might
            // not be a valid expression.  This is special to defconstant ...
            // every other body is necesarily a BodyForm.  We can allow this
            // to be looser because it is in classic chialisp.
            if result.is_err() {
                let amended_instr = enlist(
                    parsed.loc(),
                    &[
                        Rc::new(listed[0].clone()),
                        Rc::new(listed[1].clone()),
                        Rc::new(primquote(parsed.loc(), Rc::new(listed[2].clone()))),
                    ],
                );
                // Try by enwrapping the body in quote so it can act as an
                // expression to the parser.  Other kinds of errors will still
                // go through.
                return compile_helperform(opts.clone(), Rc::new(amended_instr))
                    .map(|o| o.map(|h| ParsedForm::Helper(vec![h])));
            }
        }
    }

    // Not a proper list so not the kind of thing we're looking for.
    match compile_helperform(opts.clone(), parsed.clone()).map(|o| {
        o.filter(|h| !h.new_helpers.is_empty())
            .map(|h| ParsedForm::Helper(vec![h]))
    }) {
        Err(e) => match_export_form(opts.clone(), parsed.clone())
            .map(|o| o.map(ParsedForm::ModuleExport))
            .map_err(|_| e),
        r => r,
    }
}

/// Use the same module pipeline as the compiler, including compile-time macros.
/// Unlike `frontend`, this also accepts single-form module and include files;
/// those are useful editor documents even when they do not export a program.
fn preprocess_module(
    opts: Rc<dyn CompilerOpts>,
    forms: &[Rc<SExp>],
) -> Result<FrontendOutput, CompileErr> {
    let opts =
        if let Some(dialect) = detect_chialisp_module(Srcloc::start(&opts.filename()), forms)? {
            opts.set_dialect(dialect)
        } else {
            opts
        };
    validate_definition_forms(forms)?;
    let mut includes = Vec::new();
    let mut preprocessor = Preprocessor::new(opts.clone());
    let output = preprocessor.run_modules(&mut includes, forms)?;
    let mut helpers = Vec::new();
    let mut exports = Vec::new();
    for form in output.forms {
        validate_incomplete_mods(&form)?;
        if let Some(export) = match_export_form(opts.clone(), form.clone())? {
            exports.push(export);
        } else if let Some(helper) = compile_helperform_mm(opts.clone(), form, true)? {
            helpers.extend(helper.new_helpers);
        }
    }
    let loc = forms
        .first()
        .map(|f| f.loc())
        .unwrap_or_else(|| Srcloc::start(&opts.filename()));
    Ok(FrontendOutput::Module(
        CompileForm {
            loc: loc.clone(),
            include_forms: includes,
            args: Rc::new(SExp::Nil(loc.clone())),
            helpers,
            exp: Rc::new(BodyForm::Quoted(SExp::Nil(loc))),
        },
        exports,
    ))
}

#[allow(clippy::too_many_arguments)]
pub fn reparse_subset(
    prims: &[Vec<u8>],
    opts: Rc<dyn CompilerOpts>,
    doc: &[Rc<Vec<u8>>],
    uristring: &str,
    module_style: bool,
    simple_ranges: &[DocRange],
    compiled: &CompileForm,
    prev_helpers: &HashMap<Hash, ReparsedHelper>,
) -> ReparsedModule {
    let module_semantics = if module_style {
        Some(
            parse_sexp(Srcloc::start(uristring), get_bytes(doc).into_iter())
                .map_err(|(loc, message)| CompileErr(loc, message))
                .and_then(|forms| preprocess_module(opts.clone(), &forms)),
        )
    } else {
        None
    };
    let mut result = ReparsedModule {
        module_semantics,
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

                if !have_mod && prims.iter().any(|prim| prim == m) {
                    result.ignored = true;
                    return result;
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

    if break_end == suffix_text.len() && !module_style {
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
                parsed: safe_compile_bodyform(
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
                            parsed: safe_compile_bodyform(opts.clone(), parsed[0].clone()),
                        });
                        continue;
                    } else if let Some(include) = parse_include(parsed[0].clone()) {
                        result.includes.insert(hash, include.clone());
                        continue;
                    }

                    let dc_result = compile_helperform_with_loose_defconstant_and_module_forms(
                        opts.clone(),
                        parsed[0].clone(),
                    )
                    .and_then(|mh| {
                        if let Some(h) = mh {
                            Ok(h)
                        } else {
                            Err(CompileErr(loc, "must be a helper form".to_string()))
                        }
                    });
                    match dc_result {
                        Ok(ParsedForm::Helper(res)) => {
                            for resh in res.iter() {
                                for h in resh.new_helpers.iter() {
                                    if let HelperForm::Defnsref(import) = h {
                                        let nsref: &NamespaceRefData = import.borrow();
                                        result.includes.insert(
                                            hash.clone(),
                                            IncludedFileSpec::Import(None, nsref.clone()),
                                        );
                                    }
                                }
                                result.helpers.insert(
                                    hash.clone(),
                                    ReparsedHelper {
                                        hash: hash.clone(),
                                        range: r.clone(),
                                        parsed: Ok(ParsedForm::Helper(resh.new_helpers.clone())),
                                    },
                                );
                            }
                        }
                        Ok(ParsedForm::ModuleExport(res)) => {
                            result.helpers.insert(
                                hash.clone(),
                                ReparsedHelper {
                                    hash,
                                    range: r.clone(),
                                    parsed: Ok(ParsedForm::ModuleExport(res.clone())),
                                },
                            );
                        }
                        Err(e) => {
                            result.helpers.insert(
                                hash.clone(),
                                ReparsedHelper {
                                    hash,
                                    range: r.clone(),
                                    parsed: Err(e),
                                },
                            );
                        }
                    }
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
fn find_function_in_scopes<F>(
    prims: &[Vec<u8>],
    scopes: &ParseScope,
    frontend: &CompileForm,
    name: &SExp,
    imported_helper_resolver: &mut F,
) -> bool
where
    F: FnMut(&CompileForm, &[u8]) -> bool,
{
    // Macro expansion can use either numeric integers or binary atoms for
    // opcodes. SExp equality compares the corresponding CLVM atom values.
    if chialisp::compiler::prims::prims()
        .iter()
        .any(|(_, opcode)| opcode == name)
    {
        return true;
    }
    if let SExp::Atom(_, a) = name {
        scopes.functions.contains(name)
            // These callable operators are compiler builtins rather than CLVM
            // primitives (compiler::resolve::is_compiler_builtin).
            || a == b"com" || a == b"@"
            || prims.iter().any(|p| p == a)
            || imported_helper_resolver(frontend, a)
    } else {
        false
    }
}

// Add errors for unrecognized calls.
pub fn check_live_helper_calls(
    prims: &[Vec<u8>],
    scopes: &ParseScope,
    frontend: &CompileForm,
    exp: &BodyForm,
) -> Option<CompileErr> {
    let mut no_imported_helpers = |_frontend: &CompileForm, _name: &[u8]| false;
    check_live_helper_calls_with_imports(prims, scopes, frontend, exp, &mut no_imported_helpers)
}

fn check_live_helper_calls_with_imports<F>(
    prims: &[Vec<u8>],
    scopes: &ParseScope,
    frontend: &CompileForm,
    exp: &BodyForm,
    imported_helper_resolver: &mut F,
) -> Option<CompileErr>
where
    F: FnMut(&CompileForm, &[u8]) -> bool,
{
    match exp {
        BodyForm::Call(l, v, rest_args) => {
            if v.is_empty() {
                return Some(CompileErr(l.clone(), "Empty function call".to_string()));
            }

            // Try to make sense of the list head
            if let BodyForm::Value(s) = v[0].borrow() {
                if !find_function_in_scopes(prims, scopes, frontend, s, imported_helper_resolver) {
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
                if let Some(e) = check_live_helper_calls_with_imports(
                    prims,
                    scopes,
                    frontend,
                    b,
                    imported_helper_resolver,
                ) {
                    return Some(e);
                }
            }

            if let Some(tail) = rest_args {
                if let Some(e) = check_live_helper_calls_with_imports(
                    prims,
                    scopes,
                    frontend,
                    tail,
                    imported_helper_resolver,
                ) {
                    return Some(e);
                }
            }
        }
        BodyForm::Let(_kind, letdata) => {
            return check_live_helper_calls_with_imports(
                prims,
                scopes,
                frontend,
                letdata.body.borrow(),
                imported_helper_resolver,
            );
        }

        BodyForm::Lambda(ldata) => {
            return check_live_helper_calls_with_imports(
                prims,
                scopes,
                frontend,
                ldata.body.borrow(),
                imported_helper_resolver,
            );
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
    if uristring_comps.is_empty()
        || query_file_comps.is_empty()
        || query_file_comps.len() > uristring_comps.len()
    {
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
    true
}

pub fn combine_new_with_old_parse(
    uristring: &str,
    text: &[Rc<Vec<u8>>],
    parsed: &ParsedDoc,
    reparse: &ReparsedModule,
) -> ParsedDoc {
    let mut no_imported_helpers = |_frontend: &CompileForm, _name: &[u8]| false;
    combine_new_with_old_parse_with_imports(
        uristring,
        text,
        parsed,
        reparse,
        &mut no_imported_helpers,
    )
}

pub fn combine_new_with_old_parse_with_imports<F>(
    uristring: &str,
    text: &[Rc<Vec<u8>>],
    parsed: &ParsedDoc,
    reparse: &ReparsedModule,
    imported_helper_resolver: &mut F,
) -> ParsedDoc
where
    F: FnMut(&CompileForm, &[u8]) -> bool,
{
    let mut new_includes = parsed.includes.clone();
    let mut new_helpers = parsed.helpers.clone();
    let mut extracted_helpers = Vec::new();
    let mut exports = parsed.exports.clone();
    let mut to_remove = HashSet::new();
    let mut remove_names = HashSet::new();
    let mut hash_to_name = parsed.hash_to_name.clone();
    let mut out_errors: Vec<CompileErr> = reparse
        .errors
        .iter()
        .filter(|e| {
            let borrowed_file: &String = e.0.file.borrow();
            determine_same_path(uristring, borrowed_file)
        })
        .cloned()
        .collect();

    // Collect to-delete set.
    for (h, _) in new_helpers.iter() {
        if !reparse.unparsed.contains_key(h) && !reparse.helpers.contains_key(h) {
            if let Some(name) = parsed.hash_to_name.get(h) {
                remove_names.insert(name.clone());
            }
            to_remove.insert(h.clone());
        }
    }

    for (h, _) in exports.iter() {
        // Reparse models helpers and exports together as an enum.
        if !reparse.unparsed.contains_key(h) && !reparse.helpers.contains_key(h) {
            // Exports are given synthetic names that can also be removed.
            if let Some(name) = parsed.hash_to_name.get(h) {
                remove_names.insert(name.clone());
            }
            to_remove.insert(h.clone());
        }
    }

    // Ensure that we catch when an include form disappeared.
    for (h, _) in new_includes.iter() {
        if !reparse.unparsed.contains_key(h) && !reparse.includes.contains_key(h) {
            to_remove.insert(h.clone());
        }
    }

    for h in to_remove.iter() {
        // If we removed a namespace ref, clobber it from the import files.
        hash_to_name.remove(h);
        new_helpers.remove(h);
        exports.remove(h);
        new_includes.remove(h);
    }

    for (h, i) in reparse.includes.iter() {
        new_includes.insert(h.clone(), i.clone());
    }

    // Iterate new helpers.
    for (h, p) in reparse.helpers.iter() {
        match &p.parsed {
            Err(e) => {
                out_errors.push(e.clone());
            }
            Ok(ParsedForm::Helper(parsed_helper)) => {
                for ph in parsed_helper.iter() {
                    hash_to_name.insert(h.clone(), ph.name().clone());
                    extracted_helpers.push(ph.clone());
                    new_helpers.insert(h.clone(), p.clone());
                }
            }
            Ok(ParsedForm::ModuleExport(Export::MainProgram(p))) => {
                let name = b"__export__main".to_vec();
                hash_to_name.insert(h.clone(), name.clone());
                exports.insert(h.clone(), Export::MainProgram(p.clone()));
            }
            Ok(ParsedForm::ModuleExport(Export::Function(f))) => {
                let mut name = b"__export__".to_vec();
                name.append(&mut f.name.value.clone());
                hash_to_name.insert(h.clone(), name.clone());
                exports.insert(h.clone(), Export::Function(f.clone()));
            }
        }
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

    // Macro expansion may create declarations and expressions that cannot be
    // reconstructed by parsing individual source forms. Keep the source forms
    // for coloring, but use the compiler's expanded module for semantics.
    if let Some(module_semantics) = &reparse.module_semantics {
        out_errors.clear();
        match module_semantics {
            Ok(FrontendOutput::Module(compiled, module_exports)) => {
                new_compile = compiled.clone();
                remove_names.clear();
                exports.clear();
                for (i, export) in module_exports.iter().enumerate() {
                    let hash = Hash::new(&sha256tree_from_atom(format!("export:{i}").as_bytes()));
                    exports.insert(hash, export.clone());
                }
            }
            Ok(FrontendOutput::CompileForm(compiled)) => new_compile = compiled.clone(),
            Err(error) => out_errors.push(error.clone()),
        }
    }

    let compile_with_dead_helpers_removed = new_compile.remove_helpers(&remove_names);
    let scopes = recover_scopes(uristring, text, &new_compile);

    let valid_module_semantics = !matches!(reparse.module_semantics, Some(Err(_)));
    let validation_helpers = if !valid_module_semantics {
        Vec::new()
    } else if reparse.module_semantics.is_some() {
        // A module's compile-time helpers are legal in the preprocessor's
        // evaluator but need not be runtime functions. Validate the runtime
        // dependency closure of exports, as the compiler does.
        let mut live = Vec::new();
        let live_opts = Rc::new(chialisp::compiler::compiler::DefaultCompilerOpts::new(
            uristring,
        ))
        .set_frontend_check_live(true);
        for export in exports.values() {
            let root = match export {
                Export::MainProgram(p) => p.expr.clone(),
                Export::Function(f) => Rc::new(BodyForm::Value(SExp::Atom(
                    f.name.loc.clone().unwrap_or_else(|| f.loc.clone()),
                    f.name.value.clone(),
                ))),
            };
            live.extend(compute_live_helpers(
                live_opts.clone(),
                &compile_with_dead_helpers_removed.helpers,
                root,
            ));
        }
        live
    } else {
        compile_with_dead_helpers_removed.helpers.clone()
    };
    for h in validation_helpers.iter() {
        if let HelperForm::Defun(_, d) = h {
            if let Some(error) = check_live_helper_calls_with_imports(
                &PRIM_NAMES,
                &scopes,
                &compile_with_dead_helpers_removed,
                &d.body,
                imported_helper_resolver,
            ) {
                out_errors.push(error);
            }
        }
    }

    for export in exports.iter().filter(|_| valid_module_semantics) {
        if let Export::MainProgram(p) = &export.1 {
            if let Some(error) = check_live_helper_calls_with_imports(
                &PRIM_NAMES,
                &scopes,
                &compile_with_dead_helpers_removed,
                &p.expr,
                imported_helper_resolver,
            ) {
                out_errors.push(error);
            }
        }
    }

    // Check whether functions called in exp are live
    if valid_module_semantics {
        if let Some(error) = check_live_helper_calls_with_imports(
            &PRIM_NAMES,
            &scopes,
            &compile_with_dead_helpers_removed,
            &compile_with_dead_helpers_removed.exp,
            imported_helper_resolver,
        ) {
            out_errors.push(error);
        }
    }

    ParsedDoc {
        ignored: reparse.ignored,
        mod_kw: reparse.mod_kw.clone(),
        compiled: compile_with_dead_helpers_removed,
        exports,
        errors: out_errors,
        scopes,
        includes: new_includes,
        helpers: new_helpers,
        hash_to_name,
        exp: reparse.exp.clone(),
    }
}

#[cfg(test)]
mod module_preprocessing_tests {
    use super::*;
    use crate::lsp::parse::make_simple_ranges;
    use crate::lsp::patch::split_text;
    use chialisp::compiler::compiler::DefaultCompilerOpts;

    fn analyze(source: &str) -> ParsedDoc {
        let filename = "file:///module.clsp";
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
        combine_new_with_old_parse(filename, &text, &old, &reparsed)
    }

    #[test]
    fn module_macro_generates_a_callable_helper() {
        let parsed = analyze("(defmac define (NAME) (c (q . defun) (c NAME (q . (() 1)))))\n(define F)\n(export () (F))");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        assert!(parsed.compiled.helpers.iter().any(|h| h.name() == b"F"));
        assert_eq!(parsed.exports.len(), 1);
    }

    #[test]
    fn macro_tree_arguments_are_not_runtime_calls() {
        let parsed = analyze("(defmac ignore (TREE) 1)\n(export () (ignore (unknown_argument 2)))");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    }

    #[test]
    fn compile_time_only_helpers_use_the_preprocessor_environment() {
        let parsed =
            analyze("(defun fresh (X) (string->symbol (symbol->string X)))\n(export () 1)");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    }

    #[test]
    fn expanded_macro_calls_keep_source_tokens_and_argument_navigation() {
        use crate::interfaces::{EPrintWriter, FSFileReader};
        use crate::lsp::semtok::{build_semantic_tokens, DocumentProcessingDescription};
        use crate::lsp::{LSPServiceProvider, TK_MACRO_IDX, TK_PARAMETER_IDX};
        use std::collections::BTreeMap;
        let source = "(defmac ignore (TREE) 1)\n(export (A B) (ignore (A . B)))";
        let parsed = analyze(source);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let lines = split_text(source);
        let mut resolver = LSPServiceProvider::new(
            Rc::new(FSFileReader::default()),
            Rc::new(EPrintWriter::default()),
            true,
        );
        let mut definitions = BTreeMap::new();
        let tokens = build_semantic_tokens(
            &mut DocumentProcessingDescription {
                resolver: &mut resolver,
                prims: &PRIM_NAMES,
                log: Rc::new(EPrintWriter::default()),
                id: lsp_server::RequestId::from(1),
                uristring: "file:///module.clsp",
                lines: &lines,
            },
            &HashMap::new(),
            &mut definitions,
            &parsed,
        );
        let export_line = source.lines().nth(1).unwrap();
        let macro_col = export_line.find("ignore").unwrap() + 1;
        let arg_col = export_line.rfind('A').unwrap() + 1;
        assert!(tokens
            .iter()
            .any(|t| t.loc.line == 2 && t.loc.col == macro_col && t.token_type == TK_MACRO_IDX));
        assert!(tokens
            .iter()
            .any(|t| t.loc.line == 2 && t.loc.col == arg_col && t.token_type == TK_PARAMETER_IDX));
        assert!(definitions
            .iter()
            .any(|(token, definition)| token.loc.line == 2
                && token.loc.col == arg_col
                && definition.line == 2
                && definition.col == export_line.find('A').unwrap() + 1));
    }

    #[test]
    fn macro_generated_primitive_opcodes_are_accepted() {
        let parsed = analyze("(defmac pair (A B) (c 4 (c A (c B ()))))\n(export (A B) (pair A B))");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    }

    #[test]
    fn legacy_include_wrappers_validate_definitions_but_leave_macro_trees_opaque() {
        let error = validate_compiler_input("legacy.clinc", b"((defun broken (X) (mod)))")
            .expect_err("wrapped helper must be checked before compiler preprocessing");
        assert_eq!(error.1, "incomplete mod form");
        assert!(validate_compiler_input(
            "legacy.clinc",
            b"((defmac ignore (TREE) 1) (ignore (mod)))",
        )
        .is_ok());
        assert!(validate_compiler_input("legacy.clinc", b"((defun f (mod) mod))").is_ok());
    }

    #[test]
    fn incomplete_nested_mod_is_reported_without_panicking() {
        let parsed = analyze("(mod () (list (mod)))");
        assert!(
            parsed.errors.iter().any(|e| e.1 == "incomplete mod form"),
            "{:?}",
            parsed.errors
        );
        let helper = analyze("(defun F () (mod))\n(export () (F))");
        assert!(helper.errors.iter().any(|e| e.1 == "incomplete mod form"));
        assert!(analyze("(mod () (q . (mod)))").errors.is_empty());
        assert!(analyze("(mod () (defun F (mod) mod) (F 1))")
            .errors
            .is_empty());
    }

    #[test]
    fn unknown_runtime_calls_are_diagnosed_on_the_first_analysis() {
        let parsed = analyze("(export () (missing 1))");
        assert_eq!(parsed.errors.len(), 1, "{:?}", parsed.errors);
        assert!(parsed.errors[0].1.contains("missing"));
        assert_eq!(parsed.errors[0].0.line, 1);
    }
}
