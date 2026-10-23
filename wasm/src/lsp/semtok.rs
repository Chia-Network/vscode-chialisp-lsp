use std::borrow::Borrow;
use std::cmp::Ordering;
use std::collections::{BTreeMap, HashMap, HashSet};
use std::rc::Rc;

use lsp_server::{Message, RequestId, Response};
use lsp_types::{SemanticToken, SemanticTokens, SemanticTokensParams};

use crate::interfaces::ILogWriter;
use crate::lsp::completion::PRIM_NAMES;
use crate::lsp::parse::{
    add_bindings_to_set, add_sexp_bindings, recover_scopes, IncludedFileSpec, ParsedDoc,
};
use crate::lsp::patch::get_bytes;
use crate::lsp::positions::byte_to_utf16;
use crate::lsp::resolve::HelperResolver;
use crate::lsp::types::{
    DocPosition, DocRange, Hash, IncludeData, IncludeKind, LSPServiceProvider, ParsedForm,
    ReparsedExp, ReparsedHelper,
};
use crate::lsp::{
    TK_COMMENT_IDX, TK_DEFINITION_BIT, TK_FUNCTION_IDX, TK_KEYWORD_IDX, TK_MACRO_IDX,
    TK_NUMBER_IDX, TK_PARAMETER_IDX, TK_READONLY_BIT, TK_STRING_IDX, TK_VARIABLE_IDX,
};
use chialisp::compiler::clvm::sha256tree;
use chialisp::compiler::compiler::DefaultCompilerOpts;
use chialisp::compiler::comptypes::{
    BodyForm, CompileForm, Export, HelperForm, LetFormKind, ModuleImportSpec,
};
use chialisp::compiler::frontend::match_export_form;
use chialisp::compiler::sexp::{parse_sexp, SExp};
use chialisp::compiler::srcloc::Srcloc;

#[derive(Clone, Debug)]
pub struct SemanticTokenSortable {
    pub loc: Srcloc,
    pub token_type: u32,
    pub token_mod: u32,
}

impl PartialEq for SemanticTokenSortable {
    fn eq(&self, other: &SemanticTokenSortable) -> bool {
        self.loc.file == other.loc.file
            && self.loc.line == other.loc.line
            && self.loc.col == other.loc.col
    }
}

impl Eq for SemanticTokenSortable {}

#[allow(clippy::non_canonical_partial_ord_impl)]
impl PartialOrd for SemanticTokenSortable {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        let cf = self.loc.file.cmp(other.loc.file.borrow());
        if cf != Ordering::Equal {
            return Some(cf);
        }
        let lf = self.loc.line.cmp(&other.loc.line);
        if lf != Ordering::Equal {
            return Some(lf);
        }
        Some(self.loc.col.cmp(&other.loc.col))
    }
}

impl Ord for SemanticTokenSortable {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}

pub trait LSPSemtokRequestHandler {
    fn handle_semantic_tokens(
        &mut self,
        id: RequestId,
        params: &SemanticTokensParams,
    ) -> Result<Vec<Message>, String>;
}

fn collect_arg_tokens(
    collected_tokens: &mut Vec<SemanticTokenSortable>,
    argcollection: &mut HashMap<Vec<u8>, Srcloc>,
    args: Rc<SExp>,
) {
    match args.borrow() {
        SExp::Atom(l, a) => {
            argcollection.insert(a.clone(), l.clone());
            collected_tokens.push(SemanticTokenSortable {
                loc: l.clone(),
                token_type: TK_PARAMETER_IDX,
                token_mod: 1 << TK_DEFINITION_BIT,
            });
        }
        SExp::Cons(_, a, b) => {
            collect_arg_tokens(collected_tokens, argcollection, a.clone());
            collect_arg_tokens(collected_tokens, argcollection, b.clone());
        }
        _ => {}
    }
}

pub struct DocumentProcessingDescription<'a> {
    pub resolver: &'a mut dyn HelperResolver,
    pub prims: &'a [Vec<u8>],
    pub log: Rc<dyn ILogWriter>,
    pub id: RequestId,
    pub uristring: &'a str,
    pub lines: &'a [Rc<Vec<u8>>],
}

fn process_body_code(
    env: &mut DocumentProcessingDescription,
    collected_tokens: &mut Vec<SemanticTokenSortable>,
    gotodef: &mut BTreeMap<SemanticTokenSortable, Srcloc>,
    argcollection: &HashMap<Vec<u8>, Srcloc>,
    varcollection: &HashMap<Vec<u8>, Srcloc>,
    frontend: &CompileForm,
    body: Rc<BodyForm>,
) {
    match body.borrow() {
        BodyForm::Let(k, letdata) => {
            let mut bindings_vars = varcollection.clone();
            // assign bindings are mutually visible, unlike let's initializers.
            if k == &LetFormKind::Assign {
                for binding in &letdata.bindings {
                    let mut names = HashSet::new();
                    add_bindings_to_set(&mut names, binding);
                    for name in names {
                        if let SExp::Atom(loc, name) = name.as_ref() {
                            bindings_vars.insert(name.clone(), loc.clone());
                        }
                    }
                }
            }
            if let Some(kw) = &letdata.kw {
                collected_tokens.push(SemanticTokenSortable {
                    loc: kw.clone(),
                    token_type: TK_KEYWORD_IDX,
                    token_mod: 0,
                });
            }
            for b in letdata.bindings.iter() {
                let mut bindings = HashSet::new();
                add_bindings_to_set(&mut bindings, b);
                for item in bindings.iter() {
                    if let SExp::Atom(loc, _) = item.borrow() {
                        collected_tokens.push(SemanticTokenSortable {
                            loc: loc.clone(),
                            token_type: TK_VARIABLE_IDX,
                            token_mod: 1 << TK_DEFINITION_BIT | 1 << TK_READONLY_BIT,
                        });
                    }
                }
                if k == &LetFormKind::Sequential || k == &LetFormKind::Assign {
                    // Bindings above affect code below
                    process_body_code(
                        env,
                        collected_tokens,
                        gotodef,
                        argcollection,
                        &bindings_vars,
                        frontend,
                        b.body.clone(),
                    );
                } else {
                    process_body_code(
                        env,
                        collected_tokens,
                        gotodef,
                        argcollection,
                        varcollection,
                        frontend,
                        b.body.clone(),
                    )
                }
                let mut bset = HashSet::new();
                add_bindings_to_set(&mut bset, b);
                for b in bset.iter() {
                    if let SExp::Atom(l, n) = b.borrow() {
                        bindings_vars.insert(n.clone(), l.clone());
                    }
                }
            }
            process_body_code(
                env,
                collected_tokens,
                gotodef,
                argcollection,
                &bindings_vars,
                frontend,
                letdata.body.clone(),
            );
        }
        BodyForm::Lambda(ldata) => {
            let mut bindings_vars = varcollection.clone();
            if let Some(kw) = ldata.kw.as_ref() {
                collected_tokens.push(SemanticTokenSortable {
                    loc: kw.clone(),
                    token_type: TK_KEYWORD_IDX,
                    token_mod: 0,
                });
            }

            let mut bindings: HashSet<Rc<SExp>> = HashSet::new();
            add_sexp_bindings(&mut bindings, ldata.args.clone());
            for item in bindings.iter() {
                if let SExp::Atom(loc, n) = item.borrow() {
                    bindings_vars.insert(n.clone(), loc.clone());
                    collected_tokens.push(SemanticTokenSortable {
                        loc: loc.clone(),
                        token_type: TK_VARIABLE_IDX,
                        token_mod: 1 << TK_DEFINITION_BIT | 1 << TK_READONLY_BIT,
                    });
                }
            }

            process_body_code(
                env,
                collected_tokens,
                gotodef,
                argcollection,
                &bindings_vars,
                frontend,
                ldata.body.clone(),
            );
        }
        BodyForm::Quoted(SExp::Integer(l, _)) => {
            collected_tokens.push(SemanticTokenSortable {
                loc: l.clone(),
                token_type: TK_NUMBER_IDX,
                token_mod: 0,
            });
        }
        BodyForm::Quoted(SExp::QuotedString(l, _, _)) => {
            collected_tokens.push(SemanticTokenSortable {
                loc: l.clone(),
                token_type: TK_STRING_IDX,
                token_mod: 0,
            });
        }
        BodyForm::Value(SExp::Atom(l, a)) => {
            if let Some(argloc) = argcollection.get(a) {
                let t = SemanticTokenSortable {
                    loc: l.clone(),
                    token_type: TK_PARAMETER_IDX,
                    token_mod: 0,
                };
                collected_tokens.push(t.clone());
                gotodef.insert(t, argloc.clone());
            }
            if let Some(varloc) = varcollection.get(a) {
                let t = SemanticTokenSortable {
                    loc: l.clone(),
                    token_type: TK_VARIABLE_IDX,
                    token_mod: 0,
                };
                collected_tokens.push(t.clone());
                gotodef.insert(t, varloc.clone());
            }
        }
        BodyForm::Value(SExp::Integer(l, _)) => {
            collected_tokens.push(SemanticTokenSortable {
                loc: l.clone(),
                token_type: TK_NUMBER_IDX,
                token_mod: 0,
            });
        }
        BodyForm::Value(SExp::QuotedString(l, _, _)) => {
            collected_tokens.push(SemanticTokenSortable {
                loc: l.clone(),
                token_type: TK_STRING_IDX,
                token_mod: 0,
            });
        }
        BodyForm::Call(_, args, rest_args) => {
            if args.is_empty() {
                return;
            }

            let head: &BodyForm = args[0].borrow();
            if let BodyForm::Value(SExp::Atom(l, a)) = head {
                if let Some((call_token, location)) =
                    match env.resolver.resolve_helper_reference(frontend, a) {
                        Some(HelperForm::Defun(_, d)) => Some((
                            SemanticTokenSortable {
                                loc: l.clone(),
                                token_type: TK_FUNCTION_IDX,
                                token_mod: 0,
                            },
                            d.loc.clone(),
                        )),
                        Some(HelperForm::Defmacro(m)) => Some((
                            SemanticTokenSortable {
                                loc: l.clone(),
                                token_type: TK_MACRO_IDX,
                                token_mod: 0,
                            },
                            m.loc.clone(),
                        )),
                        _ => None,
                    }
                {
                    collected_tokens.push(call_token.clone());
                    gotodef.insert(call_token, location);
                } else {
                    for p in env.prims.iter() {
                        if p == a {
                            collected_tokens.push(SemanticTokenSortable {
                                loc: l.clone(),
                                token_type: TK_FUNCTION_IDX,
                                token_mod: 0,
                            });
                        }
                    }
                }
            }

            for a in args.iter().skip(1) {
                process_body_code(
                    env,
                    collected_tokens,
                    gotodef,
                    argcollection,
                    varcollection,
                    frontend,
                    a.clone(),
                );
            }

            // Handle the rest_args
            if let Some(tail) = rest_args {
                process_body_code(
                    env,
                    collected_tokens,
                    gotodef,
                    argcollection,
                    varcollection,
                    frontend,
                    tail.clone(),
                );
            }
        }
        BodyForm::Mod(l, m) => {
            // For each helper in the submod, process it.
            let mut helpers = HashMap::new();
            for h in m.helpers.iter() {
                let hash = sha256tree(h.to_sexp());
                helpers.insert(
                    Hash::new(&hash),
                    ReparsedHelper {
                        hash: Hash::new(&hash),
                        range: DocRange::from_srcloc(h.loc()),
                        parsed: Ok(ParsedForm::Helper(vec![h.clone()])),
                    },
                );
            }

            let borrowed_exp: &BodyForm = m.exp.borrow();
            let reparsed_exp = ReparsedExp {
                hash: Hash::new(&sha256tree(m.exp.to_sexp())),
                parsed: Ok(borrowed_exp.clone()),
            };

            // Process each include in the inner mod.
            let mut includes = HashMap::new();
            for i in m.include_forms.iter() {
                if !i.name.is_empty() && i.name[0] == b'*' {
                    continue;
                }

                let hashed = Hash::new(&sha256tree(i.to_sexp()));
                includes.insert(
                    hashed,
                    IncludedFileSpec::Include(IncludeData {
                        loc: i.kw.clone(),
                        name_loc: i.nl.clone(),
                        kw_loc: i.kw.clone(),
                        kind: IncludeKind::Include,
                        filename: i.name.clone(),
                        found: None,
                    }),
                );
            }

            let scopes = recover_scopes(env.uristring, env.lines, m);
            let synthesized_doc = ParsedDoc {
                ignored: false,
                mod_kw: Some(l.clone()),
                compiled: m.clone(),
                exports: HashMap::default(),
                scopes,
                helpers,
                exp: Some(reparsed_exp),
                includes,
                hash_to_name: HashMap::new(),
                // If we got here, there were no new errors.
                // Errors would have surfaced to the form containing the mod.
                errors: Vec::new(),
            };

            let mut new_tokens = build_semantic_tokens(
                env,
                &HashMap::new(), // Comments are global so the outer context
                // is handling them.
                gotodef,
                &synthesized_doc,
            );

            collected_tokens.append(&mut new_tokens);
        }
        _ => {}
    }
}

pub fn build_semantic_tokens(
    env: &mut DocumentProcessingDescription,
    comments: &HashMap<usize, usize>,
    goto_def: &mut BTreeMap<SemanticTokenSortable, Srcloc>,
    parsed: &ParsedDoc,
) -> Vec<SemanticTokenSortable> {
    let mut collected_tokens = Vec::new();
    let mut varcollection = HashMap::from([(b"@".to_vec(), parsed.compiled.exp.loc())]);
    let mut argcollection = HashMap::new();
    if let Some(modloc) = &parsed.mod_kw {
        collected_tokens.push(SemanticTokenSortable {
            loc: modloc.clone(),
            token_type: TK_KEYWORD_IDX,
            token_mod: 0,
        });
    }

    for (_, incl) in parsed.includes.iter() {
        match incl {
            IncludedFileSpec::Include(incl) => {
                collected_tokens.push(SemanticTokenSortable {
                    loc: incl.kw_loc.clone(),
                    token_type: TK_KEYWORD_IDX,
                    token_mod: 0,
                });
                collected_tokens.push(SemanticTokenSortable {
                    loc: incl.name_loc.clone(),
                    token_type: TK_STRING_IDX,
                    token_mod: 0,
                });
                match &incl.kind {
                    IncludeKind::Include => {}
                    IncludeKind::CompileFile(il) => {
                        collected_tokens.push(SemanticTokenSortable {
                            loc: il.clone(),
                            token_type: TK_VARIABLE_IDX,
                            token_mod: TK_DEFINITION_BIT,
                        });
                    }
                    IncludeKind::EmbedFile(il, kl) => {
                        collected_tokens.push(SemanticTokenSortable {
                            loc: il.clone(),
                            token_type: TK_VARIABLE_IDX,
                            token_mod: TK_DEFINITION_BIT,
                        });
                        collected_tokens.push(SemanticTokenSortable {
                            loc: kl.clone(),
                            token_type: TK_KEYWORD_IDX,
                            token_mod: 0,
                        });
                    }
                }
            }
            IncludedFileSpec::Import(_, nsref) => {
                // Handled in the helper personality.
                collected_tokens.push(SemanticTokenSortable {
                    loc: nsref.kw.clone(),
                    token_type: TK_KEYWORD_IDX,
                    token_mod: 0,
                });
                collected_tokens.push(SemanticTokenSortable {
                    loc: nsref.nl.clone(),
                    token_type: TK_VARIABLE_IDX,
                    token_mod: 0,
                });
                match &nsref.specification {
                    ModuleImportSpec::Qualified(q) => {
                        if let Some(_target) = &q.target {
                            collected_tokens.push(SemanticTokenSortable {
                                loc: q.kw.clone(),
                                token_type: TK_KEYWORD_IDX,
                                token_mod: 0,
                            });
                            collected_tokens.push(SemanticTokenSortable {
                                loc: q.nl.clone(),
                                token_type: TK_VARIABLE_IDX,
                                token_mod: 0,
                            });
                        }
                    }
                    ModuleImportSpec::Exposing(_l, e) => {
                        for h in e.iter() {
                            collected_tokens.push(SemanticTokenSortable {
                                loc: h.nl.clone(),
                                token_type: TK_VARIABLE_IDX,
                                token_mod: 0,
                            });
                        }
                    }
                    ModuleImportSpec::Hiding(_l, e) => {
                        for h in e.iter() {
                            collected_tokens.push(SemanticTokenSortable {
                                loc: h.nl.clone(),
                                token_type: TK_VARIABLE_IDX,
                                token_mod: 0,
                            });
                        }
                    }
                }
            }
        }
    }

    // Preprocessing consumes defmac declarations and expands calls. Their raw
    // source forms still describe what the editor displays, including macro
    // parameters and compile-time helper bodies.
    let mut source_helpers = parsed.compiled.helpers.clone();
    for helper in parsed.helpers.values() {
        if let Ok(ParsedForm::Helper(forms)) = &helper.parsed {
            for form in forms {
                if form.loc().file.as_str() == env.uristring {
                    source_helpers.push(form.clone());
                }
            }
        }
    }
    let source_frontend = parsed.compiled.replace_helpers(&source_helpers);
    for form in source_helpers.iter() {
        match form {
            HelperForm::Defnamespace(_) => {}
            HelperForm::Defnsref(_) => {}
            HelperForm::Defconstant(defc) => {
                if let Some(kw) = &defc.kw {
                    collected_tokens.push(SemanticTokenSortable {
                        loc: kw.clone(),
                        token_type: TK_KEYWORD_IDX,
                        token_mod: 0,
                    });
                }
                collected_tokens.push(SemanticTokenSortable {
                    loc: defc.nl.clone(),
                    token_type: TK_VARIABLE_IDX,
                    token_mod: (1 << TK_READONLY_BIT) | (1 << TK_DEFINITION_BIT),
                });
                varcollection.insert(defc.name.clone(), defc.nl.clone());
                process_body_code(
                    env,
                    &mut collected_tokens,
                    goto_def,
                    &HashMap::new(),
                    &varcollection,
                    &source_frontend,
                    defc.body.clone(),
                );
            }
            HelperForm::Defun(_, defun) => {
                let mut argcollection = HashMap::new();
                collected_tokens.push(SemanticTokenSortable {
                    loc: defun.nl.clone(),
                    token_type: TK_FUNCTION_IDX,
                    token_mod: 1 << TK_DEFINITION_BIT,
                });
                if let Some(kw) = &defun.kw {
                    collected_tokens.push(SemanticTokenSortable {
                        loc: kw.clone(),
                        token_type: TK_KEYWORD_IDX,
                        token_mod: 0,
                    });
                }
                collect_arg_tokens(
                    &mut collected_tokens,
                    &mut argcollection,
                    defun.args.clone(),
                );
                process_body_code(
                    env,
                    &mut collected_tokens,
                    goto_def,
                    &argcollection,
                    &varcollection,
                    &source_frontend,
                    defun.body.clone(),
                );
            }
            HelperForm::Defmacro(mac) => {
                let mut argcollection = HashMap::new();
                collected_tokens.push(SemanticTokenSortable {
                    loc: mac.nl.clone(),
                    token_type: TK_FUNCTION_IDX,
                    token_mod: 1 << TK_DEFINITION_BIT,
                });
                if let Some(kwl) = &mac.kw {
                    collected_tokens.push(SemanticTokenSortable {
                        loc: kwl.clone(),
                        token_type: TK_KEYWORD_IDX,
                        token_mod: 0,
                    });
                }
                collect_arg_tokens(&mut collected_tokens, &mut argcollection, mac.args.clone());
                process_body_code(
                    env,
                    &mut collected_tokens,
                    goto_def,
                    &argcollection,
                    &varcollection,
                    &source_frontend,
                    mac.program.exp.clone(),
                );
            }
        }
    }

    // Prefer an unexpanded export for coloring when it is a regular BodyForm.
    // Macro syntax such as dotted tree arguments instead uses the compiler's
    // expanded form below; a raw SExp pass restores its macro call token.
    let source_forms = parse_sexp(
        Srcloc::start(env.uristring),
        get_bytes(env.lines).into_iter(),
    )
    .unwrap_or_default();
    // Even when a macro accepts a dotted tree that is not a BodyForm, its
    // export arguments still bind names in that source tree.
    for form in &source_forms {
        if let Some(parts) = form.proper_list() {
            if parts.len() == 3 && matches!(&parts[0], SExp::Atom(_, name) if name == b"export") {
                collect_arg_tokens(
                    &mut collected_tokens,
                    &mut argcollection,
                    Rc::new(parts[1].clone()),
                );
            }
        }
    }
    let source_opts = Rc::new(DefaultCompilerOpts::new(env.uristring));
    let mut source_exports = Vec::new();
    for form in &source_forms {
        if let Ok(Some(export)) = match_export_form(source_opts.clone(), form.clone()) {
            source_exports.push(export);
        }
    }
    for export in parsed.exports.values() {
        if !source_exports.iter().any(|source| match (source, export) {
            (Export::MainProgram(a), Export::MainProgram(b)) => a.loc == b.loc,
            (Export::Function(a), Export::Function(b)) => a.loc == b.loc,
            _ => false,
        }) {
            source_exports.push(export.clone());
        }
    }
    for export in source_exports.iter() {
        match export {
            Export::MainProgram(p) => {
                collect_arg_tokens(&mut collected_tokens, &mut argcollection, p.args.clone());
                if let Some(kw_loc) = p.kw_loc.as_ref() {
                    collected_tokens.push(SemanticTokenSortable {
                        loc: kw_loc.clone(),
                        token_type: TK_KEYWORD_IDX,
                        token_mod: 0,
                    });
                }
                process_body_code(
                    env,
                    &mut collected_tokens,
                    goto_def,
                    &argcollection,
                    &varcollection,
                    &CompileForm {
                        loc: p.loc.clone(),
                        exp: p.expr.clone(),
                        ..source_frontend.clone()
                    },
                    p.expr.clone(),
                );
            }
            Export::Function(f) => {
                for (kind, loc) in [
                    (TK_KEYWORD_IDX, f.kw_loc.as_ref()),
                    (TK_KEYWORD_IDX, f.as_loc.as_ref()),
                    (TK_VARIABLE_IDX, f.name.loc.as_ref()),
                    (
                        TK_VARIABLE_IDX,
                        f.as_name.as_ref().and_then(|n| n.loc.as_ref()),
                    ),
                ]
                .iter()
                .filter_map(|(k, l)| l.map(|l| (k, l)))
                {
                    collected_tokens.push(SemanticTokenSortable {
                        loc: loc.clone(),
                        token_type: *kind,
                        token_mod: 0,
                    })
                }
            }
        }
    }

    collect_arg_tokens(
        &mut collected_tokens,
        &mut argcollection,
        parsed.compiled.args.clone(),
    );

    process_body_code(
        env,
        &mut collected_tokens,
        goto_def,
        &argcollection,
        &varcollection,
        &source_frontend,
        parsed.compiled.exp.clone(),
    );

    fn restore_macro_calls(
        env: &mut DocumentProcessingDescription,
        frontend: &CompileForm,
        form: &SExp,
        args: &HashMap<Vec<u8>, Srcloc>,
        tokens: &mut Vec<SemanticTokenSortable>,
        goto_def: &mut BTreeMap<SemanticTokenSortable, Srcloc>,
    ) {
        if let SExp::Cons(_, head, tail) = form {
            if let SExp::Atom(loc, name) = head.as_ref() {
                if name == b"q" || name == b"quote" {
                    return;
                }
                if let Some(HelperForm::Defmacro(mac)) =
                    env.resolver.resolve_helper_reference(frontend, name)
                {
                    let token = SemanticTokenSortable {
                        loc: loc.clone(),
                        token_type: TK_MACRO_IDX,
                        token_mod: 0,
                    };
                    if !tokens.iter().any(|t| t.loc == *loc) {
                        tokens.push(token.clone());
                    }
                    goto_def.insert(token, mac.nl.clone());
                }
            }
            restore_macro_calls(env, frontend, head, args, tokens, goto_def);
            // The cdr is an argument list, not another call expression.
            let mut rest = tail.as_ref();
            while let SExp::Cons(_, argument, next) = rest {
                restore_macro_calls(env, frontend, argument, args, tokens, goto_def);
                rest = next.as_ref();
            }
            if !matches!(rest, SExp::Nil(_)) {
                restore_macro_calls(env, frontend, rest, args, tokens, goto_def);
            }
        } else if let SExp::Atom(loc, name) = form {
            if let Some(definition) = args.get(name) {
                if !tokens.iter().any(|t| t.loc == *loc) {
                    let token = SemanticTokenSortable {
                        loc: loc.clone(),
                        token_type: TK_PARAMETER_IDX,
                        token_mod: 0,
                    };
                    tokens.push(token.clone());
                    goto_def.insert(token, definition.clone());
                }
            }
        }
    }
    for form in &source_forms {
        restore_macro_calls(
            env,
            &source_frontend,
            form,
            &argcollection,
            &mut collected_tokens,
            goto_def,
        );
    }

    for (l, c) in comments.iter() {
        collected_tokens.push(SemanticTokenSortable {
            loc: DocRange {
                start: DocPosition {
                    line: *l as u32,
                    character: *c as u32,
                },
                end: DocPosition {
                    line: *l as u32,
                    character: env.lines[*l].len() as u32,
                },
            }
            .to_srcloc(env.uristring),
            token_type: TK_COMMENT_IDX,
            token_mod: 0,
        });
    }

    collected_tokens.retain(|t| {
        let borrowed: &String = t.loc.file.borrow();
        borrowed == env.uristring
    });

    collected_tokens
}

fn do_semantic_tokens(
    env: &mut DocumentProcessingDescription,
    comments: &HashMap<usize, usize>,
    goto_def: &mut BTreeMap<SemanticTokenSortable, Srcloc>,
    parsed: &ParsedDoc,
) -> Response {
    let mut collected_tokens = build_semantic_tokens(env, comments, goto_def, parsed);

    collected_tokens.sort();

    let mut result_tokens = SemanticTokens {
        result_id: None,
        data: Vec::new(),
    };

    // Compiler locations count UTF-8 bytes. Convert only at the protocol
    // boundary, and split multiline strings because clients need not support
    // semantic tokens that span lines.
    let mut encoded = Vec::new();
    for t in collected_tokens.iter() {
        let first_line = t.loc.line.saturating_sub(1);
        let end = t.loc.ending();
        let last_line = end.line.saturating_sub(1);
        for line_number in first_line..=last_line.min(env.lines.len().saturating_sub(1)) {
            let Some(line) = env.lines.get(line_number) else {
                continue;
            };
            let start_byte = if line_number == first_line {
                t.loc.col.saturating_sub(1)
            } else {
                0
            };
            let end_byte = if line_number == last_line {
                if t.loc.until.is_some() {
                    end.col.saturating_sub(1)
                } else {
                    start_byte + 1
                }
            } else {
                line.len()
            };
            let start = byte_to_utf16(line, start_byte as u32);
            let finish = byte_to_utf16(line, end_byte as u32);
            if finish > start {
                encoded.push((
                    line_number as u32,
                    start,
                    finish - start,
                    t.token_type,
                    t.token_mod,
                ));
            }
        }
    }
    encoded.sort_by_key(|(line, start, _, _, _)| (*line, *start));
    let mut last_position = None;
    for (line, start, length, token_type, token_modifiers_bitset) in encoded {
        if last_position == Some((line, start)) {
            continue;
        }
        let (last_line, last_start) = last_position.unwrap_or((0, 0));
        result_tokens.data.push(SemanticToken {
            delta_line: line - last_line,
            delta_start: if line == last_line {
                start - last_start
            } else {
                start
            },
            length,
            token_type,
            token_modifiers_bitset,
        });
        last_position = Some((line, start));
    }

    Response {
        id: env.id.clone(),
        error: None,
        result: Some(serde_json::to_value(result_tokens).unwrap()),
    }
}

impl LSPSemtokRequestHandler for LSPServiceProvider {
    fn handle_semantic_tokens(
        &mut self,
        id: RequestId,
        params: &SemanticTokensParams,
    ) -> Result<Vec<Message>, String> {
        let uristring = params.text_document.uri.to_string();
        self.parse_document_and_store_errors(&uristring);
        let mut res = Vec::new();

        if let (Some(doc), Some(frontend)) = (self.get_doc(&uristring), self.get_parsed(&uristring))
        {
            let mut our_goto_defs = BTreeMap::new();
            let log = self.log.clone();
            let resp = do_semantic_tokens(
                &mut DocumentProcessingDescription {
                    resolver: self,
                    prims: &PRIM_NAMES,
                    log,
                    id: id.clone(),
                    uristring: &uristring,
                    lines: &doc.text,
                },
                &doc.comments,
                &mut our_goto_defs,
                &frontend,
            );
            self.goto_defs.insert(uristring.clone(), our_goto_defs);
            res.push(Message::Response(resp));
        }
        if res.is_empty() {
            res.push(Message::Response(Response::new_ok(
                id,
                serde_json::json!({"data": []}),
            )));
        }

        res.append(&mut self.produce_error_list());

        Ok(res)
    }
}

impl LSPServiceProvider {
    pub fn ensure_definition_index(&mut self, uri: &str) {
        self.parse_document_and_store_errors(uri);
        if self.goto_defs.contains_key(uri) {
            return;
        }
        if let (Some(doc), Some(parsed)) = (self.get_doc(uri), self.get_parsed(uri)) {
            let mut definitions = BTreeMap::new();
            let log = self.log.clone();
            build_semantic_tokens(
                &mut DocumentProcessingDescription {
                    resolver: self,
                    prims: &PRIM_NAMES,
                    log,
                    id: RequestId::from(0),
                    uristring: uri,
                    lines: &doc.text,
                },
                &doc.comments,
                &mut definitions,
                &parsed,
            );
            self.goto_defs.insert(uri.to_owned(), definitions);
        }
    }
}
