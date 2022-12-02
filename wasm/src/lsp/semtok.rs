use std::borrow::Borrow;
use std::cmp::Ordering;
use std::collections::{BTreeMap, HashMap};
use std::rc::Rc;

use lsp_server::{Message, RequestId, Response};
use lsp_types::{SemanticToken, SemanticTokens, SemanticTokensParams};

use clvm_tools_rs::compiler::clvm::sha256tree;
use clvm_tools_rs::compiler::comptypes::{BodyForm, CompileForm, HelperForm, LetFormKind};
use crate::lsp::completion::PRIM_NAMES;
use crate::lsp::parse::{recover_scopes, IncludeData, IncludeKind, ParsedDoc};
use crate::lsp::reparse::{ReparsedExp, ReparsedHelper};
use crate::lsp::types::{DocPosition, DocRange, ILogWriter, LSPServiceProvider};
use crate::lsp::{
    TK_COMMENT_IDX, TK_DEFINITION_BIT, TK_FUNCTION_IDX, TK_KEYWORD_IDX, TK_MACRO_IDX,
    TK_NUMBER_IDX, TK_PARAMETER_IDX, TK_READONLY_BIT, TK_STRING_IDX, TK_VARIABLE_IDX,
};
use clvm_tools_rs::compiler::sexp::SExp;
use clvm_tools_rs::compiler::srcloc::Srcloc;

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

fn find_call_token(
    prims: &[Vec<u8>],
    frontend: &CompileForm,
    l: &Srcloc,
    name: &Vec<u8>,
) -> Option<(SemanticTokenSortable, Srcloc)> {
    for f in frontend.helpers.iter() {
        match f {
            HelperForm::Defun(_inline, defun) => {
                if &defun.name == name {
                    let st = SemanticTokenSortable {
                        loc: l.clone(),
                        token_type: TK_FUNCTION_IDX,
                        token_mod: 0,
                    };
                    return Some((st, defun.nl.clone()));
                }
            }
            HelperForm::Defmacro(mac) => {
                if &mac.name == name {
                    let st = SemanticTokenSortable {
                        loc: l.clone(),
                        token_type: TK_MACRO_IDX,
                        token_mod: 0,
                    };
                    return Some((st, mac.nl.clone()));
                }
            }
            _ => {}
        }
    }

    for p in prims.iter() {
        if p == name {
            return Some((
                SemanticTokenSortable {
                    loc: l.clone(),
                    token_type: TK_FUNCTION_IDX,
                    token_mod: 0,
                },
                l.clone(),
            ));
        }
    }

    None
}

pub struct DocumentProcessingDescription<'a> {
    pub prims: &'a [Vec<u8>],
    pub log: Rc<dyn ILogWriter>,
    pub id: RequestId,
    pub uristring: &'a str,
    pub lines: &'a [Rc<Vec<u8>>],
}

fn process_body_code(
    env: &DocumentProcessingDescription,
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
            if let Some(kw) = &letdata.kw {
                collected_tokens.push(SemanticTokenSortable {
                    loc: kw.clone(),
                    token_type: TK_KEYWORD_IDX,
                    token_mod: 0,
                });
            }
            for b in letdata.bindings.iter() {
                collected_tokens.push(SemanticTokenSortable {
                    loc: b.nl.clone(),
                    token_type: TK_VARIABLE_IDX,
                    token_mod: 1 << TK_DEFINITION_BIT | 1 << TK_READONLY_BIT,
                });
                if k == &LetFormKind::Sequential {
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
                bindings_vars.insert(b.name.clone(), b.nl.clone());
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
        BodyForm::Call(_, args) => {
            if args.is_empty() {
                return;
            }

            let head: &BodyForm = args[0].borrow();
            if let BodyForm::Value(SExp::Atom(l, a)) = head {
                if let Some((call_token, location)) = find_call_token(env.prims, frontend, l, a) {
                    collected_tokens.push(call_token.clone());
                    gotodef.insert(call_token, location);
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
        }
        BodyForm::Mod(l, m) => {
            // For each helper in the submod, process it.
            let mut helpers = HashMap::new();
            for h in m.helpers.iter() {
                helpers.insert(
                    h.name().clone(),
                    ReparsedHelper {
                        hash: sha256tree(h.to_sexp()),
                        range: DocRange::from_srcloc(h.loc()),
                        parsed: Ok(h.clone()),
                    },
                );
            }

            let borrowed_exp: &BodyForm = m.exp.borrow();
            let reparsed_exp = ReparsedExp {
                hash: sha256tree(m.exp.to_sexp()),
                parsed: Ok(borrowed_exp.clone()),
            };

            // Process each include in the inner mod.
            let mut includes = HashMap::new();
            for i in m.include_forms.iter() {
                if !i.name.is_empty() && i.name[0] == b'*' {
                    continue;
                }

                let hashed = sha256tree(i.to_sexp());
                includes.insert(
                    hashed,
                    IncludeData {
                        loc: i.kw.clone(),
                        nl: i.nl.clone(),
                        kw: i.kw.clone(),
                        kind: IncludeKind::Include,
                        filename: i.name.clone(),
                        found: None
                    },
                );
            }

            let scopes = recover_scopes(env.uristring, env.lines, m);
            let synthesized_doc = ParsedDoc {
                ignored: false,
                mod_kw: Some(l.clone()),
                compiled: m.clone(),
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
    env: &DocumentProcessingDescription,
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
        collected_tokens.push(SemanticTokenSortable {
            loc: incl.kw.clone(),
            token_type: TK_KEYWORD_IDX,
            token_mod: 0,
        });
        collected_tokens.push(SemanticTokenSortable {
            loc: incl.nl.clone(),
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

    for form in parsed.compiled.helpers.iter() {
        match form {
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
                    &parsed.compiled,
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
                    &parsed.compiled,
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
                    &parsed.compiled,
                    mac.program.exp.clone(),
                );
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
        &parsed.compiled,
        parsed.compiled.exp.clone(),
    );

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
    env: &DocumentProcessingDescription,
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

    let mut last_row = 1;
    let mut last_col = 1;

    for t in collected_tokens.iter() {
        if t.loc.line < last_row || (t.loc.line == last_row && t.loc.col <= last_col) {
            continue;
        }
        if t.loc.line != last_row {
            last_col = 1;
        }
        result_tokens.data.push(SemanticToken {
            delta_line: (t.loc.line - last_row) as u32,
            delta_start: (t.loc.col - last_col) as u32,
            length: t.loc.len().unwrap_or(1) as u32,
            token_type: t.token_type,
            token_modifiers_bitset: t.token_mod,
        });
        last_row = t.loc.line;
        last_col = t.loc.col;
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
        let mut res = self.parse_document_and_output_errors(&uristring);

        if let (Some(doc), Some(frontend)) = (self.get_doc(&uristring), self.get_parsed(&uristring))
        {
            let mut our_goto_defs = BTreeMap::new();
            let resp = do_semantic_tokens(
                &DocumentProcessingDescription {
                    prims: &PRIM_NAMES,
                    log: self.log.clone(),
                    id,
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

        Ok(res)
    }
}
