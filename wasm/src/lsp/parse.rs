use std::collections::HashMap;
use std::rc::Rc;
use clvm_tools_rs::compiler::comptypes::{BodyForm, CompileErr, CompileForm};
use clvm_tools_rs::compiler::sexp::SExp;
use clvm_tools_rs::compiler::srcloc::Srcloc;

use crate::lsp::types::{
    IncludeData,
    ParseScope,
    ReparsedExp,
    ReparsedHelper,
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
    pub helpers: HashMap<Vec<u8>, ReparsedHelper>,
    // If present, the main expression in ReparsedExp form.
    pub exp: Option<ReparsedExp>,
    // Includes in the various files, indexed by included file.
    pub includes: HashMap<Vec<u8>, IncludeData>,
    // Index of hashed ranges (as Reparsed* data) to the names they bind.
    pub hash_to_name: HashMap<Vec<u8>, Vec<u8>>,
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
            errors: Default::default()
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

#[test]
fn test_doc_vec_byte_iter_0() {
    // vr vec Rc
    let vr = |s: &str| {
        let bv: Vec<u8> = s.as_bytes().to_vec();
        Rc::new(bv)
    };
    let text = vec![
        vr("(mod (X)"),
        vr("  (defun F (A) (* A 2))"),
        vr("  (F X)"),
        vr("  )")
    ];

    let expected_bytes =
        b"(mod (X)\n  (defun F (A) (* A 2))\n  (F X)\n  )\n";

    for (i,b) in DocVecByteIter::new(&text).enumerate() {
        assert_eq!(expected_bytes[i], b);
    }
}
