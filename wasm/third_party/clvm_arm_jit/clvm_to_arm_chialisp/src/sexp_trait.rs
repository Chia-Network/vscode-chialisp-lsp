use std::rc::Rc;

use chialisp::classic::clvm::casts::bigint_to_bytes_clvm;
use chialisp::compiler::clvm::sha256tree;
use chialisp::compiler::sexp::{SExp, parse_sexp};
use chialisp::compiler::srcloc::Srcloc;
use chialisp::util::Number;

use clvm_to_arm_generate::sexp;
use clvm_to_arm_generate::sexp::{SExpValue, Until, bi_zero};

#[derive(Clone)]
pub struct RcSExp(pub Rc<SExp>);

impl std::fmt::Display for RcSExp {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        self.0.fmt(fmt)
    }
}

impl sexp::SExp for RcSExp {
    fn sha256tree(&self) -> Vec<u8> {
        sha256tree(self.0.clone())
    }

    fn to_number(&self) -> Option<Number> {
        self.0.as_ref().get_number().ok()
    }

    fn proper_list(&self) -> Option<Vec<Self>> {
        let mut res = Vec::new();
        let mut track = self.clone();

        loop {
            if track.nilp() {
                return Some(res);
            }

            match track.explode() {
                SExpValue::Cons(left, right) => {
                    res.push(left);
                    track = right;
                }
                _ => return None,
            }
        }
    }

    fn explode(&self) -> SExpValue<Self> {
        match self.0.as_ref() {
            SExp::Nil(_loc) => SExpValue::Nil,
            SExp::Cons(_loc, left, right) => SExpValue::Cons(RcSExp(left.clone()), RcSExp(right.clone())),
            SExp::Integer(_loc, i) => {
                if *i == bi_zero() {
                    return SExpValue::Nil;
                }
                SExpValue::Atom(bigint_to_bytes_clvm(i).data().clone())
            }
            SExp::QuotedString(_loc, _, bytes) | SExp::Atom(_loc, bytes) => {
                SExpValue::Atom(bytes.clone())
            }
        }
    }
}

#[derive(Clone)]
pub struct SrclocWrap(pub Srcloc);

impl std::fmt::Display for SrclocWrap {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        self.0.fmt(fmt)
    }
}

impl sexp::Srcloc for SrclocWrap {
    fn start(filename: &str) -> Self {
        SrclocWrap(Srcloc::start(filename))
    }
    fn filename(&self) -> String {
        self.0.file.to_string()
    }
    fn line(&self) -> usize {
        self.0.line
    }
    fn col(&self) -> usize {
        self.0.col
    }
    fn overlap(&self, other: &Self) -> bool {
        self.0.overlap(&other.0)
    }
    fn until(&self) -> Option<Until> {
        if let Some(u) = &self.0.until {
            return Some(Until {
                line: u.line as u32,
                col: u.col as u32,
            });
        }

        None
    }
}

impl sexp::HasSrcloc for RcSExp {
    type Srcloc = SrclocWrap;

    fn loc(&self) -> SrclocWrap {
        SrclocWrap(self.0.as_ref().loc())
    }
}

pub struct CreateChialispSExp;

impl sexp::CreateSExp<RcSExp> for CreateChialispSExp {
    fn atom(loc: SrclocWrap, bytes: &[u8]) -> RcSExp {
        RcSExp(Rc::new(SExp::Atom(loc.0, bytes.to_vec())))
    }

    fn cons(loc: SrclocWrap, a: RcSExp, b: RcSExp) -> RcSExp {
        RcSExp(Rc::new(SExp::Cons(loc.0, a.0, b.0)))
    }

    fn parse_sexp<I>(start: SrclocWrap, input: I) -> Result<Vec<RcSExp>, (SrclocWrap, String)>
    where
        I: Iterator<Item = u8>,
    {
        parse_sexp(start.0, input).map(|v| v.into_iter().map(RcSExp).collect()).map_err(|(s, e)| (SrclocWrap(s), e))
    }
}
