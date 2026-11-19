use std::fmt::{Debug, Display};

use num_bigint::{BigInt, ToBigInt};

pub fn u8_from_number(v: &Number) -> Vec<u8> {
    v.to_signed_bytes_be()
}

pub type Number = BigInt;

pub fn bi_zero() -> BigInt {
    BigInt::default()
}

pub fn bi_one() -> BigInt {
    1_u32.to_bigint().unwrap()
}

// Traits for varying the type of CLVM expressions.
#[derive(Clone, Debug)]
pub enum SExpValue<T: SExp> {
    Nil,
    Atom(Vec<u8>),
    Cons(T, T),
}

pub trait SExp: Clone + Display {
    fn to_number(&self) -> Option<Number>;
    fn proper_list(&self) -> Option<Vec<Self>>;
    fn sha256tree(&self) -> Vec<u8>;
    fn explode(&self) -> SExpValue<Self>;

    fn nilp(&self) -> bool {
        matches!(self.atom_bytes::<Self>(), Some(bytes) if bytes.is_empty())
    }

    fn atom_bytes<T: SExp>(&self) -> Option<Vec<u8>> {
        match self.explode() {
            SExpValue::Cons(_, _) => None,
            SExpValue::Nil => Some(Vec::new()),
            SExpValue::Atom(bytes) => Some(bytes),
        }
    }
}

pub struct Until {
    pub line: u32,
    pub col: u32,
}

pub trait Srcloc: Clone + Display {
    fn start(filename: &str) -> Self;
    fn filename(&self) -> String;
    fn line(&self) -> usize;
    fn col(&self) -> usize;
    fn overlap(&self, other: &Self) -> bool;
    fn until(&self) -> Option<Until>;
}

pub trait HasSrcloc {
    type Srcloc: Srcloc;

    fn loc(&self) -> Self::Srcloc;
}

pub trait CreateSExp<S: SExp + HasSrcloc> {
    fn atom(loc: S::Srcloc, bytes: &[u8]) -> S;
    fn cons(loc: S::Srcloc, a: S, b: S) -> S;

    fn parse_sexp<I>(start: S::Srcloc, input: I) -> Result<Vec<S>, (S::Srcloc, String)>
    where
        I: Iterator<Item = u8>;
}

pub fn dequote<T: SExp>(sexp: T) -> Option<T> {
    match sexp.explode() {
        SExpValue::Cons(left, right) => match left.explode() {
            SExpValue::Atom(atom) if atom == b"\x01" => Some(right),
            _ => None,
        },
        _ => None,
    }
}

pub fn truthy<T: SExp>(sexp: T) -> bool {
    !sexp.nilp()
}

pub fn is_atom<T: SExp>(sexp: T) -> Option<Vec<u8>> {
    sexp.atom_bytes::<T>()
}

pub fn is_wrapped_atom<T: SExp>(sexp: T) -> Option<(T, Vec<u8>)> {
    match sexp.explode() {
        SExpValue::Cons(left, right) => {
            let atom = match left.explode() {
                SExpValue::Atom(atom) => atom,
                _ => return None,
            };
            if truthy(right) {
                None
            } else {
                Some((left.clone(), atom))
            }
        }
        _ => None,
    }
}
