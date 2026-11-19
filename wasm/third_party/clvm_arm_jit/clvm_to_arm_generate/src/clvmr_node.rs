use sha2::{Digest, Sha256};
use std::cell::RefCell;
use std::fmt::Display;
use std::rc::Rc;

use crate::sexp::Number;

use crate::disassemble::disassemble;
use crate::sexp::bi_zero;
use clvmr::{Allocator, NodePtr};

#[derive(Clone)]
pub struct ClvmrAllocator {
    a: Rc<RefCell<Allocator>>,
}

impl ClvmrAllocator {
    pub fn with_allocator<F, R>(&self, f: F) -> R
    where
        F: FnOnce(&Allocator) -> R,
    {
        let ref_imu = (&*self.a).borrow();
        f(&ref_imu)
    }

    #[cfg(test)]
    pub fn with_allocator_mut<F, R>(&self, f: F) -> R
    where
        F: FnOnce(&mut Allocator) -> R,
    {
        let mut ref_mut = (&*self.a).borrow_mut();
        f(&mut ref_mut)
    }
}

impl Default for ClvmrAllocator {
    fn default() -> Self {
        ClvmrAllocator {
            a: Rc::new(RefCell::new(Allocator::new())),
        }
    }
}

#[derive(Clone)]
pub struct ClvmrWrapper {
    a: ClvmrAllocator,
    n: NodePtr,
}

impl ClvmrWrapper {
    pub fn with_allocator<F, R>(&self, f: F) -> R
    where
        F: FnOnce(&Allocator) -> R,
    {
        self.a.with_allocator(f)
    }

    #[cfg(test)]
    fn with_allocator_mut<F, R>(&self, f: F) -> R
    where
        F: FnOnce(&mut Allocator) -> R,
    {
        self.a.with_allocator_mut(f)
    }
}

impl Display for ClvmrWrapper {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        self.with_allocator(|a| {
            write!(formatter, "{}", disassemble(a, self.n))?;
            Ok(())
        })
    }
}

pub fn get_number(allocator: &Allocator, v: NodePtr) -> Option<Number> {
    if !matches!(allocator.sexp(v), clvmr::SExp::Atom) {
        return None;
    }

    let v = allocator.atom(v);
    let len = v.len();
    if len == 0 {
        Some(bi_zero())
    } else {
        Some(Number::from_signed_bytes_be(&v))
    }
}

pub fn proper_list(allocator: &Allocator, mut node: NodePtr) -> Option<Vec<NodePtr>> {
    let mut result = Vec::new();
    while let clvmr::SExp::Pair(h, e) = allocator.sexp(node) {
        result.push(h);
        node = e;
    }
    if allocator.atom(node).is_empty() {
        return Some(result);
    }

    None
}

pub fn sha256tree(allocator: &Allocator, sexp: NodePtr) -> Vec<u8> {
    match allocator.sexp(sexp) {
        clvmr::SExp::Pair(left, right) => {
            let hash_left = sha256tree(allocator, left);
            let hash_right = sha256tree(allocator, right);
            let mut hasher = Sha256::new();
            hasher.update([2]);
            hasher.update(hash_left);
            hasher.update(hash_right);
            hasher.finalize().to_vec()
        }
        _ => {
            let bytes = allocator.atom(sexp);
            let mut hasher = Sha256::new();
            hasher.update([1]);
            hasher.update(bytes);
            hasher.finalize().to_vec()
        }
    }
}
