use clvmr::{Allocator, NodePtr, SExp};
use std::rc::Rc;

use crate::ir::repr::IRRepr;
use crate::ir::writer::IROutputIterator;

fn is_printable_string(v: &[u8]) -> bool {
    v.iter().all(|ch| *ch >= b' ' && *ch < 127)
}

fn has_oversized_sign_extension(data: &[u8]) -> bool {
    // Can't have an extra sign extension if the number is too short.
    // With the exception of 0.
    if data.len() < 2 {
        return data.len() == 1 && data[0] == 0;
    }

    if data[0] == 0 {
        // This is a canonical value.  The opposite is non-canonical.
        // 0x0080 -> 128
        // 0x0000 -> 0x0000.  Non canonical because the second byte
        // wouldn't suggest sign extension so the first 0 is redundant.
        return data[1] & 0x80 == 0;
    } else if data[0] == 0xff {
        // This is a canonical value.  The opposite is non-canonical.
        // 0xff00 -> -256
        // 0xffff -> 0xffff.  Non canonical because the second byte
        // would suggest sign extension so the first 0xff is redundant.
        return data[1] & 0x80 != 0;
    }

    false
}

pub fn ir_for_atom(atom: &[u8]) -> IRRepr {
    if atom.len() == 0 {
        return IRRepr::Null;
    }
    if atom.len() > 2 {
        if is_printable_string(atom) {
            return IRRepr::Quotes(atom.to_vec());
        } else {
            return IRRepr::Hex(atom.to_vec());
        }
    } else {
        // Determine whether the bytes identity an integer in canonical form.
        // It's not canonical if there is oversized sign extension.
        if atom != &[0] && !has_oversized_sign_extension(atom) {
            return IRRepr::Int(atom.to_vec(), true);
        }
    }
    IRRepr::Hex(atom.to_vec())
}

/*
 * (2 2 (2) (2 3 4)) => (a 2 (a) (a 3 4))
 */
fn disassemble_to_ir(allocator: &Allocator, sexp: NodePtr) -> IRRepr {
    match allocator.sexp(sexp) {
        SExp::Pair(l, r) => {
            let v0 = disassemble_to_ir(allocator, l);
            let v1 = disassemble_to_ir(allocator, r);
            IRRepr::Cons(Rc::new(v0), Rc::new(v1))
        }

        SExp::Atom => {
            // sexp is the only node in scope.
            let atom = allocator.atom(sexp);
            let bytes = atom.as_ref().to_vec();
            ir_for_atom(&bytes)
        }
    }
}

pub fn disassemble(allocator: &Allocator, node: NodePtr) -> String {
    let mut result = Vec::new();
    for b in IROutputIterator::new(Rc::new(disassemble_to_ir(allocator, node))) {
        result.push(b);
    }
    result.join("")
}
