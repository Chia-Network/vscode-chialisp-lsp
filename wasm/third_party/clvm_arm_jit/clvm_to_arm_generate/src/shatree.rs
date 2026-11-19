use crate::sexp::{SExp, SExpValue};
use sha2::{Digest, Sha256};

fn collect_by_hash<T: SExp>(hash: &[u8], sexp: T, matches: &mut Vec<T>) -> Vec<u8> {
    if let SExpValue::Cons(left, right) = sexp.explode() {
        let hash_left = collect_by_hash(hash, left, matches);
        let hash_right = collect_by_hash(hash, right, matches);
        let mut hasher = Sha256::new();
        hasher.update([2]);
        hasher.update(hash_left);
        hasher.update(hash_right);
        let my_hash = hasher.finalize().to_vec();
        if my_hash == hash {
            matches.push(sexp);
        }
        my_hash
    } else {
        let the_hash = sexp.sha256tree();
        if the_hash == hash {
            matches.push(sexp);
        }
        the_hash
    }
}

pub fn find_all_by_hash<T: SExp>(hash: &[u8], sexp: T) -> Vec<T> {
    let mut matches = Vec::new();
    collect_by_hash(hash, sexp, &mut matches);
    matches
}
