use std::rc::Rc;

#[derive(Debug)]
pub enum IRRepr {
    Cons(Rc<IRRepr>, Rc<IRRepr>),
    Null,
    Quotes(Vec<u8>),
    Int(Vec<u8>, bool),
    Hex(Vec<u8>),
    Octal(Vec<u8>),
    Binary(Vec<u8>),
    Symbol(String),
}
