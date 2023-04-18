#[macro_use]
extern crate lazy_static;

#[cfg(test)]
#[macro_use]
extern crate indoc;

pub mod api;
mod dbg;
mod interfaces;
pub mod lsp;

#[cfg(test)]
mod tests;
