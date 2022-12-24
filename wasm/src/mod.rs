#[macro_use]
extern crate lazy_static;

#[cfg(test)]
#[macro_use]
extern crate indoc;

pub mod api;
pub mod lsp;

#[cfg(test)]
mod tests;
