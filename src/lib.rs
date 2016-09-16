#![recursion_limit="420"]
extern crate fnv;

mod interner;

mod tokens;

#[allow(dead_code)] //TODO: remove some day
pub mod ast;

mod tokenizer;
mod parser;
pub use parser::*;

mod printer;
pub use printer::PrettyPrinter;

#[cfg(test)]
mod tests;
