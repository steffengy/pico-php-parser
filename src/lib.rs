#![feature(test)]
#![recursion_limit="420"]
extern crate test;

mod tokens;

#[allow(dead_code)] //TODO: remove some day
mod ast;

mod tokenizer;
mod parser;
pub use parser::*;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
    }
}
