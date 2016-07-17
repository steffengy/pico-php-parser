#![feature(test)]
#![recursion_limit="420"]
extern crate test;
extern crate fnv;

mod interner;

mod tokens;

#[allow(dead_code)] //TODO: remove some day
mod ast;

mod tokenizer;
mod parser;
pub use parser::*;

#[cfg(tests)]
mod tests;
