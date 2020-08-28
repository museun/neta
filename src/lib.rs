// #![cfg_attr(debug_assertions, allow(dead_code, unused_variables))]

#[macro_use]
#[allow(dead_code)]
mod diag;

mod lexer;
pub use lexer::Lexer;
pub mod span;
pub mod token;

mod ast;

#[allow(dead_code)]
mod parser;

mod logger;

#[allow(unused_variables)]
mod printer;
mod visit;
