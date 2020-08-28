#![cfg_attr(debug_assertions, allow(dead_code, unused_variables))]

#[macro_use]
#[allow(dead_code)]
mod diag;

mod lexer;
pub use lexer::Lexer;
pub mod span;
pub mod token;

mod ast;
mod parser;
mod printer;
mod visit;

#[allow(dead_code)]
mod logger;

// macro_rules! pos {
//     () => {
//         concat!(file!(), ":", line!(), ":", column!())
//     };
// }
