pub mod expr;
pub mod stmt;
pub use expr::*;
use std::any::Any;
use std::collections::VecDeque;
use std::fmt::Debug;
pub use stmt::*;
use tokenize::Token;

#[derive(Debug, PartialEq)]
pub enum ParseError {
    UnexpectedToken(Token),
    ExpectedToken {
        line: usize,
        pos: usize,
    },
    UnclosedParen {
        line: usize,
        pos: usize,
    },
    UnendedStmt {
        line: usize,
        pos: usize,
    },
    SymbolAlreadyExists {
        name: String,
        line: usize,
        pos: usize,
    },
}
