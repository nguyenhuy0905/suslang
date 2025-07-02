use std::{error::Error, fmt::Display};

use super::*;
use tokenize::Token;

#[derive(Debug, Clone, PartialEq)]
pub enum ParserErrType {
    ExpectedToken { line: usize, pos: usize },
    UnexpectedToken(Token),
    Unended { line: usize, pos: usize },
}

impl Display for ParserErrType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl Error for ParserErrType {}

trait ParseToken {
    type Node;
    /// Given an [`Iterator`] of [`Token`]s, convert part of the iterator into
    /// a parse node.
    ///
    /// # Errors
    /// - If parsing fails, returns a [`ParseErrType`].
    fn parse_tokens<I: Iterator<Item = Token>>(
        tokens: I,
        start_ln: usize,
        start_pos: usize,
    ) -> Result<Self::Node, ParserErrType>;
}
