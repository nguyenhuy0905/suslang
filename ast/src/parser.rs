use std::{collections::VecDeque, error::Error, fmt::Display};

use tokenize::{tokens::CharPosition, Token};

mod expr;

use crate::Expr;

pub trait ParseTokens {
    /// Given a [`VecDeque`] of [`Token`]s, convert part of the iterator into
    /// a parse node.
    ///
    /// # Note
    /// - If there is no previous token (meaning no available `prev_pos`), use
    ///   [`CharPosition`] `{line: 1, column: 1}`.
    ///
    /// # Errors
    /// - If parsing fails, returns a [`ParseErrType`].
    ///
    /// # Return
    /// - The parsed struct.
    /// - The position of the last token popped.
    fn parse_tokens(
        tokens: &mut VecDeque<Token>,
        prev_pos: CharPosition,
    ) -> Result<(Expr, CharPosition), ParseError>;
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParseError {
    ExpectedToken(CharPosition),
    UnexpectedToken(Token),
    Unended(CharPosition),
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

impl Error for ParseError {}
