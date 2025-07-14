use std::collections::VecDeque;

use tokenize::{tokens::CharPosition, Token};

use super::{ParseError, ParseTokens};
use crate::Stmt;

impl ParseTokens for Stmt {
    type Node = Stmt;
    fn parse_tokens(
        tokens: &mut VecDeque<Token>,
        prev_pos: CharPosition,
    ) -> Result<(Stmt, CharPosition), ParseError> {
        todo!()
    }
}
