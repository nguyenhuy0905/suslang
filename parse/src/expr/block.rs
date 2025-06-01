use std::collections::VecDeque;

use tokenize::{Token, TokenType};

use crate::{ParseError, Stmt};

use super::{ExprAst, ExprBoxWrap, ExprParse};

#[derive(Debug, Clone, PartialEq)]
pub struct BlockExpr {
    pub statements: Vec<Stmt>,
}

#[macro_export]
macro_rules! new_block_expr {
    () => {
        BlockExpr {
            statements: Vec::new(),
        }
    };
    ($first_it:expr $(,$it:expr)* $(,)?) => {
        BlockExpr {
            statements: vec![$first_it $(,$it)*]
        }
    };
}

impl ExprAst for BlockExpr {}

impl BlockExpr {
    /// Create a new `BlockExpr` from the input tokens.
    ///
    /// Because some constructs expect exactly a `BlockExpr`, having this
    /// alongside the usual [`ExprParse::parse`] is useful.
    ///
    /// # Parameters
    /// - Analogous to [`ExprParse::parse`].
    ///
    /// # Errors
    /// - If the very first token in `tokens` is not a
    ///   [`LCParen`](TokenType::LCParen) and there's no matching
    ///   [`RCParen`](TokenType::RCParen) at the end, it's an
    ///   [`UnexpectedToken`](ParseError::UnexpectedToken).
    /// - If `tokens` is empty, [`ExpectedToken`](ParseError::ExpectedToken) is
    ///   returned.
    /// - If parsing any statement in between the two curly brackets goes wrong,
    ///   the corresponding [`ParseError`] is percolated up.
    #[allow(clippy::missing_panics_doc)]
    pub fn new_from(
        tokens: &mut VecDeque<Token>,
        line: usize,
        pos: usize,
    ) -> Result<(Self, usize, usize), Option<ParseError>> {
        let (lc_line, lc_pos) = tokens
            .pop_front()
            .ok_or(ParseError::ExpectedToken { line, pos })
            .and_then(|tok| match tok.tok_typ {
                TokenType::LCParen => Ok((tok.line_number, tok.line_position)),
                _ => Err(ParseError::UnexpectedToken(tok)),
            })?;

        // Check whether the next token is a `RCParen` ("}"). If yes, return
        // Ok(true), if empty, return Err(ExpectedToken), otherwise Ok(false).
        let check_rc_paren = |tokens: &VecDeque<Token>,
                              line: usize,
                              pos: usize|
         -> Result<bool, ParseError> {
            match tokens.front().map(Token::token_type) {
                None => Err(ParseError::ExpectedToken { line, pos }),
                Some(&TokenType::RCParen) => Ok(true),
                Some(_) => Ok(false),
            }
        };

        let mut statements: Vec<Stmt> = Vec::new();
        let (mut while_ln, mut while_pos) = (lc_line, lc_pos);
        while !check_rc_paren(tokens, line, pos)? {
            let (push_stmt, stmt_ln, stmt_pos) =
                Stmt::parse(tokens, while_ln, while_pos)?;
            statements.push(push_stmt);
            (while_ln, while_pos) = (stmt_ln, stmt_pos);
        }
        // Unwrap possible because of `check_rc_paren` while-loop above,
        // which percolates an error up when `tokens` is empty.
        // And because of that same function, we can be sure the token we
        // popped out is a `RCParen`
        let (ret_tok, ret_ln, ret_pos) =
            tokens.pop_front().map(Token::bind).unwrap();
        debug_assert!(ret_tok == TokenType::RCParen);

        Ok((Self { statements }, ret_ln, ret_pos))
    }
}

impl ExprParse for BlockExpr {
    fn parse(
        tokens: &mut VecDeque<Token>,
        line: usize,
        pos: usize,
    ) -> Result<(ExprBoxWrap, usize, usize), Option<ParseError>> {
        Self::new_from(tokens, line, pos)
            .map(|(res, ln, pos)| (ExprBoxWrap::new(res), ln, pos))
    }
}
