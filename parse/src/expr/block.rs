use std::collections::{HashSet, VecDeque};

use tokenize::{Token, TokenType};

use crate::{ParseError, Stmt};

use super::{ExprAst, ExprBoxWrap, ExprParse};

/// Block expression
///
/// # Rule
/// \<block-expr\> ::= "{" \<stmt\> "}"
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
        while !check_rc_paren(tokens, while_ln, while_pos)? {
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

/// Declare a procedure
///
/// # Rule
/// \<proc-expr\> ::= "proc" "(" (ID ("," ID)*)? ","? ")" \<block-expr\>
#[derive(Debug, Clone, PartialEq)]
pub struct ProcExpr {
    pub idents: HashSet<String>,
    pub block: BlockExpr,
}

#[macro_export]
macro_rules! new_proc_expr {
    ((), $blk:expr) => {
        ProcExpr {
            idents: HashSet::new(),
            block: $blk,
        }
    };
    (($first_id:expr $(, $id:expr)*), $blk:expr) => {
        ProcExpr {
            idents: HashSet::from([String::from($first_id) $(, String::from($id))*]),
            block: $blk,
        }
    };
}

impl ExprAst for ProcExpr {}

impl ExprParse for ProcExpr {
    fn parse(
        tokens: &mut VecDeque<Token>,
        line: usize,
        pos: usize,
    ) -> Result<(ExprBoxWrap, usize, usize), Option<ParseError>> {
        // check for keyword "proc"
        let (proc_ln, proc_pos) = tokens
            .pop_front()
            .ok_or(Some(ParseError::ExpectedToken { line, pos }))
            .and_then(|tok| match tok.tok_typ {
                TokenType::Proc => Ok((tok.line_number, tok.line_position)),
                _ => Err(Some(ParseError::UnexpectedToken(tok))),
            })?;

        // which should be followed by left paren
        let (brace_ln, brace_pos) = tokens
            .pop_front()
            .ok_or(Some(ParseError::ExpectedToken {
                line: proc_ln,
                pos: proc_pos,
            }))
            .and_then(|tok| match tok.tok_typ {
                TokenType::LParen => Ok((tok.line_number, tok.line_position)),
                _ => Err(Some(ParseError::UnexpectedToken(tok))),
            })?;

        let (mut while_ln, mut while_pos) = (brace_ln, brace_pos);
        let mut idents: HashSet<String> = HashSet::new();
        // get the parameter list
        while !matches!(
            tokens.front().map(Token::token_type),
            Some(&TokenType::RParen),
        ) {
            // match the identifier
            let (id, id_ln, id_pos) = tokens
                .pop_front()
                .ok_or(Some(ParseError::ExpectedToken {
                    line: while_ln,
                    pos: while_pos,
                }))
                .and_then(|tok| match tok.tok_typ {
                    TokenType::Identifier(s) => {
                        Ok((s, tok.line_number, tok.line_position))
                    }
                    _ => Err(Some(ParseError::UnexpectedToken(tok))),
                })?;
            idents.insert(id);
            // see if there's a comma following the identifier.
            // If yes, remove it so that the next iteration doesn't return
            // error because of the comma. (UnexpectedToken)
            // Otherwise, it can be a right paren (which ends the parameter list)
            // or a random token which will error out in the next iteration.
            match tokens.front().map(Token::bind_ref) {
                Some((&TokenType::Comma, comma_ln, comma_pos)) => {
                    tokens.pop_front();
                    (while_ln, while_pos) = (comma_ln, comma_pos);
                }
                _ => {
                    (while_ln, while_pos) = (id_ln, id_pos);
                }
            }
        }
        // remove the closing paren then parse the block expr.
        let (typ, typ_ln, typ_pos) = tokens
            .pop_front()
            .ok_or(Some(ParseError::ExpectedToken {
                line: while_ln,
                pos: while_pos,
            }))
            .map(Token::bind)?;
        debug_assert!(matches!(typ, TokenType::RParen));

        let (block, ret_ln, ret_pos) =
            BlockExpr::new_from(tokens, typ_ln, typ_pos)?;
        Ok((ExprBoxWrap::new(Self { idents, block }), ret_ln, ret_pos))
    }
}
