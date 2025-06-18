use std::collections::{HashSet, VecDeque};

use tokenize::{Token, TokenType};

use crate::{Expr, ParseError, Stmt};

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
    ) -> Result<(Self, usize, usize), ParseError> {
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
            .map_err(Some)
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
        if !matches!(
            tokens.front().map(Token::token_type),
            Some(&TokenType::RParen),
        ) {
            loop {
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
                // There should be a next token or it's an `ExpectedToken`.
                // If yes, remove it so that the next iteration doesn't return
                // error because of the comma. (UnexpectedToken)
                // If it's a RParen instead, done with the loop.
                // Otherwise, it's an `UnexpectedToken`.
                match tokens.front().map(Token::bind_ref) {
                    Some((&TokenType::Comma, comma_ln, comma_pos)) => {
                        tokens.pop_front();
                        (while_ln, while_pos) = (comma_ln, comma_pos);
                    }
                    Some((&TokenType::RParen, ..)) => {
                        (while_ln, while_pos) = (id_ln, id_pos);
                        break;
                    }
                    Some(_) => {
                        return Err(Some(ParseError::UnexpectedToken(
                            tokens.pop_front().unwrap(),
                        )));
                    }
                    None => {
                        return Err(Some(ParseError::ExpectedToken {
                            line: id_ln,
                            pos: id_pos,
                        }));
                    }
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

/// If-elif-else expression
///
/// # Rule
///
/// \<if-expr\> ::= \<if-branch\> \<elif-branch\>* \<else-branch\>?
///
/// # Note
///
/// # See also
/// [`IfBranch`]
/// [`ElifBranch`]
/// [`ElseBranch`]
/// [`Expr`](super::Expr)
#[derive(Debug, Clone, PartialEq)]
pub struct IfExpr {
    pub if_branch: IfBranch,
    pub else_branch: Option<ElseBranch>,
    pub elif_branches: Vec<ElifBranch>,
}

#[macro_export]
macro_rules! new_if_expr {
    ($if_branch:expr $(, [$($elif_branch:expr,)+])?) => {
        IfExpr {
            if_branch: $if_branch,
            else_branch: None,
            elif_branches: vec![$($($elif_branch, )+)?],
        }
    };
    // need to be placed like so, otherwise it screams ambiguity
    ($if_branch:expr, $else_branch:expr $(, [$($elif_branch:expr,)+])?) => {
        IfExpr {
            if_branch: $if_branch,
            else_branch: Some($else_branch),
            elif_branches: vec![$($($elif_branch, )+)?],
        }
    };
}

impl ExprAst for IfExpr {}

impl ExprParse for IfExpr {
    fn parse(
        tokens: &mut VecDeque<Token>,
        line: usize,
        pos: usize,
    ) -> Result<(ExprBoxWrap, usize, usize), Option<ParseError>> {
        // the first item must be an `IfBranch`
        let (if_branch, if_ln, if_pos) = IfBranch::new_from(tokens, line, pos)?;
        let mut elif_branches: Vec<ElifBranch> = Vec::new();
        let (mut elif_ln, mut elif_pos) = (if_ln, if_pos);
        // which can be followed by any number of `ElifBranch`es
        while tokens
            .front()
            .is_some_and(|tok| tok.token_type() == &TokenType::Elif)
        {
            let (next_elif, next_elif_ln, next_elif_pos) =
                ElifBranch::new_from(tokens, elif_ln, elif_pos)?;
            elif_branches.push(next_elif);
            (elif_ln, elif_pos) = (next_elif_ln, next_elif_pos);
        }
        // and can end with or without `ElseBranch`
        if tokens
            .front()
            .is_some_and(|tok| tok.token_type() == &TokenType::Else)
        {
            let (else_branch, ret_ln, ret_pos) =
                ElseBranch::new_from(tokens, elif_ln, elif_pos)?;
            Ok((
                ExprBoxWrap::new(Self {
                    if_branch,
                    else_branch: Some(else_branch),
                    elif_branches,
                }),
                ret_ln,
                ret_pos,
            ))
        } else {
            Ok((
                ExprBoxWrap::new(Self {
                    if_branch,
                    else_branch: None,
                    elif_branches,
                }),
                elif_ln,
                elif_pos,
            ))
        }
    }
}

/// If-branch of [`IfExpr`]
///
/// # Rule
/// \<if-branch\> ::= "if" \<expr\> \<block-expr\>
///
/// # See also
/// [`IfExpr`]
/// [`BlockExpr`]
/// [`ElifBranch`]
/// [`ElseBranch`]
#[derive(Debug, Clone, PartialEq)]
pub struct IfBranch {
    pub cond: ExprBoxWrap,
    pub block: BlockExpr,
}

#[macro_export]
macro_rules! new_if_branch {
    ($cond:expr, $block:expr) => {
        IfBranch {
            cond: ExprBoxWrap::new($cond),
            block: $block,
        }
    };
}

impl IfBranch {
    // TODO: remove dead code and unused allows here.
    /// Parses an [`IfBranch`] from the token list passed in.
    ///
    /// # Parameters:
    /// - `tokens` List of tokens.
    /// - `line` `pos` Position of token just popped front before passing
    ///   `tokens` into this function. If no token has been popped yet, just
    ///   pass the position of the first token in. If there's no token, why are
    ///   you even calling this.
    ///
    /// # Errors
    /// - If there's no token where a token is expected, return
    ///   [`ParseError::ExpectedToken`].
    /// - If the first token is not [`TokenType::If`], return
    ///   [`ParseError::UnexpectedToken`].
    /// - If parsing of [`Expr`] fails, percolate the failure. If the returned
    ///   failure is `None`, return a [`ParseError::UnendedStmt`] instead.
    /// - Similarly, if parsing of [`BlockExpr`] fails, percolate the failure.
    #[allow(dead_code)]
    #[allow(unused)]
    pub(super) fn new_from(
        tokens: &mut VecDeque<Token>,
        line: usize,
        pos: usize,
    ) -> Result<(Self, usize, usize), ParseError> {
        let (if_ln, if_pos) = tokens
            .pop_front()
            .ok_or(ParseError::ExpectedToken { line, pos })
            .and_then(|tok| match tok.tok_typ {
                TokenType::If => Ok((tok.line_number, tok.line_position)),
                _ => Err(ParseError::UnexpectedToken(tok)),
            })?;
        let (cond, cond_ln, cond_pos) = Expr::parse(tokens, if_ln, if_pos)
            .map_err(|e| match e {
                None => ParseError::UnendedStmt {
                    line: if_ln,
                    pos: if_pos,
                },
                Some(err) => err,
            })?;
        let (block, block_ln, block_pos) =
            BlockExpr::new_from(tokens, cond_ln, cond_pos)?;
        Ok((Self { cond, block }, block_ln, block_pos))
    }
}

/// Elif-branch of [`IfExpr`]
///
/// # Rule
/// \<elif-branch\> ::= "elif" \<expr\> \<block-expr\>
/// - Basically identical to if, but the keyword is "else".
///
/// # See also
/// [`IfExpr`]
/// [`BlockExpr`]
/// [`IfBranch`]
/// [`ElseBranch`]
#[derive(Debug, Clone, PartialEq)]
pub struct ElifBranch {
    pub cond: ExprBoxWrap,
    pub block: BlockExpr,
}

#[macro_export]
macro_rules! new_elif_branch {
    ($cond:expr, $block:expr) => {
        ElifBranch {
            cond: ExprBoxWrap::new($cond),
            block: $block,
        }
    };
}

impl ElifBranch {
    // TODO: remove dead code and unused allows here.
    /// Parses an [`ElifBranch`] from the token list passed in.
    ///
    /// # Parameters:
    /// - `tokens` List of tokens.
    /// - `line` `pos` Position of token just popped front before passing
    ///   `tokens` into this function. If no token has been popped yet, just
    ///   pass the position of the first token in. If there's no token, why are
    ///   you even calling this.
    ///
    /// # Errors
    /// - If there's no token where a token is expected, return
    ///   [`ParseError::ExpectedToken`].
    /// - If the first token is not [`TokenType::Elif`], return
    ///   [`ParseError::UnexpectedToken`].
    /// - If parsing of [`Expr`] fails, percolate the failure. If the returned
    ///   failure is `None`, return a [`ParseError::UnendedStmt`] instead.
    /// - Similarly, if parsing of [`BlockExpr`] fails, percolate the failure.
    #[allow(dead_code)]
    #[allow(unused)]
    pub(super) fn new_from(
        tokens: &mut VecDeque<Token>,
        line: usize,
        pos: usize,
    ) -> Result<(Self, usize, usize), ParseError> {
        let (elif_ln, elif_pos) = tokens
            .pop_front()
            .ok_or(ParseError::ExpectedToken { line, pos })
            .and_then(|tok| match tok.tok_typ {
                TokenType::Elif => Ok((tok.line_number, tok.line_position)),
                _ => Err(ParseError::UnexpectedToken(tok)),
            })?;
        let (cond, cond_ln, cond_pos) = Expr::parse(tokens, elif_ln, elif_pos)
            .map_err(|e| match e {
                None => ParseError::UnendedStmt {
                    line: elif_ln,
                    pos: elif_pos,
                },
                Some(err) => err,
            })?;
        let (block, block_ln, block_pos) =
            BlockExpr::new_from(tokens, cond_ln, cond_pos)?;
        Ok((Self { cond, block }, block_ln, block_pos))
    }
}

/// Else-branch of [`IfExpr`]
///
/// # Rule
/// \<else-branch\> ::= "else" \<block-expr\>
///
/// # Note
/// - Since an [`IfExpr`] is an [`Expr`](ExprAst), it can be the expression
///   for this. In the form of "else if ...".
/// - You can chain a bunch of "else if"s behind.
///
/// # See also
/// [`IfExpr`]
/// [`IfBranch`]
/// [`BlockExpr`]
/// [`ElifBranch`]
#[derive(Debug, Clone, PartialEq)]
pub struct ElseBranch {
    pub block: BlockExpr,
}

#[macro_export]
macro_rules! new_else_branch {
    ($block:expr) => {
        ElseBranch { block: $block }
    };
}

impl ElseBranch {
    // TODO: remove dead code and unused allows here.
    /// Parses an [`ElseBranch`] from the token list passed in.
    ///
    /// # Parameters:
    /// - `tokens` List of tokens.
    /// - `line` `pos` Position of token just popped front before passing
    ///   `tokens` into this function. If no token has been popped yet, just
    ///   pass the position of the first token in. If there's no token, why are
    ///   you even calling this.
    ///
    /// # Errors
    /// - If there's no token where a token is expected, return
    ///   [`ParseError::ExpectedToken`].
    /// - If the first token is not [`TokenType::Else`], return
    ///   [`ParseError::UnexpectedToken`].
    /// - If parsing of [`BlockExpr`] fails, percolate the failure.
    #[allow(dead_code)]
    #[allow(unused)]
    pub(super) fn new_from(
        tokens: &mut VecDeque<Token>,
        line: usize,
        pos: usize,
    ) -> Result<(Self, usize, usize), ParseError> {
        let (else_ln, else_pos) = tokens
            .pop_front()
            .ok_or(ParseError::ExpectedToken { line, pos })
            .and_then(|tok| match tok.tok_typ {
                TokenType::Else => Ok((tok.line_number, tok.line_position)),
                _ => Err(ParseError::UnexpectedToken(tok)),
            })?;
        let (block, block_ln, block_pos) =
            BlockExpr::new_from(tokens, else_ln, else_pos)?;
        Ok((Self { block }, block_ln, block_pos))
    }
}
