use std::{any::Any, collections::VecDeque, fmt::Debug};

use tokenize::{Token, TokenType};

use crate::{Expr, ExprBoxWrap, ExprParse, ParseError};

/// Tag for any expression statement.
///
/// Apart from [`Any`] and [`Debug`], any expression statement should also
/// implement
pub trait ExprStmtAst: Any + Debug {}

/// Auto-impl for [`ExprStmtAst`].
pub trait ExprStmtImpl: ExprStmtAst {
    /// Clone `self` as a [`Box`]
    fn boxed_clone(&self) -> Box<dyn ExprStmtImpl>;
    /// Double-dispatch to compare two `dyn` [`ExprStmtImpl`]
    fn accept_cmp(&self, other: &dyn ExprStmtImpl) -> bool;
}

impl<T: ExprStmtAst + Clone + PartialEq> ExprStmtImpl for T {
    fn boxed_clone(&self) -> Box<dyn ExprStmtImpl> {
        Box::new(self.clone())
    }

    fn accept_cmp(&self, other: &dyn ExprStmtImpl) -> bool {
        (other as &dyn Any).downcast_ref() == Some(self)
    }
}

impl PartialEq for dyn ExprStmtImpl {
    fn eq(&self, other: &Self) -> bool {
        self.accept_cmp(other)
    }
}

/// Wrapper around a [`Box<dyn ExprStmtImpl>`]
pub struct ExprStmtBoxWrap {
    pub val: Box<dyn ExprStmtImpl>,
}

impl ExprStmtBoxWrap {
    /// Create a new `ExprStmtBoxWrap`
    pub fn new<T: ExprStmtImpl>(val: T) -> Self {
        Self { val: Box::new(val) }
    }
}

impl Debug for ExprStmtBoxWrap {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.val.as_ref().fmt(f)
    }
}

impl Clone for ExprStmtBoxWrap {
    fn clone(&self) -> Self {
        Self {
            val: self.val.boxed_clone(),
        }
    }
}

impl PartialEq for ExprStmtBoxWrap {
    fn eq(&self, other: &Self) -> bool {
        self.val.accept_cmp(other.val.as_ref())
    }
}

impl AsRef<dyn ExprStmtImpl> for ExprStmtBoxWrap {
    fn as_ref(&self) -> &dyn ExprStmtImpl {
        self.val.as_ref()
    }
}

pub trait ExprStmtParse: ExprStmtImpl {
    /// Parses the list of input tokens into a [`ExprStmtBoxWrap`].
    ///
    /// # Errors
    /// - If parsing fails, a [`ParseError`] is returned.
    fn parse(
        tokens: &mut VecDeque<Token>,
        line: usize,
        pos: usize,
    ) -> Result<(ExprStmtBoxWrap, usize, usize), ParseError>;
}

/// Dummy expression statement struct.
#[derive(Debug, Clone, PartialEq)]
pub struct ExprStmt {}

impl ExprStmtAst for ExprStmt {}

impl ExprStmtParse for ExprStmt {
    fn parse(
        tokens: &mut VecDeque<Token>,
        line: usize,
        pos: usize,
    ) -> Result<(ExprStmtBoxWrap, usize, usize), ParseError> {
        match tokens.front().map(Token::token_type) {
            None => Err(ParseError::ExpectedToken { line, pos }),
            Some(TokenType::Return) => ReturnStmt::parse(tokens, line, pos),
            Some(TokenType::BlockReturn) => {
                BlockReturnStmt::parse(tokens, line, pos)
            }
            Some(TokenType::Identifier(_)) => IdStmt::parse(tokens, line, pos),
            // TODO: add more types of statements as we go.
            Some(_) => ExprValStmt::parse(tokens, line, pos),
        }
    }
}

/// Any statement that starts with an identifier
///
/// # Rule
/// \<id-stmt\> ::= IDENTIFIER ("=" \<assign-stmt-rhs\> | \<expr-val-after-id\>)
/// \<assign-stmt-rhs\> ::= \<expr\>
///
/// I dunno how I should write this in terms of BNF, but,
/// \<expr-val-after-id\> isn't really a rule, it's that I dunno how to say we
/// just delegate the parsing work down to `ExprValStmt`
///
/// # See also
/// [`ExprValStmt`]
/// [`AssignStmt`]
#[derive(Debug, Clone, PartialEq)]
pub struct IdStmt {}

impl ExprStmtAst for IdStmt {}

impl ExprStmtParse for IdStmt {
    fn parse(
        tokens: &mut VecDeque<Token>,
        line: usize,
        pos: usize,
    ) -> Result<(ExprStmtBoxWrap, usize, usize), ParseError> {
        // checking whether the first token type is an identifier can be
        // delegated.
        match tokens.get(1).map(Token::token_type) {
            Some(TokenType::Equal) => AssignStmt::parse(tokens, line, pos),
            _ => ExprValStmt::parse(tokens, line, pos),
        }
    }
}

/// An expression basically.
///
/// Very boring, but this wrapper is needed to enable some statement-specific
/// features.
///
/// # Rule
/// \<expr-stmt\> ::= \<expr\>
///
/// # See also
/// [`Expr`]
/// [`IdStmt`]
#[derive(Debug, Clone, PartialEq)]
pub struct ExprValStmt {
    pub expr: ExprBoxWrap,
}

#[macro_export]
macro_rules! new_expr_semicolon_stmt {
    ($expr:expr) => {
        ExprValStmt {
            expr: ExprBoxWrap::new($expr),
        }
    };
}

impl ExprStmtAst for ExprValStmt {}

impl ExprStmtParse for ExprValStmt {
    fn parse(
        tokens: &mut VecDeque<Token>,
        line: usize,
        pos: usize,
    ) -> Result<(ExprStmtBoxWrap, usize, usize), ParseError> {
        let (expr, expr_ln, expr_pos) = Expr::parse(tokens, line, pos)
            .map_err(|e| match e {
                Some(err) => err,
                None => ParseError::ExpectedToken { line, pos },
            })?;
        Ok((ExprStmtBoxWrap::new(Self { expr }), expr_ln, expr_pos))
    }
}

/// Assignment statement
///
/// # Rule
/// \<assign-stmt\> ::= IDENTIFIER "=" \<expr\>
///
/// # See also
/// [`IdStmt`]
#[derive(Debug, Clone, PartialEq)]
pub struct AssignStmt {
    pub id: String,
    pub val: ExprBoxWrap,
}

#[macro_export]
macro_rules! new_assign_stmt {
    ($id:expr, $val:expr) => {
        AssignStmt {
            id: String::from($id),
            val: ExprBoxWrap::new($val),
        }
    };
}

impl ExprStmtAst for AssignStmt {}

impl AssignStmt {
    /// Constructs an [`AssignStmt`] from the token list passed in
    ///
    /// # Errors
    /// - If at any point, `tokens` is empty, return [`ParseError::ExpectedToken`].
    /// - If the first element from `tokens` is not [`TokenType::Identifier`],
    ///   return [`ParseError::UnexpectedToken`]
    /// - If the second element from `tokens` is not [`TokenType::Equal`],
    ///   return [`ParseError::UnexpectedToken`]
    /// - If the next elements from `tokens` cannot be parsed into an
    ///   [`Expr`]:
    ///   - If the returned error from trying to parse is `None`, return
    ///     [`ParseError::UnendedStmt`].
    ///   - Otherwise, percolate up the error, removing the `Option` wrapper.
    fn new_from(
        tokens: &mut VecDeque<Token>,
        line: usize,
        pos: usize,
    ) -> Result<(Self, usize, usize), ParseError> {
        tokens
            .pop_front()
            // check for the identifier
            .ok_or(ParseError::ExpectedToken { line, pos })
            .and_then(|tok| match tok.tok_typ {
                TokenType::Identifier(s) => {
                    Ok((s, tok.line_number, tok.line_position))
                }
                _ => Err(ParseError::UnexpectedToken(tok)),
            })
            // check for the equal sign
            .and_then(|(name, name_ln, name_pos)| {
                tokens
                    .pop_front()
                    .ok_or(ParseError::ExpectedToken {
                        line: name_ln,
                        pos: name_pos,
                    })
                    .and_then(|tok| match tok.tok_typ {
                        TokenType::Equal => {
                            Ok((name, tok.line_number, tok.line_position))
                        }
                        _ => Err(ParseError::UnexpectedToken(tok)),
                    })
            })
            // try parse the next token(s) into an `Expr`
            .and_then(|(name, eq_ln, eq_pos)| {
                Expr::parse(tokens, eq_ln, eq_pos)
                    .map_err(|e| match e {
                        None => ParseError::UnendedStmt {
                            line: eq_ln,
                            pos: eq_pos,
                        },
                        Some(err) => err,
                    })
                    .map(|(expr, expr_ln, expr_pos)| {
                        (
                            Self {
                                id: name,
                                val: expr,
                            },
                            expr_ln,
                            expr_pos,
                        )
                    })
            })
    }
}

impl ExprStmtParse for AssignStmt {
    fn parse(
        tokens: &mut VecDeque<Token>,
        line: usize,
        pos: usize,
    ) -> Result<(ExprStmtBoxWrap, usize, usize), ParseError> {
        Self::new_from(tokens, line, pos).map(|(ret, ret_ln, ret_pos)| {
            (ExprStmtBoxWrap::new(ret), ret_ln, ret_pos)
        })
    }
}

/// Return, then optionally a statement
///
/// # Rule
/// \<expr-stmt\> ::= "return" \<expr\>?
///
/// # See also
/// [`Expr`]
#[derive(Debug, Clone, PartialEq)]
pub struct ReturnStmt {
    pub expr: Option<ExprBoxWrap>,
}

#[macro_export]
macro_rules! new_return_stmt {
    () => {
        ReturnStmt { expr: None }
    };
    ($expr:expr) => {
        ReturnStmt {
            expr: Some(ExprBoxWrap::new($expr)),
        }
    };
}

impl ExprStmtAst for ReturnStmt {}

impl ExprStmtParse for ReturnStmt {
    fn parse(
        tokens: &mut VecDeque<Token>,
        line: usize,
        pos: usize,
    ) -> Result<(ExprStmtBoxWrap, usize, usize), ParseError> {
        let (ret_tok_ln, ret_tok_pos) = tokens
            .pop_front()
            .ok_or(ParseError::ExpectedToken { line, pos })
            .and_then(|tok| match tok.tok_typ {
                TokenType::Return => Ok((tok.line_number, tok.line_position)),
                _ => Err(ParseError::UnexpectedToken(tok)),
            })?;
        let (expr, expr_ln, expr_pos) =
            match Expr::parse(tokens, ret_tok_ln, ret_tok_pos) {
                Ok((expr, ln, pos)) => Ok((Some(expr), ln, pos)),
                Err(None) => Ok((None, ret_tok_ln, ret_tok_pos)),
                Err(Some(e)) => Err(e),
            }?;

        Ok((ExprStmtBoxWrap::new(Self { expr }), expr_ln, expr_pos))
    }
}

/// Return out of current block, optionally with an expression.
///
/// # Rule
/// \<block-ret-stmt\> ::= "block_return" \<expr\>?
///
/// # See also
/// [`Expr`]
#[derive(Debug, Clone, PartialEq)]
pub struct BlockReturnStmt {
    pub expr: Option<ExprBoxWrap>,
}

#[macro_export]
macro_rules! new_block_return_stmt {
    () => {
        BlockReturnStmt { expr: None }
    };
    ($expr:expr) => {
        BlockReturnStmt {
            expr: Some(ExprBoxWrap::new($expr)),
        }
    };
}

impl ExprStmtAst for BlockReturnStmt {}

impl ExprStmtParse for BlockReturnStmt {
    fn parse(
        tokens: &mut VecDeque<Token>,
        line: usize,
        pos: usize,
    ) -> Result<(ExprStmtBoxWrap, usize, usize), ParseError> {
        let (ret_tok_ln, ret_tok_pos) = tokens
            .pop_front()
            .ok_or(ParseError::ExpectedToken { line, pos })
            .and_then(|tok| match tok.tok_typ {
                TokenType::BlockReturn => {
                    Ok((tok.line_number, tok.line_position))
                }
                _ => Err(ParseError::UnexpectedToken(tok)),
            })?;
        let (expr, expr_ln, expr_pos) =
            match Expr::parse(tokens, ret_tok_ln, ret_tok_pos) {
                Ok((expr, ln, pos)) => Ok((Some(expr), ln, pos)),
                Err(None) => Ok((None, ret_tok_ln, ret_tok_pos)),
                Err(Some(e)) => Err(e),
            }?;

        Ok((ExprStmtBoxWrap::new(Self { expr }), expr_ln, expr_pos))
    }
}
