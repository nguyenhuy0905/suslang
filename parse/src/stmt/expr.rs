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
            // TODO: add more types of statements as we go.
            Some(_) => ExprSemicolonStmt::parse(tokens, line, pos),
        }
    }
}

/// An expression followed by a semicolon
///
/// # Rule
/// \<expr-stmt\> ::= \<expr\> ";"
///
/// # See also
/// [`Expr`]
#[derive(Debug, Clone, PartialEq)]
pub struct ExprSemicolonStmt {
    pub expr: ExprBoxWrap,
}

impl ExprStmtAst for ExprSemicolonStmt {}

impl ExprStmtParse for ExprSemicolonStmt {
    fn parse(
        tokens: &mut VecDeque<Token>,
        line: usize,
        pos: usize,
    ) -> Result<(ExprStmtBoxWrap, usize, usize), ParseError> {
        let (expr, expr_ln, expr_pos) =
            Expr::parse(tokens).map_err(|e| match e {
                Some(err) => err,
                None => ParseError::ExpectedToken { line, pos },
            })?;
        let (ret_ln, ret_pos) = tokens
            .pop_front()
            .ok_or(ParseError::ExpectedToken {
                line: expr_ln,
                pos: expr_pos,
            })
            .and_then(|tok| match tok.tok_typ {
                TokenType::Semicolon => {
                    Ok((tok.line_number, tok.line_position))
                }
                _ => Err(ParseError::UnexpectedToken(tok)),
            })?;
        Ok((ExprStmtBoxWrap::new(Self { expr }), ret_ln, ret_pos))
    }
}
