use std::{
    any::Any,
    collections::VecDeque,
    fmt::Debug,
    hash::{DefaultHasher, Hash, Hasher},
};

pub mod decl;
pub mod expr;
pub use decl::*;
pub use expr::*;
use tokenize::{Token, TokenType};

use crate::ParseError;
#[cfg(test)]
mod test;

/// A tag that a struct is a `Type`. Must implement [`Debug`]. Y'know, so that
/// we can [`assert_eq`] and all.
///
/// Must also implement [`PartialEq`] + [`Hash`] + [`Clone`] in order to get
/// [`TypeImpl`] auto-implemented.
pub trait Type: Any + Debug {}

/// Auto-impl of stuff for any `Type`.
pub trait TypeImpl: Type {
    /// Double-dispatch shenanigan to compare two `Type`s.
    fn accept_cmp(&self, other: &dyn TypeImpl) -> bool;
    /// Double-dispatch shenanigan to clone the type.
    fn boxed_clone(&self) -> Box<dyn TypeImpl>;
    /// Double-dispatch shenanigan to hash the type.
    fn get_hash_value(&self) -> u64;
}

/// How to parse a type.
impl<T> TypeImpl for T
where
    T: Type + PartialEq + Hash + Clone,
{
    fn accept_cmp(&self, other: &dyn TypeImpl) -> bool {
        (other as &dyn Any)
            .downcast_ref::<T>()
            .is_none_or(|typ| self == typ)
    }
    fn boxed_clone(&self) -> Box<dyn TypeImpl> {
        Box::new(self.clone())
    }

    fn get_hash_value(&self) -> u64 {
        let mut hasher = DefaultHasher::new();
        self.hash(&mut hasher);
        hasher.finish()
    }
}

impl PartialEq for dyn TypeImpl {
    fn eq(&self, other: &Self) -> bool {
        self.accept_cmp(other)
    }
}

impl Eq for dyn TypeImpl {}

/// A statement is either an expression-statement or a declaration statement.
///
/// # Rule
/// \<stmt\> ::= (\<expr-stmt\> | \<decl-stmt\>) ";"
#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Expr(ExprStmtBoxWrap),
    Decl(DeclStmtBoxWrap),
}

#[macro_export]
macro_rules! new_stmt {
    (Expr($val:expr)) => {
        Stmt::Expr(ExprStmtBoxWrap::new($val))
    };
    (Decl($val:expr)) => {
        Stmt::Decl(DeclStmtBoxWrap::new($val))
    };
}

impl Stmt {
    /// # Errors
    /// - Percolated up from either type of statement.
    /// - Or an error if the token list is empty
    pub fn parse(
        tokens: &mut VecDeque<Token>,
        line: usize,
        pos: usize,
    ) -> Result<(Self, usize, usize), ParseError> {
        // TODO: update Stmt::parse every time I'm done with a new type of statement.
        //     match tokens
        //         .front()
        //         .ok_or(ParseError::ExpectedToken { line, pos })
        //         .map(Token::token_type)?
        //     {
        //         _ => {
        //             let (expr_stmt, ret_ln, ret_pos) =
        //                 ExprStmt::parse(tokens, line, pos)?;
        //             Ok((Self::Expr(expr_stmt), line, pos))
        //         }
        //     }
        let (expr_stmt, ret_ln, ret_pos) = ExprStmt::parse(tokens, line, pos)?;
        let (semicolon_ln, semicolon_pos) = tokens
            .pop_front()
            .ok_or(ParseError::ExpectedToken {
                line: ret_ln,
                pos: ret_pos,
            })
            .and_then(|tok| {
                if matches!(tok.tok_typ, TokenType::Semicolon) {
                    Ok((tok.line_number, tok.line_position))
                } else {
                    Err(ParseError::UnexpectedToken(tok))
                }
            })?;
        Ok((Self::Expr(expr_stmt), semicolon_ln, semicolon_pos))
    }
}
