#[allow(clippy::wildcard_imports)]
use crate::*;
pub mod arith;
pub mod block;
pub mod cond;
#[cfg(test)]
mod test;
pub use arith::*;
use block::{BlockExpr, ProcExpr};
pub use cond::*;
use tokenize::TokenType;

/// Empty for now, but in case I need something down the line.
/// Requires Debug so that the thing can be unit-tested.
pub trait ExprAst: Any + Debug {}

/// impl Ast and derive Clone and this is auto-implemented.
pub trait ExprAstImpl: ExprAst {
    /// Some double dispatch shenanigan, to compare two `&dyn AstCmp`
    fn accept_cmp(&self, other: &dyn ExprAstImpl) -> bool;
    fn boxed_clone(&self) -> Box<dyn ExprAstImpl>;
}

impl PartialEq for dyn ExprAstImpl {
    fn eq(&self, other: &Self) -> bool {
        self.accept_cmp(other)
    }
}

impl<T: ExprAst + Clone + PartialEq> ExprAstImpl for T {
    fn boxed_clone(&self) -> Box<dyn ExprAstImpl> {
        Box::new(self.clone())
    }

    fn accept_cmp(&self, other: &dyn ExprAstImpl) -> bool {
        (other as &dyn Any).downcast_ref::<T>() == Some(self)
    }
}

/// Wrapper around a `Box<dyn AstClone>`
#[derive(Debug)]
pub struct ExprBoxWrap {
    pub value: Box<dyn ExprAstImpl>,
}

impl PartialEq for ExprBoxWrap {
    fn eq(&self, other: &Self) -> bool {
        self.value.as_ref().accept_cmp(other.value.as_ref())
    }
}

impl Clone for ExprBoxWrap {
    fn clone(&self) -> Self {
        Self {
            value: self.value.boxed_clone(),
        }
    }
}

impl ExprBoxWrap {
    pub fn new<T: ExprAstImpl>(val: T) -> Self {
        Self {
            value: Box::new(val),
        }
    }
}

impl std::ops::Deref for ExprBoxWrap {
    type Target = dyn ExprAstImpl;

    fn deref(&self) -> &Self::Target {
        self.value.as_ref()
    }
}

/// Parses an expression.
pub trait ExprParse: ExprAstImpl {
    /// Parses into an `Ast`.
    ///
    /// # Errors
    /// - If parsing stops due to running out of tokens (not due to
    ///   grammartical errors), returns `Err(None)`.
    /// - If parsing stops for any other reason, returns an
    ///   `Err(Some(ParseError::<error enum>))` where the error value depends on
    ///   the type of error.
    ///
    /// # See also
    /// [`ParseError`]
    fn parse(
        tokens: &mut VecDeque<Token>,
        line: usize,
        pos: usize,
    ) -> Result<(ExprBoxWrap, usize, usize), Option<ParseError>>;
}

/// Dummy expression struct
///
/// # Rule (for now)
/// \<expr\> ::=  \<proc-expr\>
///             | \<block-expr\>
///             | \<logic-or-expr\>
///
/// # See also
/// [`LogicOrExpr`]
#[derive(Debug, Clone, PartialEq)]
pub struct Expr {}

impl ExprAst for Expr {}

impl ExprParse for Expr {
    fn parse(
        tokens: &mut VecDeque<Token>,
        line: usize,
        pos: usize,
    ) -> Result<(ExprBoxWrap, usize, usize), Option<ParseError>> {
        match tokens.front().map(Token::token_type) {
            Some(&TokenType::LCParen) => BlockExpr::parse(tokens, line, pos),
            Some(&TokenType::Proc) => ProcExpr::parse(tokens, line, pos),
            _ => LogicOrExpr::parse(tokens, line, pos),
        }
    }
}
