#[allow(clippy::wildcard_imports)]
use crate::*;
pub mod arith;
pub mod cond;
#[cfg(test)]
mod test;
pub use arith::*;
pub use cond::*;

// when are you gonna finish your language
// impl !PartialEq for Box<dyn AstClone> {}

/// Wrapper around a `Box<dyn AstClone>`
#[derive(Debug)]
pub struct ExprBoxWrap {
    pub value: Box<dyn AstClone>,
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
    pub fn new<T: AstClone>(val: T) -> Self {
        Self {
            value: Box::new(val),
        }
    }
}

impl std::ops::Deref for ExprBoxWrap {
    type Target = dyn AstClone;

    fn deref(&self) -> &Self::Target {
        self.value.as_ref()
    }
}

/// Parses an expression.
pub trait ExprParse: AstClone {
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
    ) -> Result<ExprBoxWrap, Option<ParseError>>;
}

impl<T: Ast + PartialEq> AstCmp for T {
    fn accept_cmp(&self, other: &dyn AstCmp) -> bool {
        // the function is the first dispatch
        (other as &dyn Any)
            // second dispatch
            .downcast_ref::<T>()
            .is_some_and(|ast| ast == self)
    }
}

/// Dummy expression struct
///
/// # Rule (for now)
/// \<expr\> ::= \<logic-or-expr\>
///
/// # See also
/// [`LogicOrExpr`]
#[derive(Debug, Clone, PartialEq)]
pub struct Expr {}

impl Ast for Expr {}

impl ExprParse for Expr {
    fn parse(
        tokens: &mut VecDeque<Token>,
    ) -> Result<ExprBoxWrap, Option<ParseError>> {
        LogicOrExpr::parse(tokens)
    }
}
