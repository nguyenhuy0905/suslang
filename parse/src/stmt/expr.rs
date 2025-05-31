use std::{any::Any, collections::VecDeque, fmt::Debug, string::ParseError};

use tokenize::Token;

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

pub trait ExprStmtParse: ExprStmtImpl {
    /// Parses the list of input tokens into a [`ExprStmtBoxWrap`].
    ///
    /// # Errors
    /// - If parsing fails, a [`ParseError`] is returned.
    fn parse(tokens: VecDeque<Token>) -> Result<ExprStmtBoxWrap, ParseError>;
}
