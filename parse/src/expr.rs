use std::{any::Any, fmt::Debug, string::ParseError};

use tokenize::Token;

/// Tag for all expression types.
/// Must impl `Debug + Clone + PartialEq`.
pub trait ExprAst: Debug + Any {}

/// Auto-impl for all `ExprAst`
pub trait ExprImpl: ExprAst {
    /// Dispatched call to clone.
    fn boxed_clone(&self) -> Box<dyn ExprImpl>;
    /// Dispatched call to compare 2 `dyn ExprAst`s.
    fn accept_cmp(&self, other: &dyn ExprAst) -> bool;
}

impl<T> ExprImpl for T
where
    T: ExprAst + Clone + PartialEq,
{
    fn boxed_clone(&self) -> Box<dyn ExprImpl> {
        Box::new(self.clone())
    }

    fn accept_cmp(&self, other: &dyn ExprAst) -> bool {
        (other as &dyn Any)
            .downcast_ref()
            .map_or(false, |other| self == other)
    }
}

impl PartialEq for dyn ExprImpl {
    fn eq(&self, other: &Self) -> bool {
        self.accept_cmp(other)
    }
}

impl Clone for Box<dyn ExprImpl> {
    fn clone(&self) -> Self {
        self.boxed_clone()
    }
}

pub trait ExprParse {
    fn parse(tokens: &[Token]) -> Result<ExprBox, ParseError>;
}

/// Wrapper around a `Box<dyn ExprImpl>`
///
/// Just to implement `PartialEq`
#[derive(Clone)]
pub struct ExprBox {
    pub val: Box<dyn ExprImpl>,
}

impl PartialEq for ExprBox {
    fn eq(&self, other: &Self) -> bool {
        self.val.accept_cmp(other.val.as_ref())
    }
}

// just skip the wrapper
impl Debug for ExprBox {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.val.fmt(f)
    }
}
