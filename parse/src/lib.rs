pub mod expr;
pub mod stmt;
pub use expr::*;
use std::any::Any;
use std::collections::VecDeque;
use std::fmt::Debug;
pub use stmt::*;
use tokenize::Token;

/// Empty for now, but in case I need something down the line.
/// Requires Debug so that the thing can be unit-tested.
pub trait Ast: Any + Debug {}

/// impl `Ast` and derive `PartialEq` and this is auto-implemented.
pub trait AstCmp: Ast {
    /// Some double dispatch shenanigan, to compare two `&dyn AstCmp`
    fn accept_cmp(&self, other: &dyn AstCmp) -> bool;
}

/// impl Ast and derive Clone and this is auto-implemented.
pub trait AstClone: AstCmp {
    fn boxed_clone(&self) -> Box<dyn AstClone>;
}

impl PartialEq for dyn AstCmp {
    fn eq(&self, other: &Self) -> bool {
        self.accept_cmp(other)
    }
}

impl<T: AstCmp + Clone> AstClone for T {
    fn boxed_clone(&self) -> Box<dyn AstClone> {
        Box::new(self.clone())
    }
}

#[derive(Debug, PartialEq)]
pub enum ParseError {
    UnexpectedToken(Token),
    ExpectedToken { line: usize, pos: usize },
    UnclosedParen { line: usize, pos: usize },
    UnendedStmt { line: usize, pos: usize },
}

// when are you gonna finish your language
// impl !PartialEq for Box<dyn AstClone> {}

/// Wrapper around a `Box<dyn AstClone>`
#[derive(Debug)]
pub struct AstBoxWrap {
    pub value: Box<dyn AstClone>,
}

impl PartialEq for AstBoxWrap {
    fn eq(&self, other: &Self) -> bool {
        self.value.as_ref().accept_cmp(other.value.as_ref())
    }
}

impl Clone for AstBoxWrap {
    fn clone(&self) -> Self {
        Self {
            value: self.value.boxed_clone(),
        }
    }
}

impl AstBoxWrap {
    pub fn new<T: AstClone>(val: T) -> Self {
        Self {
            value: Box::new(val),
        }
    }
}

impl std::ops::Deref for AstBoxWrap {
    type Target = dyn AstClone;

    fn deref(&self) -> &Self::Target {
        self.value.as_ref()
    }
}
