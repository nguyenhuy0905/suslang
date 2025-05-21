use std::any::Any;
use std::collections::VecDeque;
use std::ops::Deref;
use tokenize::{Token, TokenType};

pub trait AstParse: Ast {
    fn parse(tokens: &mut VecDeque<Token>) -> Box<dyn Ast>;
}

pub trait Ast: Any {
    /// Double-dispatch shenanigan to compare two dyn Ast.
    fn accept_cmp(&self, other: &dyn Ast) -> bool;
}

// Convenient macro to deal with Ast declaration boilerplate.
// TODO: write a derive for this
macro_rules! ast_accept_fn {
    () => {
        fn accept_cmp(&self, other: &dyn Ast) -> bool {
            (other as &dyn Any)
                .downcast_ref::<Self>()
                .map(|ast| self == ast)
                .unwrap_or(false)
        }
    };
}

impl PartialEq for dyn Ast {
    fn eq(&self, other: &Self) -> bool {
        self.accept_cmp(other)
    }
}

/// Reports error when parsing.
pub struct ParseError {
    typ: ParseErrorType,
    line: usize,
    pos: usize,
}

impl ParseError {
    /// Creates a new `ParseError`
    pub fn new(typ: ParseErrorType, line: usize, pos: usize) -> Self {
        Self { typ, line, pos }
    }

    pub fn typ(&self) -> &ParseErrorType {
        &self.typ
    }

    pub fn line(&self) -> usize {
        self.line
    }

    pub fn pos(&self) -> usize {
        self.pos
    }
}

/// Type of error.
pub enum ParseErrorType {
    UnexpectedToken(Token),
    ExpectedToken { line: usize, pos: usize },
}

pub struct AstBoxWrap {
    value: Box<dyn Ast>,
}

impl AstBoxWrap {
    pub fn new<T: Ast>(val: T) -> Self {
        Self {
            value: Box::new(val),
        }
    }

    pub fn value(&self) -> &dyn Ast {
        self.value.as_ref()
    }
}

impl Deref for AstBoxWrap {
    type Target = dyn Ast;

    fn deref(&self) -> &Self::Target {
        self.value.as_ref()
    }
}

impl PartialEq for AstBoxWrap {
    fn eq(&self, other: &Self) -> bool {
        &self.value == &other.value
    }
}

#[derive(PartialEq)]
pub struct Expr {
    expr: AstBoxWrap,
}

impl Ast for Expr {
    ast_accept_fn!();
}

#[derive(PartialEq)]
pub enum PrimaryType {
    Integer(u64),
    Double(f64),
    String(String),
    GroupedExpr(AstBoxWrap),
}
