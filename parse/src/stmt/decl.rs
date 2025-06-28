use std::{any::Any, collections::VecDeque, fmt::Debug, rc::Rc};

use tokenize::{Token, TokenType};

use crate::{Expr, ExprBoxWrap, ExprParse, ParseError};

/// Statement AST node tag.
///
/// Must also implement traits [`Debug`], [`Clone`], [`PartialEq`] for blanket
/// [`DeclStmtImpl`]
/// implementation.
pub trait DeclStmtAst: Any + Debug {
    fn identifier(&self) -> &str;
}

/// Blanket implementation for [`DeclStmtAst`]
pub trait DeclStmtImpl: DeclStmtAst {
    /// Double-dispatch comparison.
    fn accept_cmp(&self, other: &dyn DeclStmtImpl) -> bool;
    /// Dispatched clone.
    fn boxed_clone(&self) -> Box<dyn DeclStmtImpl>;
}

impl<T> DeclStmtImpl for T
where
    T: DeclStmtAst + Clone + PartialEq,
{
    fn accept_cmp(&self, other: &dyn DeclStmtImpl) -> bool {
        (other as &dyn Any).downcast_ref::<T>() == Some(self)
    }

    fn boxed_clone(&self) -> Box<dyn DeclStmtImpl> {
        Box::new(self.clone())
    }
}

impl PartialEq for dyn DeclStmtImpl {
    fn eq(&self, other: &Self) -> bool {
        self.accept_cmp(other)
    }
}

/// Wrapper around a `dyn` [`DeclStmtImpl`].
pub struct DeclStmtBoxWrap {
    pub val: Box<dyn DeclStmtImpl>,
}

impl DeclStmtBoxWrap {
    pub fn new<T: DeclStmtImpl>(v: T) -> Self {
        Self { val: Box::new(v) }
    }
}

impl Debug for DeclStmtBoxWrap {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.val.fmt(f)
    }
}

impl PartialEq for DeclStmtBoxWrap {
    fn eq(&self, other: &Self) -> bool {
        self.val.accept_cmp(other.val.as_ref())
    }
}

impl Clone for DeclStmtBoxWrap {
    fn clone(&self) -> Self {
        Self {
            val: self.val.boxed_clone(),
        }
    }
}

impl AsRef<dyn DeclStmtImpl> for DeclStmtBoxWrap {
    fn as_ref(&self) -> &dyn DeclStmtImpl {
        self.val.as_ref()
    }
}

pub trait DeclStmtParse: DeclStmtImpl {
    /// Parses into a [`DeclStmtBoxWrap`].
    ///
    /// # Errors
    /// - If it fails, it fails.
    fn parse(
        tokens: &mut VecDeque<Token>,
        line: usize,
        pos: usize,
    ) -> Result<(DeclStmtBoxWrap, usize, usize), ParseError>;
}

/// Let statement.
///
/// # Rule
/// "let" "mut"? ID "=" \<expr\>
///
/// # See also
/// [`Expr`]
#[derive(Debug, Clone, PartialEq)]
pub struct LetStmt {
    // when we get to variable scoping, the scope struct needs a reference to
    // the string.
    pub name: Rc<String>,
    pub init_val: ExprBoxWrap,
    pub mutability: LetStmtMut,
}

#[macro_export]
macro_rules! new_let_stmt {
    ($id:expr, $expr:expr, $mut:expr) => {
        LetStmt {
            name: Rc::new(String::from($id)),
            init_val: ExprBoxWrap::new($expr),
            mutability: $mut,
        }
    };
}

impl LetStmt {
    /// Parses into a `LetStmt`.
    ///
    /// # Errors
    /// - If the tokens list is empty at any point where a token is expected,
    ///   fail with [`ParseError::ExpectedToken`].
    /// - Otherwise, if the very first token read is not a [`TokenType::Let`],
    ///   fail with [`ParseError::UnexpectedToken`].
    /// - If the 2nd token (or the 3rd if the 2nd is [`TokenType::Mut`]) is not
    ///   a [`TokenType::Identifier`], fail with [`ParseError::UnexpectedToken`].
    /// - If the token following the identifier mentioned above is not a
    ///   [`TokenType::Equal`], fail with [`ParseError::UnexpectedToken`].
    /// - After that, follow the return of [`Expr::parse`]. Save for
    ///   [`Option::None`] error. In that case, return a
    ///   [`ParseError::UnendedStmt`].
    pub fn new_from(
        tokens: &mut VecDeque<Token>,
        line: usize,
        pos: usize,
    ) -> Result<(Self, usize, usize), ParseError> {
        // check the "let"
        let (let_ln, let_pos) = tokens
            .pop_front()
            .ok_or(ParseError::ExpectedToken { line, pos })
            .and_then(|tok| match tok.tok_typ {
                TokenType::Let => Ok((tok.line_number, tok.line_position)),
                _ => Err(ParseError::UnexpectedToken(tok)),
            })?;
        // check mutability
        let (mutability, mut_ln, mut_pos) = tokens
            .front()
            .ok_or(ParseError::ExpectedToken {
                line: let_ln,
                pos: let_pos,
            })
            .map(|tok| match tok.tok_typ {
                TokenType::Mut => {
                    (LetStmtMut::Mutable, tok.line_number, tok.line_position)
                }
                _ => {
                    (LetStmtMut::Immutable, tok.line_number, tok.line_position)
                }
            })
            .inspect(|(mutability, ..)| {
                if mutability == &LetStmtMut::Mutable {
                    tokens.pop_front();
                }
            })?;
        let (name, name_ln, name_pos) = tokens
            .pop_front()
            .ok_or(ParseError::ExpectedToken {
                line: mut_ln,
                pos: mut_pos,
            })
            .and_then(|tok| match tok.tok_typ {
                TokenType::Identifier(s) => {
                    Ok((s, tok.line_number, tok.line_position))
                }
                _ => Err(ParseError::UnexpectedToken(tok)),
            })?;
        // get the equal sign
        let (eq_ln, eq_pos) = tokens
            .pop_front()
            .ok_or(ParseError::ExpectedToken {
                line: name_ln,
                pos: name_pos,
            })
            .and_then(|tok| match tok.tok_typ {
                TokenType::Equal => Ok((tok.line_number, tok.line_position)),
                _ => Err(ParseError::UnexpectedToken(tok)),
            })?;
        // parse the expr
        let (init_val, ret_ln, ret_pos) = Expr::parse(tokens, eq_ln, eq_pos)
            .map_err(|e| match e {
                Some(err) => err,
                None => ParseError::UnendedStmt {
                    line: let_ln,
                    pos: let_pos,
                },
            })?;
        Ok((
            Self {
                name: Rc::new(name),
                init_val,
                mutability,
            },
            ret_ln,
            ret_pos,
        ))
    }
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum LetStmtMut {
    Immutable,
    Mutable,
}

impl DeclStmtAst for LetStmt {
    fn identifier(&self) -> &str {
        &self.name
    }
}

impl DeclStmtParse for LetStmt {
    fn parse(
        tokens: &mut VecDeque<Token>,
        line: usize,
        pos: usize,
    ) -> Result<(DeclStmtBoxWrap, usize, usize), ParseError> {
        Self::new_from(tokens, line, pos)
            .map(|(ret, ln, ps)| (DeclStmtBoxWrap::new(ret), ln, ps))
    }
}
