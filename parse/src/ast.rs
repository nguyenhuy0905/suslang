// TODO: remove allow dead_code on top of file
#![allow(unused)]

use std::collections::VecDeque;
use std::fmt::Debug;
use tokenize::{Token, TokenType};
#[cfg(test)]
mod test;

#[derive(Debug)]
#[repr(u8)]
pub enum ParseErrorType {
    UnexpectedToken(tokenize::TokenType),
    ExpectedExpr,
    WrongType(TypeTag),
}

#[derive(Debug)]
pub struct ParseError {
    typ: ParseErrorType,
    line: usize,
    pos: usize,
}

/// Node.rs. No vtable allowed.
trait AstNode: std::marker::Sized {
    /// Parses the list of [`Token`]s. If succeed, returns an instance of
    /// `Self`, otherwise:
    /// - If parse fails due to end-of-file, the error is `None`.
    /// - Otherwise, it's an actual error.
    ///
    /// * `tokens`: A [`VecDeque`] of tokens. Obtained from calling
    ///   [`tokenize::tokenize`].
    fn parse(tokens: &mut VecDeque<Token>) -> Result<Self, Option<ParseError>>;
}

/// A version of [`tokenize::TokenType`] but without the value and other cruft.
/// Just the types.
///
/// The custom types are stored literally. As a string.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeTag {
    Integer,
    Double,
    String,
    Custom(String),
}

/// A program consists of one or more statements, for now.
///
/// Of course, only certain types of statements may (or must) not be inside a
/// block; say, a function declaration.
///
/// # Rule
///
/// \<program\> ::= \<stmt\>+
///
/// # See also
///
/// [`Stmt`]
#[derive(Debug, PartialEq, Clone)]
struct Program {
    stmts: Vec<Stmt>,
}

/// A statement evaluates to the void type, if it ends with a semicolon.
///
/// A statement without semicolon is, for now, an expression. Hah.
#[derive(Debug, PartialEq, Clone)]
struct Stmt {}

/// An expression evaluates to a specific type.
///
/// Sometimes, that may include the void type.
#[derive(Debug, PartialEq, Clone)]
struct Expr {
    term: TermExpr,
}

// TODO: rules for LHS of either arithmetic or boolean expression,
// then a smaller rule for RHS of arithmetic and RHS of boolean expressions.

/// Add or minus.
///
/// # Rule
/// \<term-expr\> ::= \<factor-expr\> (\<term-op\> <\factor-expr\>)*
/// \<term-op\> ::= "+" | "-"
///
/// # See also
/// - [`FactorExpr`]
#[derive(Debug, PartialEq, Clone)]
struct TermExpr {
    first_term: FactorExpr,
    follow_terms: Vec<(TermOp, FactorExpr)>,
}

/// Multiply or divide
///
/// # Rule
/// \<factor-expr\> ::= \<arith-unary-expr\>
///                     (\<factor-op\> <\arith-unary-expr\>)*
///
/// \<factor-op\> ::= "*" | "/"
///
/// # See also
/// - [`ArithUnaryExpr`]
#[derive(Debug, PartialEq, Clone)]
struct FactorExpr {
    first_factor: UnaryExpr,
    follow_factors: Vec<(FacOp, UnaryExpr)>,
}

// TODO: finish writing the docs

// TODO: type-check the expression.
//
// Simplest way I could think of is, well, another enum. All expressions needs
// to be manually implemented the trait that returns that enum.
//
// Most of the expression types will just forward the type request to its child
// anyways.

// TODO: add support for boolean expressions.

/// Either just a wrapper around a primary expression, or a numerical primary
/// expression with an unary operator.
///
/// # Rule
/// \<arith-unary-expr\> ::= ("+" | "-")? \<prim-expr\>
///
/// # Note
/// - If there is "+" or "-" at the beginning, \<prim-expr\>'s type must be
///   number.
///
/// # See also
/// [`PrimaryExpr`]
#[derive(Debug, PartialEq, Clone)]
struct UnaryExpr {
    primary: PrimaryExpr,
    unary_op: Option<UnaryOp>,
}

#[derive(Debug, PartialEq, Clone)]
struct PrimaryExpr {
    typ: PrimaryExprType,
}

// a bunch of variants.
// God damn it, where is my anonymous enum?

#[derive(Debug, PartialEq, Clone)]
enum PrimaryExprType {
    LiteralInteger(u64),
    LiteralDouble(f64),
    LiteralString(String),
    // must be a box otherwise it's an infinite definition recursion.
    GroupedExpr(Box<Expr>),
}

/// Operators for [`TermExpr`].
///
/// # Rule
/// \<term-op\> ::= "+" | "-"
#[derive(Debug, PartialEq, Eq, Clone)]
#[repr(u8)]
enum TermOp {
    Plus,
    Minus,
}

/// Operators for [`FactorExpr`].
///
/// # Rule
/// \<factor-op\> ::= "*" | "/"
#[derive(Debug, PartialEq, Eq, Clone)]
#[repr(u8)]
enum FacOp {
    Multiply,
    Divide,
}

#[repr(u8)]
#[derive(Debug, PartialEq, Eq, Clone)]
enum UnaryOp {
    Negate,
    Plus,
}

// TODO: actually start recursively descending. Let's go.

impl AstNode for FactorExpr {
    fn parse(tokens: &mut VecDeque<Token>) -> Result<Self, Option<ParseError>> {
        let first_factor = UnaryExpr::parse(tokens)?;
        let follow_factors = {
            let mut ret = Vec::new();
            while let Some((Some(fac_op), line, pos)) =
                tokens.front().map(|tok| {
                    (
                        match tok.bind_ref().0 {
                            TokenType::Star => Some(FacOp::Multiply),
                            TokenType::Slash => Some(FacOp::Divide),
                            _ => None,
                        },
                        tok.bind_ref().1,
                        tok.bind_ref().2,
                    )
                })
            {
                // remove the token matching the factor operator
                tokens.pop_front();
                // parse the next unary expression.
                ret.push((
                    fac_op,
                    UnaryExpr::parse(tokens).map_err(|e| match e {
                        Some(e) => e,
                        None => ParseError {
                            typ: ParseErrorType::ExpectedExpr,
                            line,
                            pos,
                        },
                    })?,
                ));
            }
            ret
        };

        Ok(Self {
            first_factor,
            follow_factors,
        })
    }
}

impl AstNode for UnaryExpr {
    fn parse(tokens: &mut VecDeque<Token>) -> Result<Self, Option<ParseError>> {
        let Some((unary_op, line, pos)) = tokens
            .front()
            .map(Token::bind_ref)
            .map(|(tok_type, line, pos)| {
                (
                    match tok_type {
                        TokenType::Dash => Some(UnaryOp::Negate),
                        TokenType::Plus => Some(UnaryOp::Plus),
                        _ => None,
                    },
                    line,
                    pos,
                )
            })
        else {
            return Err(None);
        };
        if unary_op.is_some() {
            tokens.pop_front();
        }

        let primary = PrimaryExpr::parse(tokens).map_err(|e| {
            if e.is_none() {
                Some(ParseError {
                    typ: ParseErrorType::ExpectedExpr,
                    line,
                    pos,
                })
            } else {
                e
            }
        })?;

        Ok(Self { primary, unary_op })
    }
}

impl AstNode for PrimaryExpr {
    fn parse(tokens: &mut VecDeque<Token>) -> Result<Self, Option<ParseError>> {
        let Some((tok_type, line, pos)) = tokens.pop_front().map(Token::bind)
        else {
            return Err(None);
        };

        match tok_type {
            TokenType::Integer(s_in) => {
                debug_assert!(s_in.parse::<u64>().is_ok());
                Ok(Self {
                    typ: PrimaryExprType::LiteralInteger(s_in.parse().unwrap()),
                })
            }
            TokenType::Double(s_dbl) => {
                debug_assert!(s_dbl.parse::<f64>().is_ok());
                Ok(Self {
                    typ: PrimaryExprType::LiteralDouble(
                        s_dbl.parse::<f64>().unwrap(),
                    ),
                })
            }
            TokenType::String(s) => Ok(Self {
                typ: PrimaryExprType::LiteralString(s),
            }),
            TokenType::LPBrace => todo!(
                "PrimaryExpr parse: Grouped expression not implemented yet"
            ),
            _ => Err(Some(ParseError {
                typ: ParseErrorType::UnexpectedToken(tok_type),
                line,
                pos,
            })),
        }
    }
}
