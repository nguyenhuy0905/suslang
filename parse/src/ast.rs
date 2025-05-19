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
    expr: OrExpr,
}

/// Or logical.
///
/// # Rule
/// \<or-expr\> ::= \<and-expr\> ("||" \<and-expr\>)*
///
/// # See also
/// - [`AndExpr`]
#[derive(Debug, PartialEq, Clone)]
struct OrExpr {
    first_clause: AndExpr,
    follow_clauses: Vec<AndExpr>,
}

/// And logical.
///
/// # Rule
/// \<and-expr\> ::= \<comp-expr\> ("&&" \<comp-expr\>)*
///
/// # See also
/// - [`ComparisonExpr`]
#[derive(Debug, PartialEq, Clone)]
struct AndExpr {
    first_clause: ComparisonExpr,
    follow_clauses: Vec<ComparisonExpr>,
}

/// Equality comparison expression.
///
/// # Rule
/// \<comp-expr\> ::= \<term-expr\>
///   (("==" | "!=" | ">" | "<" | ">=" | "<=") \<term-expr\>)?
///
/// # See also
/// - [`BitAndExpr`]
#[derive(Debug, PartialEq, Clone)]
struct ComparisonExpr {
    first_comp: BitOrExpr,
    second_comp: Option<BitOrExpr>,
    op: Option<ComparisonOp>,
}

/// Bit or
///
/// # Rule
/// \<bit-or-expr\> ::= \<bit-and-expr\> ("|" \<bit-and-expr\>)*
///
/// # See also
/// - [`BitAndExpr`]
#[derive(Debug, PartialEq, Clone)]
struct BitOrExpr {
    first_bit_and: BitAndExpr,
    follow_bit_ands: Vec<BitAndExpr>,
}

/// Bit and
///
/// # Rule
/// \<bit-and-expr\> ::= \<term-expr\> ("&" \<term-expr\>)*
///
/// # See also
/// - [`TermExpr`]
#[derive(Debug, PartialEq, Clone)]
struct BitAndExpr {
    first_term: TermExpr,
    follow_terms: Vec<TermExpr>,
}

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

#[derive(Debug, PartialEq, Eq, Clone)]
#[repr(u8)]
enum ComparisonOp {
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    LessThanEqual,
    GreaterThanEqual,
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

impl AstNode for ComparisonExpr {
    fn parse(tokens: &mut VecDeque<Token>) -> Result<Self, Option<ParseError>> {
        let first_comp = BitOrExpr::parse(tokens)?;
        let (op, second_comp) = {
            if let Some((op, line, pos)) = tokens
                .front()
                .map(Token::bind_ref)
                .and_then(|(typ, line, pos)| match typ {
                    TokenType::EqualEqual
                    | TokenType::BangEqual
                    | TokenType::LPBrace
                    | TokenType::RPBrace
                    | TokenType::LPBraceEqual
                    | TokenType::RPBraceEqual => Some((typ, line, pos)),
                    _ => None,
                })
                .map(|(typ, line, pos)| (match typ {
                    TokenType::EqualEqual => ComparisonOp::Equal,
                    TokenType::BangEqual => ComparisonOp::NotEqual,
                    TokenType::LPBrace => ComparisonOp::LessThan,
                    TokenType::RPBrace => ComparisonOp::GreaterThan,
                    TokenType::LPBraceEqual => ComparisonOp::LessThanEqual,
                    TokenType::RPBraceEqual => ComparisonOp::GreaterThanEqual,
                    _ => panic!(
                        "Type {typ:?} shouldn't pass to match a comparison op"
                    ),
                }, line, pos))
            {
                tokens.pop_front();
                (Some(op), Some(BitOrExpr::parse(tokens).map_err(|e| if e.is_none() {
                    Some(ParseError {
                        typ: ParseErrorType::ExpectedExpr,
                        line, pos
                    })
                } else {e})?))
            } else {
                (None, None)
            }
        };
        Ok(Self {
            first_comp,
            second_comp,
            op,
        })
    }
}

impl AstNode for BitOrExpr {
    fn parse(tokens: &mut VecDeque<Token>) -> Result<Self, Option<ParseError>> {
        // near identical to BitAndExpr::parse save for some values.
        let first_bit_and = BitAndExpr::parse(tokens)?;
        let follow_bit_ands = {
            let mut ret = Vec::new();
            if let Some((&TokenType::Beam, line, pos)) =
                tokens.front().map(Token::bind_ref)
            {
                tokens.pop_front();
                // if there is a beam and nothing else after, it's an error.
                ret.push(BitAndExpr::parse(tokens).map_err(|e| {
                    if e.is_none() {
                        Some(ParseError {
                            typ: ParseErrorType::ExpectedExpr,
                            line,
                            pos,
                        })
                    } else {
                        e
                    }
                })?);
            }
            ret
        };
        Ok(BitOrExpr {
            first_bit_and,
            follow_bit_ands,
        })
    }
}

impl AstNode for BitAndExpr {
    fn parse(tokens: &mut VecDeque<Token>) -> Result<Self, Option<ParseError>> {
        let first_term = TermExpr::parse(tokens)?;
        let follow_terms = {
            let mut ret = Vec::new();
            if let Some((&TokenType::Ampersand, line, pos)) =
                tokens.front().map(Token::bind_ref)
            {
                tokens.pop_front();
                // if there is an ampersand and nothing else after, it's an error.
                ret.push(TermExpr::parse(tokens).map_err(|e| {
                    if e.is_none() {
                        Some(ParseError {
                            typ: ParseErrorType::ExpectedExpr,
                            line,
                            pos,
                        })
                    } else {
                        e
                    }
                })?);
            }
            ret
        };
        Ok(BitAndExpr {
            first_term,
            follow_terms,
        })
    }
}

// TODO: actually start recursively descending. Let's go.

impl AstNode for TermExpr {
    fn parse(tokens: &mut VecDeque<Token>) -> Result<Self, Option<ParseError>> {
        let first_term = FactorExpr::parse(tokens)?;
        let follow_terms = {
            let mut ret = Vec::new();
            while let Some((Some(term_op), line, pos)) = tokens
                .front()
                .map(Token::bind_ref)
                .map(|(some_op, line, pos)| {
                    (
                        match some_op {
                            TokenType::Plus => Some(TermOp::Plus),
                            TokenType::Dash => Some(TermOp::Minus),
                            _ => None,
                        },
                        line,
                        pos,
                    )
                })
            {
                // remove the term_op token
                tokens.pop_front();
                ret.push((term_op, FactorExpr::parse(tokens)?));
            }
            ret
        };
        Ok(Self {
            first_term,
            follow_terms,
        })
    }
}

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
