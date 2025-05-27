use super::Expr;
use crate::{Ast, ExprBoxWrap, ExprParse, ParseError};
use std::collections::VecDeque;
use tokenize::{Token, TokenType};

// TODO: add array and tuple.

/// Primary data types. May not be primary any more in the future.
///
/// # Rule
/// \<prim-expr\> ::= NUMBER | FLOAT | STRING | "ya" | "na"
#[derive(Debug, PartialEq, Clone)]
pub enum PrimaryExpr {
    Integer(u64),
    Float(f64),
    String(String),
    Boolean(bool),
    Identifier(String),
}

impl Ast for PrimaryExpr {}

impl ExprParse for PrimaryExpr {
    fn parse(
        tokens: &mut VecDeque<Token>,
    ) -> Result<ExprBoxWrap, Option<ParseError>> {
        if let Some((typ, line, pos)) = tokens.pop_front().map(Token::bind) {
            match typ {
                TokenType::Integer(s_in) => {
                    let parsed_int = s_in.parse::<u64>();
                    debug_assert!(parsed_int.is_ok());
                    Ok(ExprBoxWrap::new(PrimaryExpr::Integer(
                        parsed_int.unwrap(),
                    )))
                }
                TokenType::Double(s_dbl) => {
                    let parsed_dbl = s_dbl.parse::<f64>();
                    debug_assert!(parsed_dbl.is_ok());
                    Ok(ExprBoxWrap::new(PrimaryExpr::Float(
                        parsed_dbl.unwrap(),
                    )))
                }
                TokenType::String(s) => {
                    Ok(ExprBoxWrap::new(PrimaryExpr::String(s)))
                }
                TokenType::Identifier(s) => {
                    Ok(ExprBoxWrap::new(PrimaryExpr::Identifier(s)))
                }
                TokenType::Ya => {
                    Ok(ExprBoxWrap::new(PrimaryExpr::Boolean(true)))
                }
                TokenType::Na => {
                    Ok(ExprBoxWrap::new(PrimaryExpr::Boolean(false)))
                }
                TokenType::LParen => {
                    let ret = Expr::parse(tokens)?;
                    if let Some(&TokenType::RParen) =
                        tokens.front().map(Token::token_type)
                    {
                        tokens.pop_front();
                        Ok(ret)
                    } else if tokens.is_empty() {
                        Err(Some(ParseError::UnclosedParen { line, pos }))
                    } else {
                        Err(Some(ParseError::UnexpectedToken(
                            tokens.pop_front().unwrap(),
                        )))
                    }
                }
                _ => Err(Some(ParseError::UnexpectedToken(Token::new(
                    typ, line, pos,
                )))),
            }
        } else {
            Err(None)
        }
    }
}

/// Unary expressions
///
/// # Rule
/// \<unary-expr\> ::= ("+" | "-" | "*" | "&" | "!") \<prim-expr\>
///
/// # See also
/// [`PrimaryExpr`]
#[derive(Debug, PartialEq, Clone)]
pub struct UnaryExpr {
    pub primary: ExprBoxWrap,
    pub op: UnaryOp,
}

#[macro_export]
macro_rules! new_unary_expr {
    ($primary:expr, $op:expr) => {
        UnaryExpr {
            primary: ExprBoxWrap::new($primary),
            op: $op,
        }
    };
}

#[derive(Debug, PartialEq, Clone)]
pub enum UnaryOp {
    Plus,   // +
    Minus,  // -
    Deref,  // *
    Ref,    // &
    Negate, // !
}

impl Ast for UnaryExpr {}

impl ExprParse for UnaryExpr {
    fn parse(
        tokens: &mut VecDeque<Token>,
    ) -> Result<ExprBoxWrap, Option<ParseError>> {
        if let Some((typ, line, pos)) = tokens.pop_front().map(Token::bind) {
            let Some(op) = (match typ {
                TokenType::Plus => Some(UnaryOp::Plus),
                TokenType::Dash => Some(UnaryOp::Minus),
                TokenType::Star => Some(UnaryOp::Deref),
                TokenType::Ampersand => Some(UnaryOp::Ref),
                TokenType::Bang => Some(UnaryOp::Negate),
                _ => None,
            }) else {
                tokens.push_front(Token::new(typ, line, pos));
                return PrimaryExpr::parse(tokens);
            };

            Ok(ExprBoxWrap::new(UnaryExpr {
                primary: PrimaryExpr::parse(tokens).map_err(|e| {
                    if e.is_none() {
                        Some(ParseError::ExpectedToken { line, pos })
                    } else {
                        e
                    }
                })?,
                op,
            }))
        } else {
            Err(None)
        }
    }
}

/// Factor expression, that is, multiply or divide.
///
/// # Rule
/// \<fac-expr\> ::= \<unary-expr\> ("*" | "/" \<unary-expr\>)*
///
/// # See also
/// [`UnaryExpr`]
#[derive(Debug, PartialEq, Clone)]
pub struct FactorExpr {
    pub first_fac: ExprBoxWrap,
    pub follow_facs: Vec<(FactorOp, ExprBoxWrap)>,
}

/// Convenience macro to construct a new `FactorExpr`
#[macro_export]
macro_rules! new_factor_expr {
    ($first_fac:expr$(,$follow_fac_op:expr,$follow_fac_exp:expr)+$(,)?) => {
        FactorExpr {
            first_fac: ExprBoxWrap::new($first_fac),
            follow_facs: vec![$(($follow_fac_op,ExprBoxWrap::new($follow_fac_exp)),)+],
        }
    };
}

#[derive(Debug, PartialEq, Clone)]
pub enum FactorOp {
    Multiply, // *
    Divide,   // /
    Mod,      // %
}

impl Ast for FactorExpr {}

impl ExprParse for FactorExpr {
    fn parse(
        tokens: &mut VecDeque<Token>,
    ) -> Result<ExprBoxWrap, Option<ParseError>> {
        let first_fac = UnaryExpr::parse(tokens)?;
        // if I don't wrap this inside a closure, the return statement inside
        // that else, returns through the entire function.
        let follow_facs = || -> Result<Vec<_>, Option<ParseError>> {
            let mut ret = Vec::new();
            while let Some((typ, line, pos)) =
                tokens.front().map(Token::bind_ref)
            {
                let Some(fac_op) = (match typ {
                    TokenType::Star => Some(FactorOp::Multiply),
                    TokenType::Slash => Some(FactorOp::Divide),
                    TokenType::Percent => Some(FactorOp::Mod),
                    _ => None,
                }) else {
                    return Ok(ret);
                };
                tokens.pop_front();
                let next_fac = UnaryExpr::parse(tokens).map_err(|e| {
                    if e.is_none() {
                        Some(ParseError::ExpectedToken { line, pos })
                    } else {
                        e
                    }
                })?;
                ret.push((fac_op, next_fac));
            }
            Ok(ret)
        }()?;
        if follow_facs.is_empty() {
            Ok(first_fac)
        } else {
            Ok(ExprBoxWrap::new(FactorExpr {
                first_fac,
                follow_facs,
            }))
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct TermExpr {
    pub first_term: ExprBoxWrap,
    pub follow_terms: Vec<(TermOp, ExprBoxWrap)>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TermOp {
    Plus,
    Minus,
}

impl Ast for TermExpr {}

#[macro_export]
macro_rules! new_term_expr {
    ($first_term:expr$(,$follow_op:expr,$follow_term:expr)+$(,)?) => {
        TermExpr {
            first_term: ExprBoxWrap::new($first_term),
            follow_terms: vec![$(($follow_op, ExprBoxWrap::new($follow_term)),)+]
        }
    };
}

impl ExprParse for TermExpr {
    fn parse(
        tokens: &mut VecDeque<Token>,
    ) -> Result<ExprBoxWrap, Option<ParseError>> {
        let first_term = FactorExpr::parse(tokens)?;
        let follow_terms = || -> Result<Vec<_>, Option<ParseError>> {
            let mut ret = Vec::new();
            while let Some((typ, line, pos)) =
                tokens.front().map(Token::bind_ref)
            {
                let Some(term_op) = (match typ {
                    TokenType::Plus => Some(TermOp::Plus),
                    TokenType::Dash => Some(TermOp::Minus),
                    _ => None,
                }) else {
                    return Ok(ret);
                };
                tokens.pop_front();
                let next_term = FactorExpr::parse(tokens).map_err(|e| {
                    if e.is_none() {
                        Some(ParseError::ExpectedToken { line, pos })
                    } else {
                        e
                    }
                })?;
                ret.push((term_op, next_term));
            }
            Ok(ret)
        }()?;
        if follow_terms.is_empty() {
            Ok(first_term)
        } else {
            Ok(ExprBoxWrap::new(TermExpr {
                first_term,
                follow_terms,
            }))
        }
    }
}

/// Bitwise and
///
/// # Rule
/// \<bit-and-expr\> ::= \<term-expr\> ("&" \<term-expr\>)*
///
/// # See also
/// [`TermExpr`]
#[derive(Debug, PartialEq, Clone)]
pub struct BitAndExpr {
    pub first_bit_and: ExprBoxWrap,
    pub follow_bit_ands: Vec<ExprBoxWrap>,
}

#[macro_export]
macro_rules! new_bit_and_expr {
    ($first_bit_and:expr$(,$bit_and:expr)+$(,)?) => {
        BitAndExpr {
            first_bit_and: ExprBoxWrap::new($first_bit_and),
            follow_bit_ands: vec![$(ExprBoxWrap::new($bit_and))+],
        }
    }
}

impl Ast for BitAndExpr {}

impl ExprParse for BitAndExpr {
    fn parse(
        tokens: &mut VecDeque<Token>,
    ) -> Result<ExprBoxWrap, Option<ParseError>> {
        let first_bit_and = TermExpr::parse(tokens)?;
        let follow_bit_ands = || -> Result<Vec<_>, Option<ParseError>> {
            let mut ret = Vec::new();
            while let Some((&TokenType::Ampersand, line, pos)) =
                tokens.front().map(Token::bind_ref)
            {
                tokens.pop_front();
                ret.push(TermExpr::parse(tokens).map_err(|e| {
                    if e.is_none() {
                        Some(ParseError::ExpectedToken { line, pos })
                    } else {
                        e
                    }
                })?);
            }
            Ok(ret)
        }()?;
        if follow_bit_ands.is_empty() {
            Ok(first_bit_and)
        } else {
            Ok(ExprBoxWrap::new(Self {
                first_bit_and,
                follow_bit_ands,
            }))
        }
    }
}

/// Bitwise xor
///
/// # Rule
/// \<bit-xor-expr\> ::= \<bit-and-expr\> ("&" \<bit-and-expr\>)*
///
/// # See also
/// [`BitAndExpr`]
#[derive(Debug, Clone, PartialEq)]
pub struct BitXorExpr {
    pub first_bit_xor: ExprBoxWrap,
    pub follow_bit_xors: Vec<ExprBoxWrap>,
}

impl Ast for BitXorExpr {}

#[macro_export]
macro_rules! new_bit_xor_expr {
    ($first_bit_xor:expr$(,$bit_xor:expr)+$(,)?) => {
        BitXorExpr {
            first_bit_xor: ExprBoxWrap::new($first_bit_xor),
            follow_bit_xors: vec![$(ExprBoxWrap::new($bit_xor))+],
        }
    }
}

impl ExprParse for BitXorExpr {
    fn parse(
        tokens: &mut VecDeque<Token>,
    ) -> Result<ExprBoxWrap, Option<ParseError>> {
        let first_bit_xor = BitAndExpr::parse(tokens)?;
        let follow_bit_xors = || -> Result<Vec<_>, Option<ParseError>> {
            let mut ret = Vec::new();
            while let Some((&TokenType::Hat, line, pos)) =
                tokens.front().map(Token::bind_ref)
            {
                tokens.pop_front();
                ret.push(BitAndExpr::parse(tokens).map_err(|e| {
                    if e.is_none() {
                        Some(ParseError::ExpectedToken { line, pos })
                    } else {
                        e
                    }
                })?);
            }
            Ok(ret)
        }()?;
        if follow_bit_xors.is_empty() {
            Ok(first_bit_xor)
        } else {
            Ok(ExprBoxWrap::new(Self {
                first_bit_xor,
                follow_bit_xors,
            }))
        }
    }
}

/// Bitwise or
///
/// # Rule
/// \<bit-or-expr\> ::= \<bit-xor-expr\> ("&" \<bit-xor-expr\>)*
///
/// # See also
/// [`BitXorExpr`]
#[derive(Debug, Clone, PartialEq)]
pub struct BitOrExpr {
    pub first_bit_or: ExprBoxWrap,
    pub follow_bit_ors: Vec<ExprBoxWrap>,
}

impl Ast for BitOrExpr {}

#[macro_export]
macro_rules! new_bit_or_expr {
    ($first_bit_or:expr $(,$bit_or:expr)+$(,)?) => {
        BitOrExpr {
            first_bit_or: ExprBoxWrap::new($first_bit_or),
            follow_bit_ors: vec![$(ExprBoxWrap::new($bit_or),)*],
        }
    }
}

impl ExprParse for BitOrExpr {
    fn parse(
        tokens: &mut VecDeque<Token>,
    ) -> Result<ExprBoxWrap, Option<ParseError>> {
        let first_bit_or = BitXorExpr::parse(tokens)?;
        let follow_bit_ors = || -> Result<Vec<_>, Option<ParseError>> {
            let mut ret = Vec::new();
            while let Some((&TokenType::Beam, line, pos)) =
                tokens.front().map(Token::bind_ref)
            {
                tokens.pop_front();
                ret.push(BitXorExpr::parse(tokens).map_err(|e| {
                    if e.is_none() {
                        Some(ParseError::ExpectedToken { line, pos })
                    } else {
                        e
                    }
                })?);
            }
            Ok(ret)
        }()?;
        if follow_bit_ors.is_empty() {
            Ok(first_bit_or)
        } else {
            Ok(ExprBoxWrap::new(Self {
                first_bit_or,
                follow_bit_ors,
            }))
        }
    }
}
