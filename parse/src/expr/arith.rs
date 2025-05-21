use crate::{Ast, AstBoxWrap, AstClone, AstParse, ParseError};
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
}

impl Ast for PrimaryExpr {}

impl AstParse for PrimaryExpr {
    fn parse(
        tokens: &mut VecDeque<Token>,
    ) -> Result<AstBoxWrap, Option<ParseError>> {
        if let Some((typ, _, _)) = tokens.pop_front().map(Token::bind) {
            match typ {
                TokenType::Integer(s_in) => {
                    let parsed_int = s_in.parse::<u64>();
                    debug_assert!(parsed_int.is_ok());
                    Ok(AstBoxWrap::new(PrimaryExpr::Integer(
                        parsed_int.unwrap(),
                    )))
                }
                TokenType::Double(s_dbl) => {
                    let parsed_dbl = s_dbl.parse::<f64>();
                    debug_assert!(parsed_dbl.is_ok());
                    Ok(AstBoxWrap::new(PrimaryExpr::Float(parsed_dbl.unwrap())))
                }
                TokenType::String(s) => {
                    Ok(AstBoxWrap::new(PrimaryExpr::String(s)))
                }
                TokenType::Ya => {
                    Ok(AstBoxWrap::new(PrimaryExpr::Boolean(true)))
                }
                TokenType::Na => {
                    Ok(AstBoxWrap::new(PrimaryExpr::Boolean(false)))
                }
                _ => todo!("Expr::parse"),
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
    pub primary: AstBoxWrap,
    pub op: UnaryOp,
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

impl AstParse for UnaryExpr {
    fn parse(
        tokens: &mut VecDeque<Token>,
    ) -> Result<AstBoxWrap, Option<ParseError>> {
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

            Ok(AstBoxWrap::new(UnaryExpr {
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
    pub first_fac: AstBoxWrap,
    pub follow_facs: Vec<(FactorOp, AstBoxWrap)>,
}

impl FactorExpr {
    /// Preferably use the macro `new_factor_expr!`
    #[allow(dead_code)]
    pub fn new(
        first_fac: AstBoxWrap,
        follow_facs: Vec<(FactorOp, AstBoxWrap)>,
    ) -> Self {
        Self {
            first_fac,
            follow_facs,
        }
    }
}

/// Convenience macro to construct a new `FactorExpr`
#[macro_export]
macro_rules! new_factor_expr {
    ($first_fac:expr,$($follow_fac_op:expr,$follow_fac_exp:expr,)+) => {
        FactorExpr::new(
            AstBoxWrap::new($first_fac),
             vec![$(($follow_fac_op,AstBoxWrap::new($follow_fac_exp)),)+]
         )
    };
}

#[derive(Debug, PartialEq, Clone)]
pub enum FactorOp {
    Multiply, // *
    Divide,   // /
}

impl Ast for FactorExpr {}

impl AstParse for FactorExpr {
    fn parse(
        tokens: &mut VecDeque<Token>,
    ) -> Result<AstBoxWrap, Option<ParseError>> {
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
            Ok(AstBoxWrap::new(FactorExpr {
                first_fac,
                follow_facs,
            }))
        }
    }
}
