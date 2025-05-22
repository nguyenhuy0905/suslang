use std::collections::VecDeque;

use tokenize::{Token, TokenType};

use crate::*;

/// Comparison
///
/// # Rule
/// \<comp-expr\> ::= \<term-expr\>
///                   ("==" | "!=" | ">=" | "<=" | ">" | "<")
///                   \<term-expr\>
///
/// # See also
/// [`TermExpr`]
#[derive(Debug, Clone, PartialEq)]
pub struct ComparisonExpr {
    pub lhs: AstBoxWrap,
    pub rhs: AstBoxWrap,
    pub op: ComparisonOp,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ComparisonOp {
    Equal,
    NotEqual,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
}

impl Ast for ComparisonExpr {}

impl AstParse for ComparisonExpr {
    fn parse(
        tokens: &mut VecDeque<Token>,
    ) -> Result<AstBoxWrap, Option<ParseError>> {
        let lhs = TermExpr::parse(tokens)?;
        if let Some((Some(op), line, pos)) = tokens
            .front()
            .map(Token::bind_ref)
            .and_then(|(typ, line, pos)| {
                Some((
                    match typ {
                        TokenType::LPBrace => Some(ComparisonOp::Less),
                        TokenType::RPBrace => Some(ComparisonOp::Greater),
                        TokenType::LPBraceEqual => {
                            Some(ComparisonOp::GreaterEqual)
                        }
                        TokenType::RPBraceEqual => {
                            Some(ComparisonOp::LessEqual)
                        }
                        _ => None,
                    },
                    line,
                    pos,
                ))
            })
        {
            let rhs = TermExpr::parse(tokens).map_err(|e| {
                if e.is_none() {
                    Some(ParseError::ExpectedToken { line, pos })
                } else {
                    e
                }
            })?;
            Ok(AstBoxWrap::new(ComparisonExpr { lhs, rhs, op }))
        } else {
            Ok(lhs)
        }
    }
}

#[macro_export]
macro_rules! new_comparison_expr {
    ($lhs:expr,==,$rhs:expr) => {
        ComparisonOp {
            lhs: AstBoxWrap::new($lhs),
            rhs: AstBoxWrap::new($rhs),
            op: ComparisonOp::Equal,
        }
    };
    ($lhs:expr,!=,$rhs:expr) => {
        ComparisonOp {
            lhs: AstBoxWrap::new($lhs),
            rhs: AstBoxWrap::new($rhs),
            op: ComparisonOp::NotEqual,
        }
    };
    ($lhs:expr,<,$rhs:expr) => {
        ComparisonOp {
            lhs: AstBoxWrap::new($lhs),
            rhs: AstBoxWrap::new($rhs),
            op: ComparisonOp::Less,
        }
    };
    ($lhs:expr,>,$rhs:expr) => {
        ComparisonOp {
            lhs: AstBoxWrap::new($lhs),
            rhs: AstBoxWrap::new($rhs),
            op: ComparisonOp::Greater,
        }
    };
    ($lhs:expr,<=,$rhs:expr) => {
        ComparisonOp {
            lhs: AstBoxWrap::new($lhs),
            rhs: AstBoxWrap::new($rhs),
            op: ComparisonOp::LessEqual,
        }
    };
    ($lhs:expr,>=,$rhs:expr) => {
        ComparisonOp {
            lhs: AstBoxWrap::new($lhs),
            rhs: AstBoxWrap::new($rhs),
            op: ComparisonOp::GreaterEqual,
        }
    };
}
