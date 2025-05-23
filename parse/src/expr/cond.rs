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
        let lhs = BitOrExpr::parse(tokens)?;
        if let Some((Some(op), line, pos)) = tokens
            .front()
            .map(Token::bind_ref)
            .and_then(|(typ, line, pos)| {
                Some((
                    match typ {
                        TokenType::LPBrace => Some(ComparisonOp::Less),
                        TokenType::RPBrace => Some(ComparisonOp::Greater),
                        TokenType::LPBraceEqual => {
                            Some(ComparisonOp::LessEqual)
                        }
                        TokenType::RPBraceEqual => {
                            Some(ComparisonOp::GreaterEqual)
                        }
                        _ => None,
                    },
                    line,
                    pos,
                ))
            })
        {
            tokens.pop_front();
            let rhs = BitOrExpr::parse(tokens).map_err(|e| {
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
    ($lhs:expr,$op:expr,$rhs:expr) => {
        ComparisonExpr {
            lhs: AstBoxWrap::new($lhs),
            rhs: AstBoxWrap::new($rhs),
            op: $op,
        }
    };
    ($lhs:expr,$rhs:expr,$op:expr) => {
        ComparisonExpr {
            lhs: AstBoxWrap::new($lhs),
            rhs: AstBoxWrap::new($rhs),
            op: $op,
        }
    };
}
