use std::collections::VecDeque;

use tokenize::{Token, TokenType};

use crate::*;

/// Comparison
///
/// # Rule
/// \<comp-expr\> ::= \<term-expr\>
///                   (("==" | "!=" | ">=" | "<=" | ">" | "<")
///                   \<term-expr\>)?
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
    ($lhs:expr,$op:expr,$rhs:expr$(,)?) => {
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

/// Logical and
///
/// # Rule
/// \<logic-and-expr\> ::= \<comp-expr\> ("&&" \<comp-expr\>)*
///
/// # See also
/// [`ComparisonExpr`]
#[derive(Debug, PartialEq, Clone)]
pub struct LogicAndExpr {
    pub first_logic_and: AstBoxWrap,
    pub follow_logic_ands: Vec<AstBoxWrap>,
}

#[macro_export]
macro_rules! new_logic_and_expr {
    ($first_logic_and:expr $(,$follow_logic_and:expr)+$(,)?) => {
        LogicAndExpr {
            first_logic_and:AstBoxWrap::new($first_logic_and),
            follow_logic_ands:vec![$(AstBoxWrap::new($follow_logic_and),)+],
        }
    };
}

impl Ast for LogicAndExpr {}

impl AstParse for LogicAndExpr {
    fn parse(
        tokens: &mut VecDeque<Token>,
    ) -> Result<AstBoxWrap, Option<ParseError>> {
        let first_logic_and = ComparisonExpr::parse(tokens)?;
        let follow_logic_ands = {
            let mut ret = Vec::new();
            while let Some((TokenType::AmpersandAmpersand, line, pos)) =
                tokens.front().map(Token::bind_ref)
            {
                tokens.pop_front();
                ret.push(ComparisonExpr::parse(tokens).map_err(|e| {
                    if e.is_none() {
                        Some(ParseError::ExpectedToken { line, pos })
                    } else {
                        e
                    }
                })?);
            }
            ret
        };
        if follow_logic_ands.is_empty() {
            Ok(first_logic_and)
        } else {
            Ok(AstBoxWrap::new(LogicAndExpr {
                first_logic_and,
                follow_logic_ands,
            }))
        }
    }
}
