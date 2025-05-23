use std::collections::VecDeque;

use tokenize::{Token, TokenType};

#[allow(clippy::wildcard_imports)]
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
        if let Some((Some(op), line, pos)) =
            tokens.front().map(Token::bind_ref).map(|(typ, line, pos)| {
                (
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
                )
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

/// Logical or
///
/// # Rule
/// \<logic-or-expr\> ::= \<logic-and-expr\> ("||" \<logic-and-expr\>)*
///
/// # See also
/// [`LogicAndExpr`]
#[derive(Debug, PartialEq, Clone)]
pub struct LogicOrExpr {
    pub first_logic_or: AstBoxWrap,
    pub follow_logic_ors: Vec<AstBoxWrap>,
}

#[macro_export]
macro_rules! new_logic_or_expr {
    ($first_logic_or:expr $(,$follow_logic_or:expr)+ $(,)?) => {
        LogicOrExpr {
            first_logic_or: AstBoxWrap::new($first_logic_or),
            follow_logic_ors: vec![$(AstBoxWrap::new($follow_logic_or),)+],
        }
    };
}

impl Ast for LogicOrExpr {}

impl AstParse for LogicOrExpr {
    fn parse(
        tokens: &mut VecDeque<Token>,
    ) -> Result<AstBoxWrap, Option<ParseError>> {
        let first_logic_or = LogicAndExpr::parse(tokens)?;
        let follow_logic_ors = {
            let mut ret = Vec::new();
            while let Some((TokenType::BeamBeam, line, pos)) =
                tokens.front().map(Token::bind_ref)
            {
                tokens.pop_front();
                ret.push(LogicAndExpr::parse(tokens).map_err(|e| {
                    if e.is_none() {
                        Some(ParseError::ExpectedToken { line, pos })
                    } else {
                        e
                    }
                })?);
            }
            ret
        };
        if follow_logic_ors.is_empty() {
            Ok(first_logic_or)
        } else {
            Ok(AstBoxWrap::new(LogicOrExpr {
                first_logic_or,
                follow_logic_ors,
            }))
        }
    }
}
