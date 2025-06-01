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
    pub lhs: ExprBoxWrap,
    pub rhs: ExprBoxWrap,
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

impl ExprAst for ComparisonExpr {}

impl ExprParse for ComparisonExpr {
    fn parse(
        tokens: &mut VecDeque<Token>,
        line: usize,
        pos: usize,
    ) -> Result<(ExprBoxWrap, usize, usize), Option<ParseError>> {
        let (lhs, lhs_ln, lhs_pos) = BitOrExpr::parse(tokens, line, pos)?;
        if let Some((Some(op), line, pos)) =
            tokens.front().map(Token::bind_ref).map(|(typ, line, pos)| {
                (
                    match typ {
                        TokenType::EqualEqual => Some(ComparisonOp::Equal),
                        TokenType::BangEqual => Some(ComparisonOp::NotEqual),
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
            let (rhs, rhs_ln, rhs_pos) = BitOrExpr::parse(tokens, line, pos)
                .map_err(|e| {
                    if e.is_none() {
                        Some(ParseError::ExpectedToken { line, pos })
                    } else {
                        e
                    }
                })?;
            Ok((
                ExprBoxWrap::new(ComparisonExpr { lhs, rhs, op }),
                rhs_ln,
                rhs_pos,
            ))
        } else {
            Ok((lhs, lhs_ln, lhs_pos))
        }
    }
}

#[macro_export]
macro_rules! new_comparison_expr {
    ($lhs:expr,$op:expr,$rhs:expr$(,)?) => {
        ComparisonExpr {
            lhs: ExprBoxWrap::new($lhs),
            rhs: ExprBoxWrap::new($rhs),
            op: $op,
        }
    };
    ($lhs:expr,$rhs:expr,$op:expr) => {
        ComparisonExpr {
            lhs: ExprBoxWrap::new($lhs),
            rhs: ExprBoxWrap::new($rhs),
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
    pub first_logic_and: ExprBoxWrap,
    pub follow_logic_ands: Vec<ExprBoxWrap>,
}

#[macro_export]
macro_rules! new_logic_and_expr {
    ($first_logic_and:expr $(,$follow_logic_and:expr)+$(,)?) => {
        LogicAndExpr {
            first_logic_and:ExprBoxWrap::new($first_logic_and),
            follow_logic_ands:vec![$(ExprBoxWrap::new($follow_logic_and),)+],
        }
    };
}

impl ExprAst for LogicAndExpr {}

impl ExprParse for LogicAndExpr {
    fn parse(
        tokens: &mut VecDeque<Token>,
        line: usize,
        pos: usize,
    ) -> Result<(ExprBoxWrap, usize, usize), Option<ParseError>> {
        let (first_logic_and, first_ln, first_pos) =
            ComparisonExpr::parse(tokens, line, pos)?;
        let (follow_logic_ands, follow_ln, follow_pos) = {
            let mut ret = Vec::new();
            let (mut ret_ln, mut ret_pos) = (first_ln, first_pos);
            while let Some((TokenType::AmpersandAmpersand, line, pos)) =
                tokens.front().map(Token::bind_ref)
            {
                tokens.pop_front();
                let (log_and, and_ln, and_pos) =
                    ComparisonExpr::parse(tokens, line, pos).map_err(|e| {
                        if e.is_none() {
                            Some(ParseError::ExpectedToken { line, pos })
                        } else {
                            e
                        }
                    })?;
                ret.push(log_and);
                (ret_ln, ret_pos) = (and_ln, and_pos);
            }
            (ret, ret_ln, ret_pos)
        };
        if follow_logic_ands.is_empty() {
            Ok((first_logic_and, follow_ln, follow_pos))
        } else {
            Ok((
                ExprBoxWrap::new(LogicAndExpr {
                    first_logic_and,
                    follow_logic_ands,
                }),
                follow_ln,
                follow_pos,
            ))
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
    pub first_logic_or: ExprBoxWrap,
    pub follow_logic_ors: Vec<ExprBoxWrap>,
}

#[macro_export]
macro_rules! new_logic_or_expr {
    ($first_logic_or:expr $(,$follow_logic_or:expr)+ $(,)?) => {
        LogicOrExpr {
            first_logic_or: ExprBoxWrap::new($first_logic_or),
            follow_logic_ors: vec![$(ExprBoxWrap::new($follow_logic_or),)+],
        }
    };
}

impl ExprAst for LogicOrExpr {}

impl ExprParse for LogicOrExpr {
    fn parse(
        tokens: &mut VecDeque<Token>,
        line: usize,
        pos: usize,
    ) -> Result<(ExprBoxWrap, usize, usize), Option<ParseError>> {
        let (first_logic_or, first_ln, first_pos) =
            LogicAndExpr::parse(tokens, line, pos)?;
        let (follow_logic_ors, follow_ln, follow_pos) = {
            let mut ret = Vec::new();
            let (mut ret_ln, mut ret_pos) = (first_ln, first_pos);
            while let Some((TokenType::BeamBeam, line, pos)) =
                tokens.front().map(Token::bind_ref)
            {
                tokens.pop_front();
                let (logor, or_ln, or_pos) =
                    LogicAndExpr::parse(tokens, line, pos).map_err(|e| {
                        if e.is_none() {
                            Some(ParseError::ExpectedToken { line, pos })
                        } else {
                            e
                        }
                    })?;
                ret.push(logor);
                (ret_ln, ret_pos) = (or_ln, or_pos);
            }
            (ret, ret_ln, ret_pos)
        };
        if follow_logic_ors.is_empty() {
            Ok((first_logic_or, follow_ln, follow_pos))
        } else {
            Ok((
                ExprBoxWrap::new(LogicOrExpr {
                    first_logic_or,
                    follow_logic_ors,
                }),
                follow_ln,
                follow_pos,
            ))
        }
    }
}
