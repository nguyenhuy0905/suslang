use std::{
    collections::{HashMap, VecDeque},
    error::Error,
    fmt::Display,
    sync::LazyLock,
};

use tokenize::{Token, TokenKind, tokens::CharPosition};

use crate::{Expr, LiteralExpr, NoBlockExpr, UnaryExpr, UnaryOp};

#[derive(Debug, Clone, PartialEq)]
pub enum ParseError {
    ExpectedToken(CharPosition),
    UnexpectedToken(Token),
    Unended(CharPosition),
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

impl Error for ParseError {}

pub trait ParseExpr {
    /// Given a [`VecDeque`] of [`Token`]s, convert part of the iterator into
    /// a parse node.
    ///
    /// # Note
    /// - If there is no previous token (meaning no available `prev_pos`), use
    ///   [`CharPosition`] `{line: 1, column: 1}`.
    ///
    /// # Errors
    /// - If parsing fails, returns a [`ParseErrType`].
    ///
    /// # Return
    /// - The parsed struct.
    /// - The position of the last token popped.
    fn parse_tokens(
        tokens: &mut VecDeque<Token>,
        prev_pos: CharPosition,
    ) -> Result<(Expr, CharPosition), ParseError>;
}

impl ParseExpr for LiteralExpr {
    fn parse_tokens(
        tokens: &mut VecDeque<Token>,
        prev_pos: CharPosition,
    ) -> Result<(Expr, CharPosition), ParseError> {
        tokens
            .pop_front()
            .ok_or(ParseError::ExpectedToken(prev_pos))
            .and_then(|tok| match tok.kind {
                TokenKind::Identifier => Ok((
                    Expr::NoBlock(NoBlockExpr::Literal(Self::Identifier(
                        tok.repr.unwrap(),
                    ))),
                    tok.pos,
                )),
                TokenKind::Integer => Ok((
                    Expr::NoBlock(NoBlockExpr::Literal(Self::Integer(
                        tok.repr.unwrap().parse().unwrap(),
                    ))),
                    tok.pos,
                )),
                TokenKind::Float => Ok((
                    Expr::NoBlock(NoBlockExpr::Literal(Self::Float(
                        tok.repr.unwrap().parse().unwrap(),
                    ))),
                    tok.pos,
                )),
                TokenKind::String => Ok((
                    Expr::NoBlock(NoBlockExpr::Literal(Self::String(
                        tok.repr.unwrap(),
                    ))),
                    tok.pos,
                )),
                TokenKind::Char => Ok((
                    Expr::NoBlock(NoBlockExpr::Literal(Self::Char(
                        tok.repr.unwrap().parse().unwrap(),
                    ))),
                    tok.pos,
                )),
                _ => todo!(),
            })
    }
}

impl ParseExpr for UnaryExpr {
    fn parse_tokens(
        tokens: &mut VecDeque<Token>,
        prev_pos: CharPosition,
    ) -> Result<(Expr, CharPosition), ParseError> {
        tokens
            // peek
            .front()
            .and_then(|tok| {
                Some((
                    match tok.kind {
                        TokenKind::Plus => Some(UnaryOp::Plus),
                        TokenKind::Dash => Some(UnaryOp::Minus),
                        TokenKind::Not => Some(UnaryOp::Negate),
                        _ => None,
                    }?,
                    tok.pos,
                ))
            })
            // advance if the above succeeds
            .inspect(|_| {
                tokens.pop_front();
            })
            .map(|(op, op_pos)| (op, LiteralExpr::parse_tokens(tokens, op_pos)))
            .map(|(op, res)| {
                // construct the `UnaryExpr` only if the `LiteralExpr` parsing
                // step above succeeds.
                res.map(|(lit, lit_pos)| {
                    (
                        Expr::NoBlock(NoBlockExpr::Unary(UnaryExpr {
                            op,
                            val: Box::from(lit),
                        })),
                        lit_pos,
                    )
                })
            })
            .unwrap_or_else(|| LiteralExpr::parse_tokens(tokens, prev_pos))
    }
}

/// Binary operator precedence table.
/// (binary operator, (left-side binding power, right-side binding power))
static BIN_OP_PRECEDENCE: LazyLock<HashMap<TokenKind, (u16, u16)>> =
    LazyLock::new(|| {
        let mut ret = HashMap::new();
        let mut precedence = 0;
        let add_next_left_precedence =
            |map: &mut HashMap<TokenKind, (u16, u16)>,
             kinds: &[TokenKind],
             prec: &mut u16| {
                map.extend(
                    kinds
                        .iter()
                        .copied()
                        .map(|kind| (kind, (*prec, *prec + 1))),
                );
                *prec += 2;
            };
        let add_next_right_precedence =
            |map: &mut HashMap<TokenKind, (u16, u16)>,
             kinds: &[TokenKind],
             prec: &mut u16| {
                map.extend(
                    kinds
                        .iter()
                        .copied()
                        .map(|kind| (kind, (*prec + 1, *prec))),
                );
                *prec += 2;
            };

        // assignment
        add_next_right_precedence(
            &mut ret,
            &[TokenKind::Equal],
            &mut precedence,
        );
        // logic gate(?)
        add_next_left_precedence(
            &mut ret,
            &[TokenKind::And, TokenKind::Or],
            &mut precedence,
        );
        // comparison
        add_next_left_precedence(
            &mut ret,
            &[
                TokenKind::Eq,
                TokenKind::Neq,
                TokenKind::Less,
                TokenKind::LessEqual,
                TokenKind::Greater,
                TokenKind::GreaterEqual,
            ],
            &mut precedence,
        );
        // bitwise
        add_next_left_precedence(
            &mut ret,
            &[TokenKind::Hat, TokenKind::Ampersand, TokenKind::Beam],
            &mut precedence,
        );
        // term
        add_next_left_precedence(
            &mut ret,
            &[TokenKind::Plus, TokenKind::Dash],
            &mut precedence,
        );
        // factor
        add_next_left_precedence(
            &mut ret,
            &[TokenKind::Star, TokenKind::Slash],
            &mut precedence,
        );
        // member access
        add_next_left_precedence(&mut ret, &[TokenKind::Dot], &mut precedence);

        ret
    });
