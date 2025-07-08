use std::{
    collections::{HashMap, VecDeque},
    error::Error,
    fmt::Display,
    sync::LazyLock,
};

use super::*;
use tokenize::{Token, TokenKind, tokens::CharPosition};

#[derive(Debug, Clone, PartialEq)]
pub enum ParseError {
    ExpectedToken(CharPosition),
    UnexpectedToken(Token),
    Unended(CharPosition),
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl Error for ParseError {}

trait ParseExpr {
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

/// Operator precedence table.
static OP_PRECEDENCE: LazyLock<HashMap<TokenKind, u16>> = LazyLock::new(|| {
    let mut ret = HashMap::new();
    let mut precedence = 0;
    let mut add_next_precedence = |kinds: &[TokenKind]| {
        ret.extend(kinds.into_iter().copied().map(|kind| (kind, precedence)));
        precedence = precedence + 1;
    };

    add_next_precedence(&[TokenKind::BeamBeam, TokenKind::AmpersandAmpersand]);
    add_next_precedence(&[TokenKind::EqualEqual, TokenKind::BangEqual]);
    add_next_precedence(&[
        TokenKind::Hat,
        TokenKind::Ampersand,
        TokenKind::Beam,
    ]);
    add_next_precedence(&[TokenKind::Plus, TokenKind::Dash]);
    add_next_precedence(&[TokenKind::Star, TokenKind::Slash]);

    ret
});

impl ParseExpr for LiteralExpr {
    fn parse_tokens(
        tokens: &mut VecDeque<Token>,
        prev_pos: CharPosition,
    ) -> Result<(Expr, CharPosition), ParseError> {
        tokens
            .pop_front()
            .ok_or(ParseError::ExpectedToken(prev_pos))
            .and_then(|tok| {
                use TokenKind::*;
                match tok.kind {
                    Identifier => Ok((
                        Expr::NoBlock(NoBlockExpr::Literal(Self::Identifier(
                            tok.repr.unwrap(),
                        ))),
                        tok.pos,
                    )),
                    Integer => Ok((
                        Expr::NoBlock(NoBlockExpr::Literal(Self::Integer(
                            tok.repr.unwrap().parse().unwrap(),
                        ))),
                        tok.pos,
                    )),
                    Float => Ok((
                        Expr::NoBlock(NoBlockExpr::Literal(Self::Float(
                            tok.repr.unwrap().parse().unwrap(),
                        ))),
                        tok.pos,
                    )),
                    String => Ok((
                        Expr::NoBlock(NoBlockExpr::Literal(Self::Identifier(
                            tok.repr.unwrap(),
                        ))),
                        tok.pos,
                    )),
                    Char => Ok((
                        Expr::NoBlock(NoBlockExpr::Literal(Self::Identifier(
                            tok.repr.unwrap(),
                        ))),
                        tok.pos,
                    )),
                    _ => unreachable!(),
                }
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
                use TokenKind::*;
                Some((
                    match tok.kind {
                        Plus => Some(UnaryOp::Plus),
                        Dash => Some(UnaryOp::Minus),
                        Bang => Some(UnaryOp::Negate),
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
