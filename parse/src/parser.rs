use std::{collections::VecDeque, error::Error, fmt::Display};

use tokenize::{tokens::CharPosition, Token, TokenKind};

use crate::{
    BinaryExpr, BinaryOp, Expr, NoBlockExpr, PrimaryExpr, ProcCallExpr,
    UnaryExpr, UnaryOp,
};

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

impl ParseExpr for Expr {
    fn parse_tokens(
        tokens: &mut VecDeque<Token>,
        prev_pos: CharPosition,
    ) -> Result<(Expr, CharPosition), ParseError> {
        todo!()
    }
}

impl ParseExpr for PrimaryExpr {
    fn parse_tokens(
        tokens: &mut VecDeque<Token>,
        prev_pos: CharPosition,
    ) -> Result<(Expr, CharPosition), ParseError> {
        let lhs = tokens
            .pop_front()
            .ok_or(ParseError::ExpectedToken(prev_pos))
            .and_then(|tok| match tok.kind {
                TokenKind::Identifier => Ok((
                    Expr::NoBlock(NoBlockExpr::Primary(Self::Identifier(
                        tok.repr.unwrap(),
                    ))),
                    tok.pos,
                )),
                TokenKind::Integer => Ok((
                    Expr::NoBlock(NoBlockExpr::Primary(Self::Integer(
                        tok.repr.unwrap().parse().unwrap(),
                    ))),
                    tok.pos,
                )),
                TokenKind::Float => Ok((
                    Expr::NoBlock(NoBlockExpr::Primary(Self::Float(
                        tok.repr.unwrap().parse().unwrap(),
                    ))),
                    tok.pos,
                )),
                TokenKind::String => Ok((
                    Expr::NoBlock(NoBlockExpr::Primary(Self::String(
                        tok.repr.unwrap(),
                    ))),
                    tok.pos,
                )),
                TokenKind::Char => Ok((
                    Expr::NoBlock(NoBlockExpr::Primary(Self::Char(
                        tok.repr.unwrap().parse().unwrap(),
                    ))),
                    tok.pos,
                )),
                _ => {
                    tokens.push_front(tok);
                    Expr::parse_tokens(tokens, prev_pos)
                }
            });

        if tokens.front().map(|tok| tok.kind) != Some(TokenKind::LParen) {
            return lhs;
        }
        let lhs = lhs?;

        let proc_rhs = ProcCallExpr::parse_params(tokens, lhs.1)?;
        Ok((
            Expr::NoBlock(NoBlockExpr::ProcCall(ProcCallExpr {
                id_expr: Box::from(lhs.0),
                params: proc_rhs.0,
            })),
            proc_rhs.1,
        ))
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
            .map(|(op, op_pos)| (op, PrimaryExpr::parse_tokens(tokens, op_pos)))
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
            .unwrap_or_else(|| PrimaryExpr::parse_tokens(tokens, prev_pos))
    }
}

pub(crate) struct BinaryOpPrecedence {
    pub lhs: usize,
    pub rhs: usize,
}

impl BinaryOp {
    pub(crate) fn precedence(self) -> BinaryOpPrecedence {
        // lhs < rhs == right associative
        // lhs > rhs == left associative
        match self {
            BinaryOp::Assign => BinaryOpPrecedence { lhs: 1, rhs: 0 },
            BinaryOp::And | BinaryOp::Or => {
                BinaryOpPrecedence { lhs: 2, rhs: 3 }
            }
            BinaryOp::Eq
            | BinaryOp::Neq
            | BinaryOp::Lt
            | BinaryOp::Gt
            | BinaryOp::Le
            | BinaryOp::Ge => BinaryOpPrecedence { lhs: 4, rhs: 5 },
            BinaryOp::BitAnd | BinaryOp::BitXOr | BinaryOp::BitOr => {
                BinaryOpPrecedence { lhs: 6, rhs: 7 }
            }
            BinaryOp::Plus | BinaryOp::Minus => {
                BinaryOpPrecedence { lhs: 8, rhs: 9 }
            }
            BinaryOp::Mul | BinaryOp::Div => {
                BinaryOpPrecedence { lhs: 10, rhs: 11 }
            }
            BinaryOp::Member => BinaryOpPrecedence { lhs: 12, rhs: 13 },
        }
    }

    pub(crate) fn from_token_kind(kind: TokenKind) -> Option<Self> {
        match kind {
            TokenKind::Equal => Some(BinaryOp::Assign),
            TokenKind::And => Some(BinaryOp::And),
            TokenKind::Or => Some(BinaryOp::Or),
            TokenKind::Eq => Some(BinaryOp::Eq),
            TokenKind::Neq => Some(BinaryOp::Neq),
            TokenKind::LessEqual => Some(BinaryOp::Le),
            TokenKind::GreaterEqual => Some(BinaryOp::Ge),
            TokenKind::Less => Some(BinaryOp::Lt),
            TokenKind::Greater => Some(BinaryOp::Gt),
            TokenKind::Hat => Some(BinaryOp::BitXOr),
            TokenKind::Beam => Some(BinaryOp::BitOr),
            TokenKind::Ampersand => Some(BinaryOp::BitAnd),
            TokenKind::Plus => Some(BinaryOp::Plus),
            TokenKind::Dash => Some(BinaryOp::Minus),
            TokenKind::Star => Some(BinaryOp::Mul),
            TokenKind::Slash => Some(BinaryOp::Div),
            TokenKind::Dot => Some(BinaryOp::Member),
            _ => None,
        }
    }
}

impl BinaryExpr {
    // Given \<expr\> \<op\>, try parse right-hand side, then combine all that
    // into one \<expr\>.
    pub(crate) fn parse_with_op(
        lhs: Expr,
        op: BinaryOp,
        tokens: &mut VecDeque<Token>,
        prev_pos: CharPosition,
    ) -> Result<(Expr, CharPosition), ParseError> {
        let (mut rhs, mut rhs_pos) = UnaryExpr::parse_tokens(tokens, prev_pos)?;
        let mut min_precd = op.precedence().rhs;

        while let Some(next_op) = tokens
            .front()
            .and_then(|tok| BinaryOp::from_token_kind(tok.kind))
        {
            let next_op_precd = next_op.precedence();
            if next_op_precd.lhs > min_precd {
                tokens.pop_front();
                min_precd = next_op_precd.rhs;
                (rhs, rhs_pos) =
                    Self::parse_with_op(rhs, next_op, tokens, rhs_pos)?;
                continue;
            }

            return Ok((
                Expr::NoBlock(NoBlockExpr::Binary(Self {
                    op,
                    lhs: Box::from(lhs),
                    rhs: Box::from(rhs),
                })),
                rhs_pos,
            ));
        }

        Ok((
            Expr::NoBlock(NoBlockExpr::Binary(Self {
                op,
                lhs: Box::from(lhs),
                rhs: Box::from(rhs),
            })),
            rhs_pos,
        ))
    }
}

impl ParseExpr for BinaryExpr {
    fn parse_tokens(
        tokens: &mut VecDeque<Token>,
        prev_pos: CharPosition,
    ) -> Result<(Expr, CharPosition), ParseError> {
        let (mut lhs, mut lhs_pos) = UnaryExpr::parse_tokens(tokens, prev_pos)?;
        // damn it, borrow checker.
        while let Some(op) = tokens
            .front()
            .and_then(|tok| BinaryOp::from_token_kind(tok.kind))
            .inspect(|_| {
                tokens.pop_front();
            })
        {
            (lhs, lhs_pos) =
                BinaryExpr::parse_with_op(lhs, op, tokens, lhs_pos)?;
        }
        Ok((lhs, lhs_pos))
    }
}

impl ProcCallExpr {
    /// Parse the parameter list of a `ProcCallExpr`.
    pub(crate) fn parse_params(
        tokens: &mut VecDeque<Token>,
        prev_pos: CharPosition,
    ) -> Result<(Vec<Expr>, CharPosition), ParseError> {
        let lp_pos = tokens
            .pop_front()
            .ok_or(ParseError::ExpectedToken(prev_pos))
            .and_then(|tok| {
                if tok.kind == TokenKind::LParen {
                    Ok(tok.pos)
                } else {
                    Err(ParseError::UnexpectedToken(tok))
                }
            })?;
        let mut ret: Vec<Expr> = Vec::new();
        // will be position of right-paren when it exits the loop
        let mut last_pos = lp_pos;
        while !matches!(
            tokens.front().map(|tok| tok.kind),
            Some(TokenKind::RParen) | None
        ) {
            let (exp, exp_pos) = Expr::parse_tokens(tokens, last_pos)?;
            ret.push(exp);
            let next_tok = tokens
                .pop_front()
                .ok_or(ParseError::ExpectedToken(exp_pos))?;
            match next_tok.kind {
                TokenKind::RParen => {
                    last_pos = next_tok.pos;
                    break;
                }
                TokenKind::Comma => {
                    last_pos = next_tok.pos;
                }
                _ => return Err(ParseError::UnexpectedToken(next_tok)),
            }
        }
        Ok((ret, last_pos))
    }
}
