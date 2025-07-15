use std::collections::VecDeque;

use tokenize::{tokens::CharPosition, Token, TokenKind};

use super::{ParseError, ParseTokens};
use crate::{BlockReturnStmt, Expr, NoBlockExprStmt, ReturnStmt, Stmt};

impl ParseTokens for Stmt {
    type Node = Stmt;
    fn parse_tokens(
        tokens: &mut VecDeque<Token>,
        prev_pos: CharPosition,
    ) -> Result<(Stmt, CharPosition), ParseError> {
        todo!()
    }
}

impl ParseTokens for ReturnStmt {
    type Node = Self;

    fn parse_tokens(
        tokens: &mut VecDeque<Token>,
        prev_pos: CharPosition,
    ) -> Result<(Self::Node, CharPosition), ParseError> {
        let ret_pos = tokens
            .pop_front()
            .ok_or(ParseError::ExpectedToken(prev_pos))
            .and_then(|tok| match tok.kind {
                TokenKind::Return => Ok(tok.pos),
                _ => Err(ParseError::UnexpectedToken(tok)),
            })?;
        let (val, val_pos) = Expr::parse_tokens(tokens, ret_pos)?;
        Ok((Self { val }, val_pos))
    }
}

impl ParseTokens for BlockReturnStmt {
    type Node = Self;

    fn parse_tokens(
        tokens: &mut VecDeque<Token>,
        prev_pos: CharPosition,
    ) -> Result<(Self::Node, CharPosition), ParseError> {
        let ret_pos = tokens
            .pop_front()
            .ok_or(ParseError::ExpectedToken(prev_pos))
            .and_then(|tok| match tok.kind {
                TokenKind::BlockReturn => Ok(tok.pos),
                _ => Err(ParseError::UnexpectedToken(tok)),
            })?;
        let (val, val_pos) = Expr::parse_tokens(tokens, ret_pos)?;
        Ok((Self { val }, val_pos))
    }
}

impl ParseTokens for NoBlockExprStmt {
    type Node = Self;

    fn parse_tokens(
        tokens: &mut VecDeque<Token>,
        prev_pos: CharPosition,
    ) -> Result<(Self::Node, CharPosition), ParseError> {
        tokens
            .front()
            .ok_or(ParseError::ExpectedToken(prev_pos))
            // fuck you borrow checker
            .map(|tok| tok.kind)
            .and_then(|kind| match kind {
                TokenKind::Return => {
                    let (ret, ret_pos) =
                        ReturnStmt::parse_tokens(tokens, prev_pos)?;
                    Ok((Self::Return(ret), ret_pos))
                }
                TokenKind::BlockReturn => {
                    let (blk_ret, blk_ret_pos) =
                        BlockReturnStmt::parse_tokens(tokens, prev_pos)?;
                    Ok((Self::BlockReturn(blk_ret), blk_ret_pos))
                }
                _ => {
                    let (exp, exp_pos) = Expr::parse_tokens(tokens, prev_pos)?;
                    match exp {
                        Expr::NoBlock(noblk) => {
                            Ok((Self::Expr(noblk), exp_pos))
                        }
                        Expr::WithBlock(_) => panic!(
                            "NoBlockExprStmt::parse_tokens: expected no-block \
                                statement, got {exp:?}"
                        ),
                    }
                }
            })
            .and_then(|(ret, ret_pos)| {
                tokens
                    .pop_front()
                    .ok_or(ParseError::ExpectedToken(ret_pos))
                    .and_then(|tok| {
                        if tok.kind == TokenKind::Semicolon {
                            Ok((ret, tok.pos))
                        } else {
                            Err(ParseError::UnexpectedToken(tok))
                        }
                    })
            })
    }
}
