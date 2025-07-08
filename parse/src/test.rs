#![allow(clippy::wildcard_imports)]
use std::collections::VecDeque;

use tokenize::{Token, TokenKind, tokens::CharPosition};

use crate::{Expr, LiteralExpr, NoBlockExpr, parser::ParseExpr};

fn build_token_deque(tokens: &[(TokenKind, Option<&str>)]) -> VecDeque<Token> {
    let mut column = 0;
    tokens
        .iter()
        .copied()
        .map(|(kind, repr)| Token {
            kind,
            pos: CharPosition {
                line: 1,
                column: {
                    column += 1;
                    column
                },
            },
            repr: repr.map(|s| Box::from(s)).or_else(|| None),
        })
        .collect()
}

#[test]
fn parse_literal() {
    let mut deque = build_token_deque(&[(TokenKind::Integer, Some("12345"))]);
    let lit1 = LiteralExpr::parse_tokens(
        &mut deque,
        CharPosition { line: 1, column: 1 },
    )
    .map(|(ast, _)| ast);
    assert_eq!(
        lit1,
        Ok(Expr::NoBlock(NoBlockExpr::Literal(LiteralExpr::Integer(
            12345
        ))))
    );
}
