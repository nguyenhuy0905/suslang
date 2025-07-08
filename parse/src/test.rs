#![allow(clippy::wildcard_imports)]
use std::collections::VecDeque;

use tokenize::{Token, TokenKind, tokens::CharPosition};

use crate::{Expr, LiteralExpr, NoBlockExpr, UnaryExpr, parser::ParseExpr};

/// Token list is inferred to be having one space between each token
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
                    repr.inspect(|s| column += s.len() + 1);
                    column
                },
            },
            repr: repr.map(|s| Box::from(s)).or_else(|| None),
        })
        .collect()
}

#[test]
fn parse_literal() {
    let mut deque = build_token_deque(&[
        (TokenKind::Integer, Some("12345")),
        (TokenKind::Float, Some("123.45")),
        (TokenKind::String, Some("hello")),
        (TokenKind::Char, Some("c")),
        (TokenKind::Identifier, Some("sus1")),
        // while '(' <expr> ')' has the same precedence as any literal expr,
        // let's face it, that's not a literal expr.
    ]);

    let asts = {
        let mut ret = Vec::new();
        let mut prev_pos = CharPosition { line: 1, column: 1 };
        while !deque.is_empty() {
            ret.push(
                LiteralExpr::parse_tokens(&mut deque, prev_pos)
                    .inspect(|(_, pos)| prev_pos = *pos)
                    .map(|(tok, _)| tok)
                    .unwrap(),
            );
        }
        ret
    };

    assert_eq!(
        asts,
        [
            LiteralExpr::Integer(12345),
            LiteralExpr::Float(123.45),
            LiteralExpr::String(Box::from("hello")),
            LiteralExpr::Char('c'),
            LiteralExpr::Identifier(Box::from("sus1")),
        ]
        .map(|lit| Expr::NoBlock(NoBlockExpr::Literal(lit)))
    );
}

#[test]
fn parse_unary() {
    // just make a literal expr
    {
        let mut deque =
            build_token_deque(&[(TokenKind::String, Some("hello"))]);
        let unary = UnaryExpr::parse_tokens(
            &mut deque,
            CharPosition { line: 1, column: 1 },
        )
        .unwrap()
        .0;
        assert_eq!(
            unary,
            Expr::NoBlock(NoBlockExpr::Literal(LiteralExpr::String(
                Box::from("hello")
            )))
        );
    }
}
