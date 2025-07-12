#![allow(clippy::wildcard_imports)]
use std::{
    collections::{HashMap, VecDeque},
    sync::LazyLock,
};

use tokenize::{Token, TokenKind, tokens::CharPosition};

use super::*;

/// Token list is inferred to be having one space between each token
fn build_token_deque(tokens: &[(TokenKind, Option<&str>)]) -> VecDeque<Token> {
    let mut column = 0;
    static TOKEN_LENS: LazyLock<HashMap<TokenKind, usize>> =
        LazyLock::new(|| {
            #[allow(clippy::wildcard_imports)]
            use TokenKind::*;
            // since we have a lot of single-character tokens.
            let single_char_tokens = [
                Plus, Dash, Star, Slash, Ampersand, Hat, Beam, Backslash,
                LParen, RParen, LBrace, RBrace, Dot, Colon, Semicolon, Comma,
                Equal, Less, Greater,
            ];
            let mut ret = HashMap::new();
            ret.extend(single_char_tokens.map(|kind| (kind, 1)).into_iter());
            ret.extend(
                [
                    (LessEqual, 2),
                    (GreaterEqual, 2),
                    (Ya, 2),
                    (Na, 2),
                    (And, 3),
                    (Or, 2),
                    (Not, 3),
                    (Eq, 2),
                    (Neq, 3),
                    (If, 2),
                    (Elif, 4),
                    (Else, 4),
                    (Let, 3),
                    (While, 5),
                    (Def, 3),
                    (Mut, 3),
                    (Proc, 4),
                    (Return, 6),
                    (BlockReturn, 12),
                    (Struct, 7),
                ]
                .into_iter(),
            );
            ret
        });

    tokens
        .iter()
        .copied()
        .map(|(kind, repr)| Token {
            kind,
            pos: CharPosition {
                line: 1,
                column: {
                    match repr {
                        Some(s) => column += s.len(),
                        None => {
                            column +=
                                TOKEN_LENS.get(&kind).copied().unwrap_or(1usize)
                        }
                    }
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
    // with all valid unary signs
    {
        let mut deque = build_token_deque(&[
            (TokenKind::Not, None),
            (TokenKind::Integer, Some("123")),
            (TokenKind::Dash, None),
            (TokenKind::Integer, Some("123")),
            (TokenKind::Plus, None),
            (TokenKind::Integer, Some("123")),
        ]);
        let asts = {
            let mut ret = Vec::new();
            let mut prev_pos = CharPosition { line: 1, column: 1 };
            while !deque.is_empty() {
                ret.push(
                    UnaryExpr::parse_tokens(&mut deque, prev_pos)
                        .inspect(|(_, pos)| prev_pos = *pos)
                        .map(|(ast, _)| ast)
                        .unwrap(),
                );
            }
            ret
        };
        assert_eq!(
            asts,
            [
                UnaryExpr {
                    op: UnaryOp::Negate,
                    val: Box::new(Expr::NoBlock(NoBlockExpr::Literal(
                        LiteralExpr::Integer(123)
                    ))),
                },
                UnaryExpr {
                    op: UnaryOp::Minus,
                    val: Box::new(Expr::NoBlock(NoBlockExpr::Literal(
                        LiteralExpr::Integer(123)
                    ))),
                },
                UnaryExpr {
                    op: UnaryOp::Plus,
                    val: Box::new(Expr::NoBlock(NoBlockExpr::Literal(
                        LiteralExpr::Integer(123)
                    ))),
                }
            ]
            .map(|val| Expr::NoBlock(NoBlockExpr::Unary(val)))
        );
    }
    // invalid unary sign cannot be tested until we're done with everything
    // else. Because, something like `if 1 == 1 { 1 } else { 2 }` is valid.
    // And, `if` isn't a valid unary sign, but this is a valid expression.
}

#[test]
fn binary_expr() {
    // passthrough to unary
    {
        let mut deque = build_token_deque(&[
            (TokenKind::Dash, None),
            (TokenKind::Integer, Some("123")),
        ]);
        let binary = BinaryExpr::parse_tokens(
            &mut deque,
            CharPosition { line: 1, column: 1 },
        )
        .map(|ret| ret.0)
        .unwrap();
        assert_eq!(
            binary,
            Expr::NoBlock(NoBlockExpr::Unary(UnaryExpr {
                op: UnaryOp::Minus,
                val: Box::new(Expr::NoBlock(NoBlockExpr::Literal(
                    LiteralExpr::Integer(123)
                )))
            }))
        );
    }
    // simple expr op expr
    {
        // 123 + 123
        let mut deque = build_token_deque(&[
            (TokenKind::Integer, Some("123")),
            (TokenKind::Plus, None),
            (TokenKind::Integer, Some("123")),
        ]);
        let last_pos = deque.back().unwrap().pos;
        let (binary, pos) = BinaryExpr::parse_tokens(
            &mut deque,
            CharPosition { line: 1, column: 1 },
        )
        .unwrap();
        assert_eq!(
            binary,
            Expr::NoBlock(NoBlockExpr::Binary(BinaryExpr {
                op: BinaryOp::Plus,
                lhs: Box::new(Expr::NoBlock(NoBlockExpr::Literal(
                    LiteralExpr::Integer(123)
                ))),
                rhs: Box::new(Expr::NoBlock(NoBlockExpr::Literal(
                    LiteralExpr::Integer(123)
                ))),
            }))
        );
        assert_eq!(pos, last_pos);
    }
    // 2 same-precedence operators
    {
        let mut deque = build_token_deque(&[
            (TokenKind::Integer, Some("123")),
            (TokenKind::Plus, None),
            (TokenKind::Integer, Some("123")),
            (TokenKind::Dash, None),
            (TokenKind::Integer, Some("123")),
        ]);
        let last_pos = deque.back().unwrap().pos;
        let (binary, pos) = BinaryExpr::parse_tokens(
            &mut deque,
            CharPosition { line: 1, column: 1 },
        )
        .unwrap();
        assert_eq!(
            binary,
            // (123 + 123) - 123
            Expr::NoBlock(NoBlockExpr::Binary(BinaryExpr {
                op: BinaryOp::Minus,
                lhs: Box::new(Expr::NoBlock(NoBlockExpr::Binary(BinaryExpr {
                    op: BinaryOp::Plus,
                    lhs: Box::new(Expr::NoBlock(NoBlockExpr::Literal(
                        LiteralExpr::Integer(123)
                    ))),
                    rhs: Box::new(Expr::NoBlock(NoBlockExpr::Literal(
                        LiteralExpr::Integer(123)
                    ))),
                }))),
                rhs: Box::new(Expr::NoBlock(NoBlockExpr::Literal(
                    LiteralExpr::Integer(123)
                ))),
            }))
        );
        assert_eq!(pos, last_pos);
    }
    // 2 different-precedence operators
    {
        // 123 + 123 * 123
        let mut deque = build_token_deque(&[
            (TokenKind::Integer, Some("123")),
            (TokenKind::Plus, None),
            (TokenKind::Integer, Some("123")),
            (TokenKind::Star, None),
            (TokenKind::Integer, Some("123")),
        ]);
        let last_pos = deque.back().unwrap().pos;
        let (binary, pos) = BinaryExpr::parse_tokens(
            &mut deque,
            CharPosition { line: 1, column: 1 },
        )
        .unwrap();
        assert_eq!(
            binary,
            // 123 + (123 * 123)
            Expr::NoBlock(NoBlockExpr::Binary(BinaryExpr {
                op: BinaryOp::Plus,
                lhs: Box::new(Expr::NoBlock(NoBlockExpr::Literal(
                    LiteralExpr::Integer(123)
                ))),
                rhs: Box::new(Expr::NoBlock(NoBlockExpr::Binary(BinaryExpr {
                    op: BinaryOp::Mul,
                    lhs: Box::new(Expr::NoBlock(NoBlockExpr::Literal(
                        LiteralExpr::Integer(123)
                    ))),
                    rhs: Box::new(Expr::NoBlock(NoBlockExpr::Literal(
                        LiteralExpr::Integer(123)
                    ))),
                })))
            }))
        );
        assert_eq!(pos, last_pos);
    }
    // right-associative vs left-associative
    {
        // hello = 123 + 123
        let mut deque = build_token_deque(&[
            (TokenKind::Identifier, Some("hello")),
            (TokenKind::Equal, None),
            (TokenKind::Integer, Some("123")),
            (TokenKind::Plus, None),
            (TokenKind::Integer, Some("123")),
        ]);
        let last_pos = deque.back().unwrap().pos;
        let (binary, pos) = BinaryExpr::parse_tokens(
            &mut deque,
            CharPosition { line: 1, column: 1 },
        )
        .unwrap();
        assert_eq!(
            binary,
            Expr::NoBlock(NoBlockExpr::Binary(BinaryExpr {
                op: BinaryOp::Assign,
                lhs: Box::new(Expr::NoBlock(NoBlockExpr::Literal(
                    LiteralExpr::Identifier(Box::from("hello")),
                ))),
                rhs: Box::new(Expr::NoBlock(NoBlockExpr::Binary(BinaryExpr {
                    op: BinaryOp::Plus,
                    lhs: Box::new(Expr::NoBlock(NoBlockExpr::Literal(
                        LiteralExpr::Integer(123)
                    ))),
                    rhs: Box::new(Expr::NoBlock(NoBlockExpr::Literal(
                        LiteralExpr::Integer(123)
                    ))),
                })))
            }))
        );
        assert_eq!(pos, last_pos);
    }
    // precedence versus unary expression
    {
        // 123 + +123
        let mut deque = build_token_deque(&[
            (TokenKind::Integer, Some("123")),
            (TokenKind::Plus, None),
            (TokenKind::Plus, None),
            (TokenKind::Integer, Some("123")),
        ]);
        let last_pos = deque.back().unwrap().pos;
        let (binary, pos) = BinaryExpr::parse_tokens(
            &mut deque,
            CharPosition { line: 1, column: 1 },
        )
        .unwrap();
        assert_eq!(
            binary,
            Expr::NoBlock(NoBlockExpr::Binary(BinaryExpr {
                op: BinaryOp::Plus,
                lhs: Box::new(Expr::NoBlock(NoBlockExpr::Literal(
                    LiteralExpr::Integer(123)
                ))),
                rhs: Box::new(Expr::NoBlock(NoBlockExpr::Unary(UnaryExpr {
                    op: UnaryOp::Plus,
                    val: Box::new(Expr::NoBlock(NoBlockExpr::Literal(
                        LiteralExpr::Integer(123)
                    )))
                })))
            }))
        );
        assert_eq!(pos, last_pos);
    }
}
