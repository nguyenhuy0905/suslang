use crate::block::{BlockExpr, ProcExpr};
use std::collections::HashSet;

use super::*;

#[test]
fn block_expr() {
    // simple, empty block
    {
        let mut deque = new_test_deque![TokenType::LCParen, TokenType::RCParen];
        let (block, .., pos) = BlockExpr::parse(&mut deque, 1, 1).unwrap();
        assert_ast_eq!(
            block,
            BlockExpr {
                statements: Vec::new()
            }
        );
        assert_eq!(pos, 2);
    }
    // simple, one-statement
    {
        let mut deque = new_test_deque![
            TokenType::LCParen,
            TokenType::Identifier("urmom".to_string()),
            TokenType::Semicolon,
            TokenType::RCParen
        ];
        let (block, .., pos) = BlockExpr::parse(&mut deque, 1, 1).unwrap();
        assert_ast_eq!(
            block,
            new_block_expr![new_stmt!(Expr(new_expr_semicolon_stmt!(
                PrimaryExpr::Identifier("urmom".to_string())
            )))]
        );
        assert_eq!(pos, 4);
    }
    // simple, multiple statements
    {
        let mut deque = new_test_deque![
            TokenType::LCParen,
            TokenType::Identifier("urmom".to_string()),
            TokenType::Semicolon,
            TokenType::Integer(1.to_string()),
            TokenType::EqualEqual,
            TokenType::Integer(1.to_string()),
            TokenType::Semicolon,
            TokenType::RCParen,
        ];
        let (block, .., pos) = BlockExpr::parse(&mut deque, 1, 1).unwrap();
        assert_ast_eq!(
            block,
            new_block_expr![
                new_stmt!(Expr(new_expr_semicolon_stmt!(
                    PrimaryExpr::Identifier("urmom".to_string())
                ))),
                new_stmt!(Expr(new_expr_semicolon_stmt!(
                    new_comparison_expr!(
                        PrimaryExpr::Integer(1),
                        ComparisonOp::Equal,
                        PrimaryExpr::Integer(1)
                    )
                ))),
            ]
        );
        assert_eq!(pos, 8);
    }
    // no closing brace
    {
        let mut deque = new_test_deque![
            TokenType::LCParen,
            TokenType::Integer(1.to_string()),
            TokenType::Semicolon
        ];
        let block = BlockExpr::parse(&mut deque, 1, 1);
        assert_eq!(
            block,
            Err(Some(ParseError::ExpectedToken { line: 1, pos: 3 }))
        );
    }
}

#[test]
fn proc_expr() {
    // simplest
    {
        let mut deque = new_test_deque![
            TokenType::Proc,
            TokenType::LParen,
            TokenType::RParen,
            TokenType::LCParen,
            TokenType::RCParen
        ];
        let (proc, .., pos) = ProcExpr::parse(&mut deque, 1, 1).unwrap();
        assert_ast_eq!(proc, new_proc_expr!((), new_block_expr!()));
        assert_eq!(pos, 5);
    }
    // single param
    {
        let mut deque = new_test_deque![
            TokenType::Proc,
            TokenType::LParen,
            TokenType::Identifier("urmom".to_string()),
            TokenType::RParen,
            TokenType::LCParen,
            TokenType::RCParen
        ];
        let (proc, .., pos) = ProcExpr::parse(&mut deque, 1, 1).unwrap();
        assert_ast_eq!(proc, new_proc_expr!(("urmom"), new_block_expr!()));
        assert_eq!(pos, 6);
    }
    // multiple (2) params
    {
        let mut deque = new_test_deque![
            TokenType::Proc,
            TokenType::LParen,
            TokenType::Identifier("urmom".to_string()),
            TokenType::Comma,
            TokenType::Identifier("yomama".to_string()),
            TokenType::Comma,
            TokenType::Identifier("joe".to_string()),
            TokenType::RParen,
            TokenType::LCParen,
            TokenType::RCParen,
        ];
        let (proc, .., pos) = ProcExpr::parse(&mut deque, 1, 1).unwrap();
        assert_ast_eq!(
            proc,
            new_proc_expr!(("urmom", "yomama", "joe"), new_block_expr!())
        );
        assert_eq!(pos, 10);
    }
    // proc only
    {
        let mut deque = new_test_deque![TokenType::Proc];
        let proc = ProcExpr::parse(&mut deque, 1, 1);
        assert_eq!(
            proc,
            Err(Some(ParseError::ExpectedToken { line: 1, pos: 1 }))
        );
    }
    // no closing paren
    {
        let mut deque = new_test_deque![
            TokenType::Proc,
            TokenType::LParen,
            TokenType::Identifier("urmom".to_string()),
            TokenType::Comma,
            TokenType::Identifier("yomama".to_string())
        ];
        let proc = ProcExpr::parse(&mut deque, 1, 1);
        assert_eq!(
            proc,
            Err(Some(ParseError::ExpectedToken { line: 1, pos: 5 }))
        );
    }
    // no block
    {
        let mut deque = new_test_deque![
            TokenType::Proc,
            TokenType::LParen,
            TokenType::Identifier("urmom".to_string()),
            TokenType::Comma,
            TokenType::Identifier("yomama".to_string()),
            TokenType::RParen,
        ];
        let proc = ProcExpr::parse(&mut deque, 1, 1);
        assert_eq!(
            proc,
            Err(Some(ParseError::ExpectedToken { line: 1, pos: 6 }))
        );
    }
    // trailing comma in param list
    {
        let mut deque = new_test_deque![
            TokenType::Proc,
            TokenType::LParen,
            TokenType::Identifier("urmom".to_string()),
            TokenType::Comma,
            TokenType::RParen,
            TokenType::LCParen,
            TokenType::RCParen,
        ];
        let proc = ProcExpr::parse(&mut deque, 1, 1);
        assert_eq!(
            proc,
            Err(Some(ParseError::UnexpectedToken(Token::new(
                TokenType::RParen,
                1,
                5
            ))))
        );
    }
}

#[test]
fn if_expr() {
    // simplest
    {
        // if 1 == 1 {}
        let mut deque = new_test_deque![
            TokenType::If,
            TokenType::Integer(1.to_string()),
            TokenType::EqualEqual,
            TokenType::Integer(1.to_string()),
            TokenType::LCParen,
            TokenType::RCParen,
        ];
        let (if_expr, .., pos) = IfExpr::parse(&mut deque, 1, 1).unwrap();
        assert_ast_eq!(
            if_expr,
            new_if_expr!(new_if_branch!(
                new_comparison_expr!(
                    PrimaryExpr::Integer(1),
                    ComparisonOp::Equal,
                    PrimaryExpr::Integer(1),
                ),
                BlockExpr {
                    statements: Vec::new(),
                }
            ))
        );
        assert_eq!(pos, 6);
    }
    // if-else
    {
        // if 1 == 1 {} else {}
        let mut deque = new_test_deque![
            TokenType::If,
            TokenType::Integer(1.to_string()),
            TokenType::EqualEqual,
            TokenType::Integer(1.to_string()),
            TokenType::LCParen,
            TokenType::RCParen,
            TokenType::Else,
            TokenType::LCParen,
            TokenType::RCParen,
        ];
        let (if_expr, .., pos) = IfExpr::parse(&mut deque, 1, 1).unwrap();
        assert_ast_eq!(
            if_expr,
            new_if_expr!(
                new_if_branch!(
                    new_comparison_expr!(
                        PrimaryExpr::Integer(1),
                        ComparisonOp::Equal,
                        PrimaryExpr::Integer(1),
                    ),
                    new_block_expr!()
                ),
                new_else_branch!(new_block_expr!())
            )
        );
        assert_eq!(pos, 9);
    }
    // if-elif
    {
        // if 1 == 1 {} elif 2 == 3 {}
        let mut deque = new_test_deque![
            TokenType::If,
            TokenType::Integer(1.to_string()),
            TokenType::EqualEqual,
            TokenType::Integer(1.to_string()),
            TokenType::LCParen,
            TokenType::RCParen,
            TokenType::Elif,
            TokenType::Integer(2.to_string()),
            TokenType::EqualEqual,
            TokenType::Integer(3.to_string()),
            TokenType::LCParen,
            TokenType::RCParen,
        ];
        let (if_expr, .., pos) = IfExpr::parse(&mut deque, 1, 1).unwrap();
        assert_ast_eq!(
            if_expr,
            new_if_expr!(
                new_if_branch!(
                    new_comparison_expr!(
                        PrimaryExpr::Integer(1),
                        ComparisonOp::Equal,
                        PrimaryExpr::Integer(1),
                    ),
                    new_block_expr!()
                ),
                [new_elif_branch!(
                    new_comparison_expr!(
                        PrimaryExpr::Integer(2),
                        ComparisonOp::Equal,
                        PrimaryExpr::Integer(3)
                    ),
                    new_block_expr!()
                ),]
            )
        );
        assert_eq!(pos, 12);
    }
    // if-elif-else
    {
        // if 1 == 1 {} elif 2 == 3 {} else {}
        let mut deque = new_test_deque![
            TokenType::If,
            TokenType::Integer(1.to_string()),
            TokenType::EqualEqual,
            TokenType::Integer(1.to_string()),
            TokenType::LCParen,
            TokenType::RCParen,
            TokenType::Elif,
            TokenType::Integer(2.to_string()),
            TokenType::EqualEqual,
            TokenType::Integer(3.to_string()),
            TokenType::LCParen,
            TokenType::RCParen,
            TokenType::Else,
            TokenType::LCParen,
            TokenType::RCParen,
        ];
        let (if_expr, .., pos) = IfExpr::parse(&mut deque, 1, 1).unwrap();
        assert_ast_eq!(
            if_expr,
            new_if_expr!(
                new_if_branch!(
                    new_comparison_expr!(
                        PrimaryExpr::Integer(1),
                        ComparisonOp::Equal,
                        PrimaryExpr::Integer(1),
                    ),
                    new_block_expr!()
                ),
                new_else_branch!(new_block_expr!()),
                [new_elif_branch!(
                    new_comparison_expr!(
                        PrimaryExpr::Integer(2),
                        ComparisonOp::Equal,
                        PrimaryExpr::Integer(3)
                    ),
                    new_block_expr!()
                ),]
            )
        );
        assert_eq!(pos, 15);
    }
    // if-multiple elifs-else
    {
        // if 1 == 1 {}
        // elif 2 == 3 {}
        // elif "hello" == "bonjour" {}
        // elif 1 + 1 == 2 - 1 {}
        // else {}
        let mut deque = new_test_deque![
            TokenType::If,
            TokenType::Integer(1.to_string()),
            TokenType::EqualEqual,
            TokenType::Integer(1.to_string()),
            TokenType::LCParen,
            TokenType::RCParen,
            TokenType::Elif,
            TokenType::Integer(2.to_string()),
            TokenType::EqualEqual,
            TokenType::Integer(3.to_string()),
            TokenType::LCParen,
            TokenType::RCParen,
            TokenType::Elif,
            TokenType::String("hello".to_string()),
            TokenType::EqualEqual,
            TokenType::String("bonjour".to_string()),
            TokenType::LCParen,
            TokenType::RCParen,
            TokenType::Elif,
            TokenType::Integer(1.to_string()),
            TokenType::Plus,
            TokenType::Integer(1.to_string()),
            TokenType::EqualEqual,
            TokenType::Integer(2.to_string()),
            TokenType::Dash,
            TokenType::Integer(1.to_string()),
            TokenType::LCParen,
            TokenType::RCParen,
            TokenType::Else,
            TokenType::LCParen,
            TokenType::RCParen,
        ];
        let deque_len = deque.len();
        let (if_expr, .., pos) = IfExpr::parse(&mut deque, 1, 1).unwrap();
        assert_ast_eq!(
            if_expr,
            new_if_expr!(
                new_if_branch!(
                    new_comparison_expr!(
                        PrimaryExpr::Integer(1),
                        ComparisonOp::Equal,
                        PrimaryExpr::Integer(1),
                    ),
                    new_block_expr!()
                ),
                new_else_branch!(new_block_expr!()),
                [
                    new_elif_branch!(
                        new_comparison_expr!(
                            PrimaryExpr::Integer(2),
                            ComparisonOp::Equal,
                            PrimaryExpr::Integer(3)
                        ),
                        new_block_expr!()
                    ),
                    new_elif_branch!(
                        new_comparison_expr!(
                            PrimaryExpr::String("hello".to_string()),
                            ComparisonOp::Equal,
                            PrimaryExpr::String("bonjour".to_string()),
                        ),
                        new_block_expr!()
                    ),
                    new_elif_branch!(
                        new_comparison_expr!(
                            new_term_expr!(
                                PrimaryExpr::Integer(1),
                                TermOp::Plus,
                                PrimaryExpr::Integer(1)
                            ),
                            ComparisonOp::Equal,
                            new_term_expr!(
                                PrimaryExpr::Integer(2),
                                TermOp::Minus,
                                PrimaryExpr::Integer(1)
                            )
                        ),
                        new_block_expr!()
                    ),
                ]
            )
        );
        assert_eq!(pos, deque_len);
    }
    // if with some content inside the block
    {
        // if 1 == 1 { print("yourmom"); }
        let mut deque = new_test_deque![
            TokenType::If,
            TokenType::Integer(1.to_string()),
            TokenType::EqualEqual,
            TokenType::Integer(1.to_string()),
            TokenType::LCParen,
            TokenType::Identifier("print".to_string()),
            TokenType::LParen,
            TokenType::String("yourmom".to_string()),
            TokenType::RParen,
            TokenType::Semicolon,
            TokenType::RCParen,
        ];
        let (if_expr, .., pos) = IfExpr::parse(&mut deque, 1, 1).unwrap();
        assert_ast_eq!(
            if_expr,
            new_if_expr!(new_if_branch!(
                new_comparison_expr!(
                    PrimaryExpr::Integer(1),
                    ComparisonOp::Equal,
                    PrimaryExpr::Integer(1)
                ),
                new_block_expr!(new_stmt!(Expr(new_expr_semicolon_stmt!(
                    new_proc_call_expr!(
                        "print".to_string(),
                        (PrimaryExpr::String("yourmom".to_string()))
                    )
                ))))
            ))
        );
        assert_eq!(pos, 11);
    }
}
