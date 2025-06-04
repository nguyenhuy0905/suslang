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
}
