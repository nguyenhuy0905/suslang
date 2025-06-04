use crate::block::BlockExpr;
use std::collections::VecDeque;
use tokenize::{Token, TokenType};

use crate::{
    new_block_expr, new_comparison_expr, new_let_stmt, new_test_deque,
    ComparisonExpr, ComparisonOp, ExprBoxWrap, ParseError, PrimaryExpr,
};

use super::*;

#[test]
fn expr_semicolon_stmt() {
    // ur average stmt
    {
        let mut deque = new_test_deque![
            TokenType::Integer(1.to_string()),
            TokenType::EqualEqual,
            TokenType::Integer(1.to_string()),
            TokenType::Semicolon,
        ];
        let (expr_stmt, line, pos) =
            ExprValStmt::parse(&mut deque, 1, 1).unwrap();
        assert_eq!(
            expr_stmt.as_ref(),
            &ExprValStmt {
                expr: ExprBoxWrap::new(new_comparison_expr![
                    PrimaryExpr::Integer(1),
                    ComparisonOp::Equal,
                    PrimaryExpr::Integer(1),
                ])
            } as &dyn ExprStmtImpl
        );
        assert_eq!(line, 1);
        assert_eq!(pos, 4);
    }
}

#[test]
fn return_stmt() {
    // ur simple return stmt, without value
    {
        let mut deque =
            new_test_deque![TokenType::Return, TokenType::Semicolon];
        let (ret_stmt, line, pos) =
            ReturnStmt::parse(&mut deque, 1, 1).unwrap();
        assert_eq!(
            ret_stmt.as_ref(),
            &ReturnStmt { expr: None } as &dyn ExprStmtImpl
        );
        assert_eq!(line, 1);
        assert_eq!(pos, 2);
    }
    // ur simple return stmt, with value
    {
        let mut deque = new_test_deque![
            TokenType::Return,
            TokenType::Integer(1.to_string()),
            TokenType::EqualEqual,
            TokenType::Integer(1.to_string()),
            TokenType::Semicolon,
        ];
        let (ret_stmt, line, pos) =
            ReturnStmt::parse(&mut deque, 1, 1).unwrap();
        assert_eq!(
            ret_stmt.as_ref(),
            &ReturnStmt {
                expr: Some(ExprBoxWrap::new(new_comparison_expr![
                    PrimaryExpr::Integer(1),
                    ComparisonOp::Equal,
                    PrimaryExpr::Integer(1),
                ]))
            } as &dyn ExprStmtImpl
        );
        assert_eq!(line, 1);
        assert_eq!(pos, 5);
    }
    // no semicolon
    {
        let mut deque = new_test_deque![TokenType::Return,];
        let ret_stmt = ReturnStmt::parse(&mut deque, 1, 1);
        assert_eq!(
            ret_stmt,
            Err(ParseError::ExpectedToken { line: 1, pos: 1 })
        );
    }
}

#[test]
fn let_stmt() {
    // simplest case
    {
        let mut deque = new_test_deque![
            TokenType::Let,
            TokenType::Identifier("void".to_string()),
            TokenType::Equal,
            TokenType::LCParen,
            TokenType::RCParen,
        ];
        let (let_stmt, .., pos) = LetStmt::parse(&mut deque, 1, 1).unwrap();
        assert_eq!(
            let_stmt.as_ref() as &dyn DeclStmtImpl,
            &new_let_stmt!("void", new_block_expr!(), LetStmtMut::Immutable)
                as &dyn DeclStmtImpl,
        );
        assert_eq!(pos, 5);
    }
    // mutable
    {
        let mut deque = new_test_deque![
            TokenType::Let,
            TokenType::Mut,
            TokenType::Identifier("void".to_string()),
            TokenType::Equal,
            TokenType::LCParen,
            TokenType::RCParen,
        ];
        let (let_stmt, .., pos) = LetStmt::parse(&mut deque, 1, 1).unwrap();
        assert_eq!(
            let_stmt.as_ref() as &dyn DeclStmtImpl,
            &new_let_stmt!("void", new_block_expr!(), LetStmtMut::Mutable)
                as &dyn DeclStmtImpl,
        );
        assert_eq!(pos, 6);
    }
    // let followed by nothing at all
    {
        let mut deque = new_test_deque![TokenType::Let];
        let let_stmt = LetStmt::parse(&mut deque, 1, 1);
        assert_eq!(
            let_stmt,
            Err(ParseError::ExpectedToken { line: 1, pos: 1 })
        );
    }
    // same case as above but let mut
    {
        let mut deque = new_test_deque![TokenType::Let, TokenType::Mut];
        let let_stmt = LetStmt::parse(&mut deque, 1, 1);
        assert_eq!(
            let_stmt,
            Err(ParseError::ExpectedToken { line: 1, pos: 2 })
        );
    }
    // no RHS
    {
        let mut deque = new_test_deque![
            TokenType::Let,
            TokenType::Mut,
            TokenType::Identifier("sus".to_string()),
            TokenType::Equal
        ];
        let let_stmt = LetStmt::parse(&mut deque, 1, 1);
        // NOTE: should this return the starting position of the
        // statement, or the position where the error is encountered.
        assert_eq!(let_stmt, Err(ParseError::UnendedStmt { line: 1, pos: 1 }));
    }
}
