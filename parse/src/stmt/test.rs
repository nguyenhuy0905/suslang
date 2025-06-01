use std::collections::VecDeque;
use tokenize::{Token, TokenType};

use crate::{
    new_comparison_expr, new_test_deque, ComparisonExpr, ComparisonOp,
    ExprBoxWrap, ParseError, PrimaryExpr,
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
            ExprSemicolonStmt::parse(&mut deque, 1, 1).unwrap();
        assert_eq!(
            expr_stmt.as_ref(),
            &ExprSemicolonStmt {
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
    // no semicolon
    {
        let mut deque = new_test_deque![
            TokenType::Integer(1.to_string()),
            TokenType::EqualEqual,
            TokenType::Integer(1.to_string()),
        ];
        let expr_stmt = ExprSemicolonStmt::parse(&mut deque, 1, 1);
        assert_eq!(
            expr_stmt,
            Err(ParseError::ExpectedToken { line: 1, pos: 3 })
        );
    }
    // no expression, just semicolon
    {
        let mut deque = new_test_deque![TokenType::Semicolon,];
        let expr_stmt = ExprSemicolonStmt::parse(&mut deque, 1, 1);
        // this error is percolated up from PrimaryExpr::parse
        assert_eq!(
            expr_stmt,
            Err(ParseError::UnexpectedToken(Token::new(
                TokenType::Semicolon,
                1,
                1
            )))
        );
    }
}
