use crate::block::BlockExpr;

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
}
