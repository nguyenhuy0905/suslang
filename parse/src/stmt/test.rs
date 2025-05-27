use crate::*;
use std::ops::Deref;
use tokenize::TokenType;

#[test]
fn var_decl_stmt() {
    // valid
    {
        let mut deque = new_test_deque![
            TokenType::Let,
            TokenType::Identifier("urmom".to_string()),
            TokenType::Equal,
            TokenType::Integer(3.to_string()),
            TokenType::Plus,
            TokenType::Integer(4.to_string()),
            TokenType::EqualEqual,
            TokenType::Integer(7.to_string()),
        ];
        let var_decl = VarDeclStmt::parse(&mut deque).unwrap();
        assert_ast_eq!(
            var_decl,
            new_var_decl_stmt!(
                "urmom",
                new_comparison_expr![
                    new_term_expr![
                        PrimaryExpr::Integer(3),
                        TermOp::Plus,
                        PrimaryExpr::Integer(4),
                    ],
                    ComparisonOp::Equal,
                    PrimaryExpr::Integer(7)
                ]
            )
        );
    }
}
