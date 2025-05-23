use super::*;
use crate::cond::*;

#[test]
fn comparison_test() {
    // fallthrough
    {
        let mut deque = new_test_deque![
            TokenType::Integer(3.to_string()),
            TokenType::Beam,
            TokenType::Integer(4.to_string()),
        ];
        let comp = ComparisonExpr::parse(&mut deque).unwrap();
        assert_ast_eq!(
            comp,
            new_bit_or_expr![PrimaryExpr::Integer(3), PrimaryExpr::Integer(4),]
        );
    }
    // precedence
    {
        let mut deque = new_test_deque![
            TokenType::Integer(3.to_string()),
            TokenType::Beam,
            TokenType::Integer(4.to_string()),
            TokenType::LPBraceEqual,
            TokenType::Integer(127.to_string()),
            TokenType::Beam,
            TokenType::Integer(129.to_string()),
        ];
        let comp = ComparisonExpr::parse(&mut deque).unwrap();
        assert_ast_eq!(
            comp,
            new_comparison_expr![
                new_bit_or_expr![
                    PrimaryExpr::Integer(3),
                    PrimaryExpr::Integer(4),
                ],
                ComparisonOp::LessEqual,
                new_bit_or_expr![
                    PrimaryExpr::Integer(127),
                    PrimaryExpr::Integer(129),
                ]
            ]
        );
    }
    // expected expression
    {
        let mut deque = new_test_deque![
            TokenType::Integer(3.to_string()),
            TokenType::RPBrace,
        ];
        let comp = ComparisonExpr::parse(&mut deque);
        assert_eq!(
            comp,
            Err(Some(ParseError::ExpectedToken { line: 1, pos: 2 }))
        );
    }
}
