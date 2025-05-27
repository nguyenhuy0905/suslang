use super::*;

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

#[test]
fn logic_and_expr() {
    // fallthrough
    {
        let mut deque = new_test_deque![
            TokenType::Integer(3.to_string()),
            TokenType::LPBraceEqual,
            TokenType::Integer(4.to_string()),
        ];
        let logic_and = LogicAndExpr::parse(&mut deque).unwrap();
        assert_ast_eq!(
            logic_and,
            new_comparison_expr![
                PrimaryExpr::Integer(3),
                ComparisonOp::LessEqual,
                PrimaryExpr::Integer(4),
            ]
        );
    }
    // precedence
    {
        let mut deque = new_test_deque![
            TokenType::Integer(3.to_string()),
            TokenType::RPBraceEqual,
            TokenType::Integer(4.to_string()),
            TokenType::AmpersandAmpersand,
            TokenType::Integer(5.to_string()),
            TokenType::LPBraceEqual,
            TokenType::Integer(6.to_string()),
        ];
        let logic_and = LogicAndExpr::parse(&mut deque).unwrap();
        assert_ast_eq!(
            logic_and,
            new_logic_and_expr![
                new_comparison_expr![
                    PrimaryExpr::Integer(3),
                    ComparisonOp::GreaterEqual,
                    PrimaryExpr::Integer(4),
                ],
                new_comparison_expr![
                    PrimaryExpr::Integer(5),
                    ComparisonOp::LessEqual,
                    PrimaryExpr::Integer(6),
                ],
            ]
        );
    }
    // expected expression
    {
        let mut deque = new_test_deque![
            TokenType::Integer(3.to_string()),
            TokenType::LPBrace,
        ];
        let logic_and = LogicAndExpr::parse(&mut deque);
        assert_eq!(
            logic_and,
            Err(Some(ParseError::ExpectedToken { line: 1, pos: 2 }))
        );
    }
}

#[test]
fn logic_or_expr() {
    // fallthrough
    {
        let mut deque = new_test_deque![
            TokenType::Integer(3.to_string()),
            TokenType::AmpersandAmpersand,
            TokenType::Integer(4.to_string()),
        ];
        let logic_or = LogicOrExpr::parse(&mut deque).unwrap();
        assert_ast_eq!(
            logic_or,
            new_logic_and_expr![
                PrimaryExpr::Integer(3),
                PrimaryExpr::Integer(4),
            ]
        );
    }
    // precedence
    {
        let mut deque = new_test_deque![
            TokenType::Integer(3.to_string()),
            TokenType::AmpersandAmpersand,
            TokenType::Integer(4.to_string()),
            TokenType::BeamBeam,
            TokenType::Integer(3.to_string()),
            TokenType::AmpersandAmpersand,
            TokenType::Integer(4.to_string()),
        ];
        let logic_or = LogicOrExpr::parse(&mut deque).unwrap();
        assert_ast_eq!(
            logic_or,
            new_logic_or_expr![
                new_logic_and_expr![
                    PrimaryExpr::Integer(3),
                    PrimaryExpr::Integer(4),
                ],
                new_logic_and_expr![
                    PrimaryExpr::Integer(3),
                    PrimaryExpr::Integer(4),
                ]
            ]
        );
    }
    // expected expression
    {
        let mut deque = new_test_deque![
            TokenType::Integer(3.to_string()),
            TokenType::BeamBeam,
        ];
        let logic_or = LogicOrExpr::parse(&mut deque);
        assert_eq!(
            logic_or,
            Err(Some(ParseError::ExpectedToken { line: 1, pos: 2 }))
        );
    }
}
