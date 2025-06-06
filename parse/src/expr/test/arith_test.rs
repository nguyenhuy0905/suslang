use super::*;

#[test]
fn primary_types() {
    let mut deque = new_test_deque![
        TokenType::Integer("3".to_string()),
        TokenType::Double("4.20".to_string()),
        TokenType::String("hello".to_string()),
        TokenType::Ya,
        TokenType::Na,
    ];

    let prim1 = PrimaryExpr::parse(&mut deque).unwrap();
    assert_ast_eq!(prim1, PrimaryExpr::Integer(3));
    let prim2 = PrimaryExpr::parse(&mut deque).unwrap();
    assert_ast_eq!(prim2, PrimaryExpr::Float(4.2));
    let prim3 = PrimaryExpr::parse(&mut deque).unwrap();
    assert_ast_eq!(prim3, PrimaryExpr::String("hello".to_string()));
    let prim4 = PrimaryExpr::parse(&mut deque).unwrap();
    assert_ast_eq!(prim4, PrimaryExpr::Boolean(true));
    let prim5 = PrimaryExpr::parse(&mut deque).unwrap();
    assert_ast_eq!(prim5, PrimaryExpr::Boolean(false));
}

#[test]
fn unary_expr() {
    // fallthrough
    {
        let mut deque = new_test_deque![TokenType::Integer("3".to_string())];
        let un = UnaryExpr::parse(&mut deque).unwrap();
        assert_ast_eq!(un, PrimaryExpr::Integer(3));
    }
    // coverage farming
    {
        let mut deque = new_test_deque![
            TokenType::Plus,
            TokenType::Integer("3".to_string())
        ];
        let un1 = UnaryExpr::parse(&mut deque).unwrap();
        assert_ast_eq!(
            un1,
            UnaryExpr {
                primary: AstBoxWrap::new(PrimaryExpr::Integer(3)),
                op: UnaryOp::Plus,
            }
        );
    }
    {
        let mut deque = new_test_deque![
            TokenType::Dash,
            TokenType::Integer("3".to_string())
        ];
        let un2 = UnaryExpr::parse(&mut deque).unwrap();
        assert_ast_eq!(
            un2,
            UnaryExpr {
                primary: AstBoxWrap::new(PrimaryExpr::Integer(3)),
                op: UnaryOp::Minus,
            }
        );
    }
    {
        // deref only makes sense if going with an lvalue. But, hey, we are
        // checking that later.
        let mut deque = new_test_deque![
            TokenType::Star,
            TokenType::Integer("3".to_string())
        ];
        let un3 = UnaryExpr::parse(&mut deque).unwrap();
        assert_ast_eq!(
            un3,
            UnaryExpr {
                primary: AstBoxWrap::new(PrimaryExpr::Integer(3)),
                op: UnaryOp::Deref,
            }
        );
    }
    {
        let mut deque = new_test_deque![
            TokenType::Ampersand,
            TokenType::Integer("3".to_string())
        ];
        let un4 = UnaryExpr::parse(&mut deque).unwrap();
        assert_ast_eq!(
            un4,
            UnaryExpr {
                primary: AstBoxWrap::new(PrimaryExpr::Integer(3)),
                op: UnaryOp::Ref,
            }
        );
    }
    // expected expression
    {
        let mut deque = new_test_deque![TokenType::Dash];
        let un5 = UnaryExpr::parse(&mut deque);
        assert_eq!(
            un5,
            Err(Some(ParseError::ExpectedToken { line: 1, pos: 1 }))
        );
    }
}

#[test]
fn factor_expr() {
    // fallthrough
    {
        let mut deque = VecDeque::from([Token::new(
            TokenType::Integer("3".to_string()),
            1,
            1,
        )]);
        let fac = FactorExpr::parse(&mut deque).unwrap();
        assert_ast_eq!(fac, PrimaryExpr::Integer(3));
    }
    // simple non-fallthrough
    {
        let mut deque = new_test_deque![
            TokenType::Integer("3".to_string()),
            TokenType::Star,
            TokenType::Integer("4".to_string()),
            TokenType::Star,
            TokenType::Integer("4".to_string())
        ];

        let fac = FactorExpr::parse(&mut deque).unwrap();
        assert_ast_eq!(
            fac,
            new_factor_expr![
                PrimaryExpr::Integer(3),
                FactorOp::Multiply,
                PrimaryExpr::Integer(4),
                FactorOp::Multiply,
                PrimaryExpr::Integer(4),
            ]
        );
    }
    // unary precedence
    {
        let mut deque = new_test_deque![
            TokenType::Integer("3".to_string()),
            TokenType::Star,
            TokenType::Star,
            TokenType::Integer("4".to_string())
        ];
        let fac = FactorExpr::parse(&mut deque).unwrap();
        assert_ast_eq!(
            fac,
            new_factor_expr![
                PrimaryExpr::Integer(3),
                FactorOp::Multiply,
                new_unary_expr!(PrimaryExpr::Integer(4), UnaryOp::Deref),
            ]
        );
    }
    // expected expression
    {
        let mut deque = new_test_deque![
            TokenType::Integer("3".to_string()),
            TokenType::Star
        ];
        let fac = FactorExpr::parse(&mut deque);
        assert_eq!(
            fac,
            Err(Some(ParseError::ExpectedToken { line: 1, pos: 2 }))
        );
    }
}

#[test]
fn term_expr() {
    // fallthrough
    {
        let mut deque = new_test_deque![
            TokenType::Integer("3".to_string()),
            TokenType::Star,
            TokenType::Integer("4".to_string()),
        ];
        let term = TermExpr::parse(&mut deque).unwrap();
        assert_ast_eq!(
            term,
            new_factor_expr![
                PrimaryExpr::Integer(3),
                FactorOp::Multiply,
                PrimaryExpr::Integer(4),
            ]
        );
    }
    // term precedence
    {
        // 3 + -4 * 5
        let mut deque = new_test_deque![
            TokenType::Integer("3".to_string()),
            TokenType::Plus,
            TokenType::Dash,
            TokenType::Integer("4".to_string()),
            TokenType::Star,
            TokenType::Integer("5".to_string()),
        ];
        let term = TermExpr::parse(&mut deque).unwrap();
        assert_ast_eq!(
            term,
            new_term_expr![
                PrimaryExpr::Integer(3),
                TermOp::Plus,
                new_factor_expr![
                    new_unary_expr!(PrimaryExpr::Integer(4), UnaryOp::Minus),
                    FactorOp::Multiply,
                    PrimaryExpr::Integer(5),
                ],
            ]
        );
    }
    // expected expression
    {
        let mut deque = new_test_deque![
            TokenType::Integer("3".to_string()),
            TokenType::Plus,
        ];
        let term = TermExpr::parse(&mut deque);
        assert_eq!(
            term,
            Err(Some(ParseError::ExpectedToken { line: 1, pos: 2 }))
        );
    }
}

#[test]
fn bit_and_expr() {
    // fallthrough
    {
        let mut deque = new_test_deque![
            TokenType::Integer("3".to_string()),
            TokenType::Plus,
            TokenType::Integer("4".to_string())
        ];
        let bit_and = BitAndExpr::parse(&mut deque).unwrap();
        assert_ast_eq!(
            bit_and,
            new_term_expr![
                PrimaryExpr::Integer(3),
                TermOp::Plus,
                PrimaryExpr::Integer(4),
            ]
        );
    }
    // precedence
    {
        let mut deque = new_test_deque![
            TokenType::Integer("3".to_string()),
            TokenType::Plus,
            TokenType::Integer("4".to_string()),
            TokenType::Ampersand,
            TokenType::Integer("5".to_string()),
            TokenType::Dash,
            TokenType::Integer("6".to_string()),
        ];
        let bit_and = BitAndExpr::parse(&mut deque).unwrap();
        assert_ast_eq!(
            bit_and,
            new_bit_and_expr![
                new_term_expr![
                    PrimaryExpr::Integer(3),
                    TermOp::Plus,
                    PrimaryExpr::Integer(4),
                ],
                new_term_expr![
                    PrimaryExpr::Integer(5),
                    TermOp::Minus,
                    PrimaryExpr::Integer(6),
                ],
            ]
        );
    }
    // expected expression
    {
        let mut deque = new_test_deque![
            TokenType::Integer("3".to_string()),
            TokenType::Ampersand,
        ];
        let bit_and = BitAndExpr::parse(&mut deque);
        assert_eq!(
            bit_and,
            Err(Some(ParseError::ExpectedToken { line: 1, pos: 2 }))
        );
    }
}

#[test]
fn bit_xor_expr() {
    // fallthrough
    {
        let mut deque = new_test_deque![
            TokenType::Integer("3".to_string()),
            TokenType::Ampersand,
            TokenType::Integer("4".to_string()),
        ];
        let bit_xor = BitXorExpr::parse(&mut deque).unwrap();
        assert_ast_eq!(
            bit_xor,
            new_bit_and_expr![PrimaryExpr::Integer(3), PrimaryExpr::Integer(4),]
        );
    }
    // precedence
    {
        let mut deque = new_test_deque![
            TokenType::Integer("3".to_string()),
            TokenType::Ampersand,
            TokenType::Integer("4".to_string()),
            TokenType::Hat,
            TokenType::Integer("3".to_string()),
            TokenType::Ampersand,
            TokenType::Integer("4".to_string()),
        ];
        let bit_xor = BitXorExpr::parse(&mut deque).unwrap();
        assert_ast_eq!(
            bit_xor,
            new_bit_xor_expr![
                new_bit_and_expr![
                    PrimaryExpr::Integer(3),
                    PrimaryExpr::Integer(4),
                ],
                new_bit_and_expr![
                    PrimaryExpr::Integer(3),
                    PrimaryExpr::Integer(4),
                ],
            ]
        );
    }
    // expected expression
    {
        let mut deque = new_test_deque![
            TokenType::Integer("3".to_string()),
            TokenType::Hat,
        ];
        let bit_xor = BitXorExpr::parse(&mut deque);
        assert_eq!(
            bit_xor,
            Err(Some(ParseError::ExpectedToken { line: 1, pos: 2 }))
        );
    }
}

#[test]
fn bit_or_expr() {
    // fallthrough
    {
        let mut deque = new_test_deque![
            TokenType::Integer("3".to_string()),
            TokenType::Hat,
            TokenType::Integer("4".to_string()),
        ];
        let bit_or = BitOrExpr::parse(&mut deque).unwrap();
        assert_ast_eq!(
            bit_or,
            new_bit_xor_expr![PrimaryExpr::Integer(3), PrimaryExpr::Integer(4),]
        );
    }
    // precedence
    {
        let mut deque = new_test_deque![
            TokenType::Integer("3".to_string()),
            TokenType::Hat,
            TokenType::Integer("4".to_string()),
            TokenType::Beam,
            TokenType::Integer("3".to_string()),
            TokenType::Hat,
            TokenType::Integer("4".to_string()),
        ];
        let bit_or = BitOrExpr::parse(&mut deque).unwrap();
        assert_ast_eq!(
            bit_or,
            new_bit_or_expr![
                new_bit_xor_expr![
                    PrimaryExpr::Integer(3),
                    PrimaryExpr::Integer(4),
                ],
                new_bit_xor_expr![
                    PrimaryExpr::Integer(3),
                    PrimaryExpr::Integer(4),
                ],
            ]
        );
    }
    // expected expression
    {
        let mut deque = new_test_deque![
            TokenType::Integer("3".to_string()),
            TokenType::Beam,
        ];
        let bit_or = BitOrExpr::parse(&mut deque);
        assert_eq!(
            bit_or,
            Err(Some(ParseError::ExpectedToken { line: 1, pos: 2 }))
        );
    }
}
