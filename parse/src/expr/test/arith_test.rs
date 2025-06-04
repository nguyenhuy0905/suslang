use super::*;

#[test]
fn primary_types() {
    // actual primary expressions
    {
        let (line, pos) = (1, 1);
        let mut deque = new_test_deque![
            TokenType::Integer("3".to_string()),
            TokenType::Double("4.20".to_string()),
            TokenType::String("hello".to_string()),
            TokenType::Ya,
            TokenType::Na,
            TokenType::Identifier("sup".to_string()),
        ];

        let (prim1, line, pos) =
            PrimaryExpr::parse(&mut deque, line, pos).unwrap();
        assert_ast_eq!(prim1, PrimaryExpr::Integer(3));
        let (prim2, line, pos) =
            PrimaryExpr::parse(&mut deque, line, pos).unwrap();
        assert_ast_eq!(prim2, PrimaryExpr::Float(4.2));
        let (prim3, line, pos) =
            PrimaryExpr::parse(&mut deque, line, pos).unwrap();
        assert_ast_eq!(prim3, PrimaryExpr::String("hello".to_string()));
        let (prim4, line, pos) =
            PrimaryExpr::parse(&mut deque, line, pos).unwrap();
        assert_ast_eq!(prim4, PrimaryExpr::Boolean(true));
        let (prim5, line, pos) =
            PrimaryExpr::parse(&mut deque, line, pos).unwrap();
        assert_ast_eq!(prim5, PrimaryExpr::Boolean(false));
        let (prim6, ..) = PrimaryExpr::parse(&mut deque, line, pos).unwrap();
        assert_ast_eq!(prim6, PrimaryExpr::Identifier("sup".to_string()));
    }
    // proc-call has same precedence
    // proc-call simplest
    {
        let mut deque = new_test_deque![
            TokenType::Identifier("sus".to_string()),
            TokenType::LParen,
            TokenType::RParen,
        ];
        let (prim, .., pos) = PrimaryExpr::parse(&mut deque, 1, 1).unwrap();
        assert_ast_eq!(prim, new_proc_call_expr!("sus", ()));
        assert_eq!(pos, 3);
    }
    // proc-call with 1 param
    {
        let mut deque = new_test_deque![
            TokenType::Identifier("mofo".to_string()),
            TokenType::LParen,
            TokenType::Identifier("mofo".to_string()),
            TokenType::LParen,
            TokenType::Identifier("ehe".to_string()),
            TokenType::RParen,
            TokenType::RParen,
        ];
        let (prim, .., pos) = PrimaryExpr::parse(&mut deque, 1, 1).unwrap();
        assert_ast_eq!(
            prim,
            new_proc_call_expr!(
                "mofo".to_string(),
                (new_proc_call_expr!(
                    "mofo".to_string(),
                    (PrimaryExpr::Identifier("ehe".to_string()))
                ))
            )
        );
        assert_eq!(pos, 7);
    }
    // proc-call with multiple params
    {
        let mut deque = new_test_deque![
            TokenType::Identifier("mofo".to_string()),
            TokenType::LParen,
            TokenType::Identifier("sus".to_string()),
            TokenType::Comma,
            TokenType::Identifier("amogus".to_string()),
            TokenType::RParen
        ];
        let (prim, .., pos) = PrimaryExpr::parse(&mut deque, 1, 1).unwrap();
        assert_ast_eq!(
            prim,
            new_proc_call_expr!(
                "mofo".to_string(),
                (
                    PrimaryExpr::Identifier("sus".to_string()),
                    PrimaryExpr::Identifier("amogus".to_string()),
                )
            )
        );
        assert_eq!(pos, 6);
    }
    // proc-call without closing paren and params
    {
        let mut deque = new_test_deque![
            TokenType::Identifier("mofo".to_string()),
            TokenType::LParen,
        ];
        let prim = PrimaryExpr::parse(&mut deque, 1, 1);
        assert_eq!(
            prim,
            Err(Some(ParseError::ExpectedToken { line: 1, pos: 2 }))
        );
    }
    // proc-call without closing paren
    {
        let mut deque = new_test_deque![
            TokenType::Identifier("mofo".to_string()),
            TokenType::LParen,
            TokenType::Identifier("sus".to_string())
        ];
        let prim = PrimaryExpr::parse(&mut deque, 1, 1);
        assert_eq!(
            prim,
            Err(Some(ParseError::ExpectedToken { line: 1, pos: 3 }))
        );
    }
    // params not separated with comma
    {
        let mut deque = new_test_deque![
            TokenType::Identifier("mofo".to_string()),
            TokenType::LParen,
            TokenType::Identifier("sus".to_string()),
            TokenType::Identifier("amogus".to_string()),
            TokenType::RParen
        ];
        let prim = PrimaryExpr::parse(&mut deque, 1, 1);
        assert_eq!(
            prim,
            Err(Some(ParseError::UnexpectedToken(Token::new(
                TokenType::Identifier("amogus".to_string()),
                1,
                4
            ))))
        );
    }
}

#[test]
fn unary_expr() {
    let (line, pos) = (1, 1);
    // fallthrough
    {
        let mut deque = new_test_deque![TokenType::Integer("3".to_string())];
        let (un, ..) = UnaryExpr::parse(&mut deque, line, pos).unwrap();
        assert_ast_eq!(un, PrimaryExpr::Integer(3));
    }
    // coverage farming
    {
        let mut deque = new_test_deque![
            TokenType::Plus,
            TokenType::Integer("3".to_string())
        ];
        let (un1, ..) = UnaryExpr::parse(&mut deque, line, pos).unwrap();
        assert_ast_eq!(
            un1,
            UnaryExpr {
                primary: ExprBoxWrap::new(PrimaryExpr::Integer(3)),
                op: UnaryOp::Plus,
            }
        );
    }
    {
        let mut deque = new_test_deque![
            TokenType::Dash,
            TokenType::Integer("3".to_string())
        ];
        let (un2, ..) = UnaryExpr::parse(&mut deque, line, pos).unwrap();
        assert_ast_eq!(
            un2,
            UnaryExpr {
                primary: ExprBoxWrap::new(PrimaryExpr::Integer(3)),
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
        let (un3, ..) = UnaryExpr::parse(&mut deque, line, pos).unwrap();
        assert_ast_eq!(
            un3,
            UnaryExpr {
                primary: ExprBoxWrap::new(PrimaryExpr::Integer(3)),
                op: UnaryOp::Deref,
            }
        );
    }
    {
        let mut deque = new_test_deque![
            TokenType::Ampersand,
            TokenType::Integer("3".to_string())
        ];
        let (un4, ..) = UnaryExpr::parse(&mut deque, line, pos).unwrap();
        assert_ast_eq!(
            un4,
            UnaryExpr {
                primary: ExprBoxWrap::new(PrimaryExpr::Integer(3)),
                op: UnaryOp::Ref,
            }
        );
    }
    // expected expression
    {
        let mut deque = new_test_deque![TokenType::Dash];
        let un5 = UnaryExpr::parse(&mut deque, line, pos);
        assert_eq!(
            un5,
            Err(Some(ParseError::ExpectedToken { line: 1, pos: 1 }))
        );
    }
}

#[test]
fn factor_expr() {
    let (line, pos) = (1, 1);
    // fallthrough
    {
        let mut deque = VecDeque::from([Token::new(
            TokenType::Integer("3".to_string()),
            1,
            1,
        )]);
        let (fac, ..) = FactorExpr::parse(&mut deque, line, pos).unwrap();
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

        let (fac, ..) = FactorExpr::parse(&mut deque, line, pos).unwrap();
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
        let (fac, ..) = FactorExpr::parse(&mut deque, line, pos).unwrap();
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
        let fac = FactorExpr::parse(&mut deque, line, pos);
        assert_eq!(
            fac,
            Err(Some(ParseError::ExpectedToken { line: 1, pos: 2 }))
        );
    }
}

#[test]
fn term_expr() {
    let (line, pos) = (1, 1);
    // fallthrough
    {
        let mut deque = new_test_deque![
            TokenType::Integer("3".to_string()),
            TokenType::Star,
            TokenType::Integer("4".to_string()),
        ];
        let (term, ..) = TermExpr::parse(&mut deque, line, pos).unwrap();
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
        let (term, ..) = TermExpr::parse(&mut deque, line, pos).unwrap();
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
        let term = TermExpr::parse(&mut deque, line, pos);
        assert_eq!(
            term,
            Err(Some(ParseError::ExpectedToken { line: 1, pos: 2 }))
        );
    }
}

#[test]
fn bit_and_expr() {
    let (line, pos) = (1, 1);
    // fallthrough
    {
        let mut deque = new_test_deque![
            TokenType::Integer("3".to_string()),
            TokenType::Plus,
            TokenType::Integer("4".to_string())
        ];
        let (bit_and, ..) = BitAndExpr::parse(&mut deque, line, pos).unwrap();
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
        let (bit_and, ..) = BitAndExpr::parse(&mut deque, line, pos).unwrap();
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
        let bit_and = BitAndExpr::parse(&mut deque, line, pos);
        assert_eq!(
            bit_and,
            Err(Some(ParseError::ExpectedToken { line: 1, pos: 2 }))
        );
    }
}

#[test]
fn bit_xor_expr() {
    let (line, pos) = (1, 1);
    // fallthrough
    {
        let mut deque = new_test_deque![
            TokenType::Integer("3".to_string()),
            TokenType::Ampersand,
            TokenType::Integer("4".to_string()),
        ];
        let (bit_xor, ..) = BitXorExpr::parse(&mut deque, line, pos).unwrap();
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
        let (bit_xor, ..) = BitXorExpr::parse(&mut deque, line, pos).unwrap();
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
        let bit_xor = BitXorExpr::parse(&mut deque, line, pos);
        assert_eq!(
            bit_xor,
            Err(Some(ParseError::ExpectedToken { line: 1, pos: 2 }))
        );
    }
}

#[test]
fn bit_or_expr() {
    let (line, pos) = (1, 1);
    // fallthrough
    {
        let mut deque = new_test_deque![
            TokenType::Integer("3".to_string()),
            TokenType::Hat,
            TokenType::Integer("4".to_string()),
        ];
        let (bit_or, ..) = BitOrExpr::parse(&mut deque, line, pos).unwrap();
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
        let (bit_or, ..) = BitOrExpr::parse(&mut deque, line, pos).unwrap();
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
        let bit_or = BitOrExpr::parse(&mut deque, line, pos);
        assert_eq!(
            bit_or,
            Err(Some(ParseError::ExpectedToken { line: 1, pos: 2 }))
        );
    }
}
