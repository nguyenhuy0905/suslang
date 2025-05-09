use super::*;

#[test]
fn literal_number() {
    let mut lit_num =
        VecDeque::from([Token::new(TokenType::Integer("69".into()), 0, 0)]);
    let prim = PrimaryExpr::parse(&mut lit_num).unwrap();
    assert_eq!(prim.typ, PrimaryExprType::LiteralInteger(69));
}

#[test]
fn literal_string() {
    let mut lit_str =
        VecDeque::from([Token::new(TokenType::String("hello".into()), 0, 0)]);
    let prim = PrimaryExpr::parse(&mut lit_str).unwrap();
    assert_eq!(prim.typ, PrimaryExprType::LiteralString("hello".into()));
}

#[test]
fn unary_simple_num() {
    let mut deque = VecDeque::from([
        Token::new(TokenType::Dash, 0, 0),
        Token::new(TokenType::Integer("420".to_string()), 0, 1),
    ]);
    let unar = UnaryExpr::parse(&mut deque).unwrap();
    assert!(matches!(
        unar,
        UnaryExpr {
            unary_op: Some(UnaryOp::Negate),
            primary: PrimaryExpr {
                typ: PrimaryExprType::LiteralInteger(420),
                ..
            },
            ..
        }
    ));
}

#[test]
fn unary_expected_tok_err() {
    let mut deque = VecDeque::from([Token::new(TokenType::Dash, 0, 0)]);
    let unar = UnaryExpr::parse(&mut deque);
    assert!(matches!(
        unar,
        Err(Some(ParseError {
            typ: ParseErrorType::ExpectedExpr,
            ..
        }))
    ));
}

#[test]
fn unary_simple_string() {
    let mut deque = VecDeque::from([Token::new(
        TokenType::String("hello".to_string()),
        0,
        1,
    )]);
    let unar = UnaryExpr::parse(&mut deque);
    assert!(unar.is_ok());
    assert!(matches!(
        unar,
        Ok(UnaryExpr {
            primary: PrimaryExpr {
                typ: PrimaryExprType::LiteralString(_),
                ..
            },
            ..
        })
    ));
}

#[test]
fn factor_simple_string() {
    let mut deque = VecDeque::from([Token::new(
        TokenType::String("hello".to_string()),
        0,
        1,
    )]);
    let fact = FactorExpr::parse(&mut deque);
    assert!(fact.is_ok());
    // assert_matches is unstable ATM
    assert!(matches!(
        fact,
        Ok(FactorExpr {
            first_factor: UnaryExpr {
                primary: PrimaryExpr {
                    typ: PrimaryExprType::LiteralString(_),
                    ..
                },
                ..
            },
            ..
        })
    ));
}

#[test]
fn factor_simple_mult() {
    // 2 * 3
    let mut deque = VecDeque::from([
        Token::new(TokenType::Dash, 0, 1),
        Token::new(TokenType::Integer("2".to_string()), 0, 2),
        Token::new(TokenType::Star, 0, 3),
        Token::new(TokenType::Integer("3".to_string()), 0, 4),
    ]);
    let fact = FactorExpr::parse(&mut deque).unwrap();
    // 2
    assert!(matches!(
        fact,
        FactorExpr {
            first_factor: UnaryExpr {
                primary: PrimaryExpr {
                    typ: PrimaryExprType::LiteralInteger(2),
                    ..
                },
                unary_op: Some(UnaryOp::Negate),
            },
            ..
        }
    ));
    // * 3
    assert_eq!(
        fact.follow_factors,
        vec![(
            FacOp::Multiply,
            UnaryExpr {
                primary: PrimaryExpr {
                    typ: PrimaryExprType::LiteralInteger(3),
                },
                unary_op: None
            }
        )]
    );
}

#[test]
fn term_simple() {
    let mut deque = VecDeque::from([Token::new(
        TokenType::String("hello".to_string()),
        0,
        1,
    )]);
    let term = TermExpr::parse(&mut deque).unwrap();
    if let TermExpr {
        first_term:
            FactorExpr {
                first_factor:
                    UnaryExpr {
                        primary:
                            PrimaryExpr {
                                typ: PrimaryExprType::LiteralString(s),
                                ..
                            },
                        ..
                    },
                ..
            },
        ..
    } = term
    {
        assert_eq!(s, "hello");
    } else {
        panic!("Type {term:?} doesn't match literal string expression");
    }
}

#[test]
fn term_simple_plus() {
    let mut deque = VecDeque::from([
        Token::new(TokenType::Integer("1".to_string()), 0, 1),
        Token::new(TokenType::Plus, 0, 3),
        Token::new(TokenType::Integer("3".to_string()), 0, 5),
    ]);
    let term = TermExpr::parse(&mut deque).unwrap();
    let cmp = {
        let first_prim = PrimaryExpr {
            typ: PrimaryExprType::LiteralInteger(1),
        };
        let second_prim = PrimaryExpr {
            typ: PrimaryExprType::LiteralInteger(3),
        };
        let first_unary = UnaryExpr {
            primary: first_prim,
            unary_op: None,
        };
        let second_unary = UnaryExpr {
            primary: second_prim,
            unary_op: None,
        };
        let first_fac = FactorExpr {
            first_factor: first_unary,
            follow_factors: Vec::new(),
        };
        let second_fac = FactorExpr {
            first_factor: second_unary,
            follow_factors: Vec::new(),
        };
        TermExpr {
            first_term: first_fac,
            follow_terms: vec![(TermOp::Plus, second_fac)],
        }
    };
    assert_eq!(term, cmp);
}
