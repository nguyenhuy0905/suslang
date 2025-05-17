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
fn unary_signed_num() {
    let mut deque = VecDeque::from([
        Token::new(TokenType::Dash, 0, 1),
        Token::new(TokenType::Integer("4".to_string()), 0, 2),
    ]);
    let unary = UnaryExpr::parse(&mut deque).unwrap();
    let cmp = UnaryExpr {
        primary: PrimaryExpr {
            typ: PrimaryExprType::LiteralInteger(4),
        },
        unary_op: Some(UnaryOp::Negate),
    };
    assert_eq!(unary, cmp);
}

#[test]
fn factor_simple_string() {
    let mut deque = VecDeque::from([Token::new(
        TokenType::String("hello".to_string()),
        0,
        1,
    )]);
    let fact = FactorExpr::parse(&mut deque).unwrap();
    let cmp = {
        let prim = PrimaryExpr {
            typ: PrimaryExprType::LiteralString("hello".to_string()),
        };
        let unary = UnaryExpr {
            primary: prim,
            unary_op: None,
        };
        FactorExpr {
            first_factor: unary,
            follow_factors: Vec::new(),
        }
    };
    assert_eq!(fact, cmp);
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
    let cmp = {
        let (first_un, second_un) = [2, 3]
            .map(|n| PrimaryExpr {
                typ: PrimaryExprType::LiteralInteger(n),
            })
            .map(|prim| UnaryExpr {
                primary: prim,
                unary_op: None,
            })
            .into();
        FactorExpr {
            first_factor: first_un,
            follow_factors: vec![(FacOp::Multiply, second_un)],
        }
    };
}

#[test]
fn factor_priority() {
    let mut deque = VecDeque::from([
        Token::new(TokenType::Integer("3".to_string()), 0, 1),
        Token::new(TokenType::Star, 0, 2),
        Token::new(TokenType::Dash, 0, 3),
        Token::new(TokenType::Integer("2".to_string()), 0, 4),
    ]);
    let fact = FactorExpr::parse(&mut deque).unwrap();
    let cmp = {
        let (first_un, second_un) = [3, 2]
            .map(|n| PrimaryExpr {
                typ: PrimaryExprType::LiteralInteger(n),
            })
            .map(|prim| UnaryExpr {
                primary: prim,
                unary_op: None,
            })
            .into();
        let second_un = UnaryExpr {
            primary: second_un.primary,
            unary_op: Some(UnaryOp::Negate),
        };
        FactorExpr {
            first_factor: first_un,
            follow_factors: vec![(FacOp::Multiply, second_un)],
        }
    };
    assert_eq!(fact, cmp);
}

#[test]
fn factor_unfinished() {
    let mut deque = VecDeque::from([
        Token::new(TokenType::Integer("2".to_string()), 0, 1),
        Token::new(TokenType::Star, 0, 2),
    ]);
    let fact = FactorExpr::parse(&mut deque);
    assert!(matches!(
        fact,
        Err(Some(ParseError {
            typ: ParseErrorType::ExpectedExpr,
            ..
        }))
    ));
}

#[test]
fn term_simple() {
    let mut deque = VecDeque::from([Token::new(
        TokenType::String("hello".to_string()),
        0,
        1,
    )]);
    let term = TermExpr::parse(&mut deque).unwrap();
    let cmp = {
        let prim = PrimaryExpr {
            typ: PrimaryExprType::LiteralString("hello".to_string()),
        };
        let unary = UnaryExpr {
            primary: prim,
            unary_op: None,
        };
        let fac = FactorExpr {
            first_factor: unary,
            follow_factors: Vec::new(),
        };
        TermExpr {
            first_term: fac,
            follow_terms: Vec::new(),
        }
    };
    assert_eq!(term, cmp);
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

#[test]
fn term_expected_expr() {
    let mut deque = VecDeque::from([
        Token::new(TokenType::Integer("3".to_string()), 0, 1),
        Token::new(TokenType::Dash, 0, 2),
    ]);
    let ret = TermExpr::parse(&mut deque);
    assert!(matches!(
        ret,
        Err(Some(ParseError {
            typ: ParseErrorType::ExpectedExpr,
            line: 0,
            pos: 2,
        }))
    ));
}

#[test]
fn term_priority() {
    // 3 + 2 * 4
    let mut deque = VecDeque::from([
        Token::new(TokenType::Integer("3".to_string()), 0, 1),
        Token::new(TokenType::Plus, 0, 2),
        Token::new(TokenType::Integer("2".to_string()), 0, 3),
        Token::new(TokenType::Star, 0, 4),
        Token::new(TokenType::Integer("4".to_string()), 0, 5),
    ]);
    let term = TermExpr::parse(&mut deque).unwrap();
    let cmp = {
        let (first_un, second_un, third_un) = [3, 2, 4]
            .map(|n| PrimaryExpr {
                typ: PrimaryExprType::LiteralInteger(n),
            })
            .map(|prim| UnaryExpr {
                primary: prim,
                unary_op: None,
            })
            .into();
        let second_fac = FactorExpr {
            first_factor: second_un,
            follow_factors: vec![(FacOp::Multiply, third_un)],
        };
        let first_fac = FactorExpr {
            first_factor: first_un,
            follow_factors: Vec::new(),
        };
        TermExpr {
            first_term: first_fac,
            follow_terms: vec![(TermOp::Plus, second_fac)],
        }
    };
    assert_eq!(term, cmp);
}
