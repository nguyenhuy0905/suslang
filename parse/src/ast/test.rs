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

#[test]
fn simple_bit_and() {
    let mut deque =
        VecDeque::from([Token::new(TokenType::Integer("3".to_string()), 0, 1)]);
    let bitand = BitAndExpr::parse(&mut deque).unwrap();
    let cmp = {
        let un_exp = UnaryExpr {
            primary: PrimaryExpr {
                typ: PrimaryExprType::LiteralInteger(3),
            },
            unary_op: None,
        };
        let fac_exp = FactorExpr {
            first_factor: un_exp,
            follow_factors: Vec::new(),
        };
        let term_exp = TermExpr {
            first_term: fac_exp,
            follow_terms: Vec::new(),
        };
        BitAndExpr {
            first_term: term_exp,
            follow_terms: Vec::new(),
        }
    };
    assert_eq!(bitand, cmp);
}

#[test]
fn two_clause_bit_and() {
    let mut deque = VecDeque::from([
        Token::new(TokenType::Integer("3".to_string()), 0, 1),
        Token::new(TokenType::Ampersand, 0, 1),
        Token::new(TokenType::Integer("69".to_string()), 0, 1),
    ]);
    let bitand = BitAndExpr::parse(&mut deque).unwrap();
    let cmp = {
        let first_un = UnaryExpr {
            primary: PrimaryExpr {
                typ: PrimaryExprType::LiteralInteger(3),
            },
            unary_op: None,
        };
        let second_un = UnaryExpr {
            primary: PrimaryExpr {
                typ: PrimaryExprType::LiteralInteger(69),
            },
            unary_op: None,
        };
        let first_fac = FactorExpr {
            first_factor: first_un,
            follow_factors: Vec::new(),
        };
        let second_fac = FactorExpr {
            first_factor: second_un,
            follow_factors: Vec::new(),
        };
        let first_term = TermExpr {
            first_term: first_fac,
            follow_terms: Vec::new(),
        };
        let second_term = TermExpr {
            first_term: second_fac,
            follow_terms: Vec::new(),
        };
        BitAndExpr {
            first_term,
            follow_terms: vec![second_term],
        }
    };
    assert_eq!(bitand, cmp);
}

#[test]
fn bit_and_expected_term() {
    let mut deque = VecDeque::from([
        Token::new(TokenType::Integer("3".to_string()), 0, 1),
        Token::new(TokenType::Ampersand, 0, 2),
    ]);
    let ret = BitAndExpr::parse(&mut deque);
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
fn bit_and_chain() {
    let mut deque: VecDeque<_> = {
        let mut counter: usize = 0;
        [
            TokenType::Integer("3".to_string()),
            TokenType::Ampersand,
            TokenType::Integer("4".to_string()),
            TokenType::Ampersand,
            TokenType::Integer("7".to_string()),
        ]
        .map(|typ| {
            Token::new(typ, 0, {
                counter += 1;
                counter
            })
        })
        .into()
    };
    let bitand = BitAndExpr::parse(&mut deque).unwrap();
    let cmp = {
        let (term1, term2, term3) = [3, 4, 7]
            .map(|num| PrimaryExpr {
                typ: PrimaryExprType::LiteralInteger(num),
            })
            .map(|primary| UnaryExpr {
                primary,
                unary_op: None,
            })
            .map(|prim| FactorExpr {
                first_factor: prim,
                follow_factors: Vec::new(),
            })
            .map(|fac| TermExpr {
                first_term: fac,
                follow_terms: Vec::new(),
            })
            .into();
        BitAndExpr {
            first_term: term1,
            follow_terms: vec![term2, term3],
        }
    };
    assert_eq!(bitand, cmp);
}

#[test]
fn bit_or_precedence() {
    // 3 & 4 | 5 & 6
    // eqv to (3 & 4) | (5 & 6)
    let mut deque: VecDeque<_> = {
        let mut counter: usize = 0;
        [
            TokenType::Integer("3".to_string()),
            TokenType::Ampersand,
            TokenType::Integer("4".to_string()),
            TokenType::Beam,
            TokenType::Integer("5".to_string()),
            TokenType::Ampersand,
            TokenType::Integer("6".to_string()),
        ]
        .map(|tok_typ| {
            counter += 1;
            Token::new(tok_typ, 0, counter)
        })
        .into()
    };
    let bitor = BitOrExpr::parse(&mut deque).unwrap();
    let cmp = {
        let (term1, term2, term3, term4) = [3, 4, 5, 6]
            .map(|num| PrimaryExpr {
                typ: PrimaryExprType::LiteralInteger(num),
            })
            .map(|primary| UnaryExpr {
                primary,
                unary_op: None,
            })
            .map(|unary| FactorExpr {
                first_factor: unary,
                follow_factors: Vec::new(),
            })
            .map(|fac| TermExpr {
                first_term: fac,
                follow_terms: Vec::new(),
            })
            .into();
        let bitand1 = BitAndExpr {
            first_term: term1,
            follow_terms: vec![term2],
        };
        let bitand2 = BitAndExpr {
            first_term: term3,
            follow_terms: vec![term4],
        };
        BitOrExpr {
            first_bit_and: bitand1,
            follow_bit_ands: vec![bitand2],
        }
    };
    assert_eq!(bitor, cmp);
}

#[test]
fn comp_expr() {
    // 3 + 4 == 7
    let mut deque = VecDeque::from([
        Token::new(TokenType::Integer("3".to_string()), 0, 1),
        Token::new(TokenType::Plus, 0, 2),
        Token::new(TokenType::Integer("4".to_string()), 0, 3),
        Token::new(TokenType::EqualEqual, 0, 4),
        Token::new(TokenType::Integer("7".to_string()), 0, 5),
    ]);
    let comp_exp = ComparisonExpr::parse(&mut deque).unwrap();
    let cmp = {
        let (first_fac, second_fac, third_fac) = [3, 4, 7]
            .map(|num| PrimaryExprType::LiteralInteger(num))
            .map(|prim| PrimaryExpr { typ: prim })
            .map(|prim| UnaryExpr {
                primary: prim,
                unary_op: None,
            })
            .map(|unary| FactorExpr {
                first_factor: unary,
                follow_factors: Vec::new(),
            })
            .into();
        let first_term = TermExpr {
            first_term: first_fac,
            follow_terms: vec![(TermOp::Plus, second_fac)],
        };
        let second_term = TermExpr {
            first_term: third_fac,
            follow_terms: Vec::new(),
        };
        let (first_bit_or, second_bit_or) = [first_term, second_term]
            .map(|term| BitAndExpr {
                first_term: term,
                follow_terms: Vec::new(),
            })
            .map(|bitand| BitOrExpr {
                first_bit_and: bitand,
                follow_bit_ands: Vec::new(),
            })
            .into();
        ComparisonExpr {
            first_comp: first_bit_or,
            second_comp: Some(second_bit_or),
            op: Some(ComparisonOp::Equal),
        }
    };
    assert_eq!(comp_exp, cmp);
}

#[test]
fn comp_exp_expected_expr() {
    // 3 + 4 ==
    let mut deque = VecDeque::from([
        Token::new(TokenType::Integer("3".to_string()), 0, 1),
        Token::new(TokenType::Plus, 0, 2),
        Token::new(TokenType::Integer("4".to_string()), 0, 3),
        Token::new(TokenType::EqualEqual, 0, 4),
    ]);
    let ret = ComparisonExpr::parse(&mut deque);
    assert!(matches!(
        ret,
        Err(Some(ParseError {
            typ: ParseErrorType::ExpectedExpr,
            line: 0,
            pos: 4,
        }))
    ));
}
