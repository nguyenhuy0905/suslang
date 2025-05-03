use super::*;

#[test]
fn literal_number() {
    let mut lit_num =
        VecDeque::from([Token::new(TokenType::Integer("69".into()), 0, 0)]);
    let prim = PrimaryExpr::parse(&mut lit_num).unwrap();
    assert_eq!(prim.typ, PrimaryExprType::LiteralNum(69));
}

#[test]
fn literal_string() {
    let mut lit_str =
        VecDeque::from([Token::new(TokenType::String("hello".into()), 0, 0)]);
    let prim = PrimaryExpr::parse(&mut lit_str).unwrap();
    assert_eq!(prim.typ, PrimaryExprType::LiteralString("hello".into()));
}

#[test]
fn unary_simple() {
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
                typ: PrimaryExprType::LiteralNum(420),
            }
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
