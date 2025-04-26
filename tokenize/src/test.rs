use super::*;

#[test]
fn ignore_space() {
    let ret = tokenize("     \r\t");
    assert!(ret.is_ok());
    assert!(ret.unwrap().is_empty());
}

#[test]
fn non_ascii_begin_token() {
    let ret = tokenize("\u{f4a2} russ \u{f4a2}");
    assert!(ret.is_err());
    assert!(matches!(
        ret.err().unwrap().err_type,
        TokenizeErrorType::InvalidToken(_)
    ));
}

#[test]
fn simple_string() {
    let ret = tokenize("\"hello\"");
    assert!(ret.is_ok());
    let ur = ret.unwrap();
    assert!(!ur.is_empty());
    assert!(matches!(
        ur.first().unwrap().token_type(),
        TokenType::String(_)
    ));
    if let TokenType::String(s) = ur.first().unwrap().token_type() {
        assert_eq!(s, "hello");
        assert_ne!(s, "hell");
    }
}

#[test]
fn unicode_string() {
    let ret = tokenize("\"hello\u{f4a2}!\"");
    assert!(ret.is_ok());
    let ur = ret.unwrap();
    assert!(!ur.is_empty());
    assert!(matches!(
        ur.first().unwrap().token_type(),
        TokenType::String(_)
    ));
    if let TokenType::String(s) = ur.first().unwrap().token_type() {
        assert_eq!(s, "hello\u{f4a2}!");
        assert_ne!(s, "hell");
    }
}

#[test]
fn string_error() {
    let ret = tokenize("\"");
    assert!(ret.is_err());
    assert!(matches!(
        ret,
        Err(TokenizeError {
            err_type: TokenizeErrorType::UnfinishedToken,
            line: 1,
            pos: 0,
            ..
        })
    ));
}

#[test]
fn empty_string() {
    let ret = tokenize("\"\"");
    assert!(ret.is_ok());
    let ur = ret.unwrap();
    assert!(!ur.is_empty());
    assert!(matches!(
        ur.first().unwrap().token_type(),
        TokenType::String(_)
    ));
    if let TokenType::String(s) = ur.first().unwrap().token_type() {
        assert!(s.is_empty());
    }
}

#[test]
fn simple_identifier() {
    let ret = tokenize("hello");
    assert!(ret.is_ok());
    let ur = ret.unwrap();
    assert!(!ur.is_empty());
    assert!(matches!(
        ur.first().unwrap().token_type(),
        TokenType::Identifier(_)
    ));
    if let TokenType::Identifier(s) = ur.first().unwrap().token_type() {
        assert!(!s.is_empty());
        assert_eq!(s, "hello");
    }
}

#[test]
fn non_ascii_identifier() {
    let ret = tokenize("hello\u{eee2}");
    assert!(ret.is_err());
    assert!(matches!(
        ret.err().unwrap().err_type,
        TokenizeErrorType::InvalidToken(_)
    ));
}

#[test]
fn keyword() {
    let ret = tokenize("let");
    let ur = ret.unwrap();
    assert!(!ur.is_empty());
    assert!(matches!(ur.first().unwrap().token_type(), TokenType::Let));
}

#[test]
fn keyword_and_identifier() {
    let ret = tokenize("let hello");
    let ur = ret.unwrap();
    assert!(!ur.is_empty());
    assert!(matches!(ur.first().unwrap().token_type(), TokenType::Let));
    assert!(ur.get(1).is_some());
    if let TokenType::Identifier(s) = ur.get(1).unwrap().token_type() {
        assert!(!s.is_empty());
        assert_eq!(s, "hello");
    }
}

#[test]
fn number() {
    let ret = tokenize("69420");
    let ur = ret.unwrap();
    assert!(!ur.is_empty());
    let Some(Token {
        token_type: TokenType::Integer(int_str),
        ..
    }) = ur.first()
    else {
        panic!("tokenize::test::number: ur wrong");
    };
    assert_eq!(int_str, "69420");
}

#[test]
fn one_symbol() {
    let ret = tokenize("+").unwrap();
    assert!(!ret.is_empty());
    assert!(matches!(ret.first().unwrap().token_type, TokenType::Plus));
}
