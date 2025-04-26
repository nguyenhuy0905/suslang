use super::*;

#[test]
fn ignore_space() {
    let ret = tokenize("     \r\t");
    assert!(ret.is_ok());
    assert!(ret.unwrap().is_empty());
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
