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
