use super::*;

#[test]
fn test_tokenizer_ignore_space() {
    let ret = tokenize("     \r\t");
    assert!(ret.is_ok());
    assert!(ret.unwrap().is_empty());
}

#[test]
fn test_tokenizer_simple_string() {
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
