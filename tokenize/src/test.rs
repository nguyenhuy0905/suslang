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
fn simple_char() {
    let ret = tokenize("' '").unwrap();
    assert_eq!(ret.len(), 1);
    assert_eq!(ret.front().unwrap().token_type, TokenType::Char(' '));
}

#[test]
fn empty_char() {
    let ret = tokenize("''");
    assert!(ret.is_err());
    assert!(matches!(
        ret,
        Err(TokenizeError {
            err_type: TokenizeErrorType::InvalidToken(_),
            ..
        })
    ));
}

#[test]
fn too_many_chars() {
    let ret = tokenize("'coc'");
    assert!(ret.is_err());
    assert!(matches!(
        ret,
        Err(TokenizeError {
            err_type: TokenizeErrorType::InvalidToken(_),
            ..
        })
    ));
}

#[test]
fn unicode_char() {
    let ret = tokenize("'\u{eb54}'").unwrap();
    assert_eq!(ret.len(), 1);
    assert!(matches!(
        ret.front(),
        Some(Token {
            token_type: TokenType::Char('\u{eb54}'),
            ..
        })
    ));
}

#[test]
fn simple_string() {
    let ret = tokenize("\"hello\"");
    assert!(ret.is_ok());
    let ur = ret.unwrap();
    assert!(!ur.is_empty());
    assert!(matches!(
        ur.front().unwrap().token_type(),
        TokenType::String(_)
    ));
    if let TokenType::String(s) = ur.front().unwrap().token_type() {
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
        ur.front().unwrap().token_type(),
        TokenType::String(_)
    ));
    if let TokenType::String(s) = ur.front().unwrap().token_type() {
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
fn unfinished_string() {
    let ret = tokenize("\"hello");
    assert!(ret.is_err());
    assert!(matches!(
        ret,
        Err(TokenizeError {
            err_type: TokenizeErrorType::UnfinishedToken,
            line: 1,
            pos: 2, // the position of the 'h'
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
        ur.front().unwrap().token_type(),
        TokenType::String(_)
    ));
    if let TokenType::String(s) = ur.front().unwrap().token_type() {
        assert!(s.is_empty());
    }
}

#[test]
fn multiline_string() {
    let ret = tokenize("\"hell\no\"").unwrap();
    assert_eq!(ret.len(), 1);
    assert_eq!(
        ret.front().unwrap().token_type,
        TokenType::String("hello".into())
    );
}

#[test]
fn single_quote_inside_string() {
    let ret = tokenize("\"'hello'\"").unwrap();
    assert_eq!(ret.len(), 1);
    assert_eq!(
        ret.front().unwrap().token_type,
        TokenType::String("'hello'".into())
    );
}

#[test]
fn simple_identifier() {
    let ret = tokenize("hello");
    assert!(ret.is_ok());
    let ur = ret.unwrap();
    assert!(!ur.is_empty());
    assert!(matches!(
        ur.front().unwrap().token_type(),
        TokenType::Identifier(_)
    ));
    if let TokenType::Identifier(s) = ur.front().unwrap().token_type() {
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
    assert!(matches!(ur.front().unwrap().token_type(), TokenType::Let));
}

#[test]
fn keyword_and_identifier() {
    let ret = tokenize("let hello");
    let ur = ret.unwrap();
    assert!(!ur.is_empty());
    assert!(matches!(ur.front().unwrap().token_type(), TokenType::Let));
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
    }) = ur.front()
    else {
        panic!("tokenize::test::number: ur wrong");
    };
    assert_eq!(int_str, "69420");
}

#[test]
fn multiline_number() {
    let ret = tokenize("69420\n66666\n424242").unwrap();
    assert_eq!(ret.len(), 3);
    let cmp_arr = [
        TokenType::Integer("69420".into()),
        TokenType::Integer("66666".into()),
        TokenType::Integer("424242".into()),
    ];
    assert_eq!(
        ret.into_iter().map(|t| t.token_type).collect::<Vec<_>>(),
        cmp_arr
    );
}

#[test]
fn one_symbol() {
    let ret = tokenize("+").unwrap();
    assert!(!ret.is_empty());
    assert!(matches!(ret.front().unwrap().token_type, TokenType::Plus));
}

#[test]
fn symbols_on_multi_line() {
    let ret = tokenize("=\n=").unwrap();
    assert_eq!(ret.len(), 2);
    assert!(matches!(
        ret.get(1).unwrap(),
        Token {
            token_type: TokenType::Equal,
            line_number: 2,
            ..
        }
    ));
}

#[test]
fn comparisons() {
    let ret = tokenize("<= >= !=").unwrap();
    assert_eq!(ret.len(), 3);
    let cmp_arr = [
        TokenType::LPBraceEqual,
        TokenType::RPBraceEqual,
        TokenType::BangEqual,
    ];
    assert_eq!(
        ret.into_iter().map(|t| t.token_type).collect::<Vec<_>>(),
        cmp_arr
    );
}

#[test]
fn multi_word_symbol() {
    let ret = tokenize("===").unwrap();
    assert_eq!(ret.len(), 2);
    let ret = ret.into_iter().map(|tt| tt.token_type).collect::<Vec<_>>();
    let cmp_vec = [TokenType::EqualEqual, TokenType::Equal];
    assert_eq!(ret, cmp_vec);
}

#[test]
fn looks_like_namespace_reso() {
    let ret = tokenize("hello::byebye").unwrap();
    assert_eq!(ret.len(), 3);
    let cmp_arr = [
        TokenType::Identifier("hello".into()),
        TokenType::ColonColon,
        TokenType::Identifier("byebye".into()),
    ];
    assert_eq!(
        ret.into_iter().map(|t| t.token_type).collect::<Vec<_>>(),
        cmp_arr
    );
}

#[test]
fn comment() {
    let ret = tokenize("// hello this is a comment").unwrap();
    assert!(ret.is_empty());
}

#[test]
fn comment_and_line() {
    let ret = tokenize("// hello this is a comment\n\"hello\"").unwrap();
    assert_eq!(ret.len(), 1);
    assert_eq!(
        ret.front().unwrap().token_type,
        TokenType::String("hello".into())
    );
}

#[test]
fn multiple_exprs() {
    let ret = tokenize("\"hello\";\"goodbye\"").unwrap();
    assert_eq!(ret.len(), 3);
    let cmp_arr = [
        TokenType::String("hello".into()),
        TokenType::Semicolon,
        TokenType::String("goodbye".into()),
    ];
    assert_eq!(
        ret.into_iter().map(|tt| tt.token_type).collect::<Vec<_>>(),
        cmp_arr
    );
}

#[test]
fn funny_looking_code() {
    let ret = tokenize("proc hello() {\nlet goodbye_1 = 1;\n}").unwrap();
    let cmp_arr = [
        TokenType::Proc,
        TokenType::Identifier("hello".into()),
        TokenType::LParen,
        TokenType::RParen,
        TokenType::LCParen,
        TokenType::Let,
        TokenType::Identifier("goodbye_1".into()),
        TokenType::Equal,
        TokenType::Integer("1".into()),
        TokenType::Semicolon,
        TokenType::RCParen,
    ];
    assert_eq!(ret.len(), cmp_arr.len());
    assert_eq!(
        ret.into_iter().map(|t| t.token_type).collect::<Vec<_>>(),
        cmp_arr
    );
}
