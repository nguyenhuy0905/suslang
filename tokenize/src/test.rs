use crate::{
    Token, TokenKind, TokenizeError, TokenizeErrorType, Tokenizer,
    tokens::CharPosition,
};

#[test]
fn ignore_space() {
    let ret = Tokenizer::tokenize("   \t").unwrap();
    assert!(ret.is_empty());
}

#[test]
fn non_ascii_begin_token() {
    let ret = Tokenizer::tokenize("\u{f4a2} russ \u{f4a2}");
    assert_eq!(
        ret,
        Err(TokenizeError {
            err_type: TokenizeErrorType::InvalidChar('\u{f4a2}'),
            pos: CharPosition { line: 1, column: 1 }
        })
    );
}

#[test]
fn simple_char() {
    let ret = Tokenizer::tokenize("\' \'").unwrap();
    assert_eq!(
        ret,
        [Token {
            kind: TokenKind::Char,
            pos: CharPosition { line: 1, column: 2 },
            repr: Some(Box::from(' '.to_string())),
        }]
    );
}

#[test]
fn empty_char() {
    let ret = Tokenizer::tokenize("\'\'");
    assert_eq!(
        ret,
        Err(TokenizeError {
            err_type: TokenizeErrorType::InvalidChar('\''),
            pos: CharPosition { line: 1, column: 2 }
        })
    );
}

#[test]
fn too_many_chars() {
    let ret = Tokenizer::tokenize("'coc'");
    assert_eq!(
        ret,
        Err(TokenizeError {
            err_type: TokenizeErrorType::InvalidChar('o'),
            pos: CharPosition { line: 1, column: 3 }
        })
    );
}

#[test]
fn unicode_char() {
    let ret = Tokenizer::tokenize("'\u{eb54}'");
    assert_eq!(ret.clone().map(|ret| ret.len()), Ok(1));
    assert_eq!(
        ret,
        Ok(vec![Token {
            kind: TokenKind::Char,
            pos: CharPosition { line: 1, column: 2 },
            repr: Some(Box::from('\u{eb54}'.to_string())),
        }])
    );
}

#[test]
fn simple_string() {
    let ret = Tokenizer::tokenize("\"hello\"");
    assert_eq!(
        ret,
        Ok(vec![Token {
            kind: TokenKind::String,
            pos: CharPosition { line: 1, column: 2 },
            repr: Some(Box::from("hello")),
        }])
    );
}

#[test]
fn unicode_string() {
    let ret = Tokenizer::tokenize("\"hello\u{f4a2}!\"");
    assert_eq!(
        ret.as_ref().map(|ret| ret.first()),
        Ok(Some(&Token {
            kind: TokenKind::String,
            pos: CharPosition { line: 1, column: 2 },
            repr: Some(Box::from("hello\u{f4a2}!")),
        }))
    );
}

#[test]
fn unfinished_string() {
    let ret = Tokenizer::tokenize("\"hello");
    assert_eq!(
        ret,
        Err(TokenizeError {
            err_type: TokenizeErrorType::ExpectChar,
            pos: CharPosition { line: 1, column: 2 }
        })
    );
}

#[test]
fn empty_string() {
    let ret = Tokenizer::tokenize("\"\"");
    assert_eq!(
        ret,
        Ok(vec![Token {
            kind: TokenKind::String,
            pos: CharPosition { line: 1, column: 2 },
            repr: Some(Box::from(""))
        }])
    );
}

#[test]
fn single_quote_inside_string() {
    let ret = Tokenizer::tokenize("\"'hello'\"");
    assert_eq!(
        ret,
        Ok(vec![Token {
            kind: TokenKind::String,
            pos: CharPosition { line: 1, column: 2 },
            repr: Some(Box::from("'hello'"))
        }])
    );
}

#[test]
fn simple_identifier() {
    let ret = Tokenizer::tokenize("hello");
    assert_eq!(
        ret,
        Ok(vec![Token {
            kind: TokenKind::Identifier,
            pos: CharPosition { line: 1, column: 1 },
            repr: Some(Box::from("hello")),
        }])
    );
}

#[test]
fn non_ascii_identifier() {
    let ret = Tokenizer::tokenize("hello\u{eee2}");
    assert_eq!(
        ret,
        Err(TokenizeError {
            err_type: TokenizeErrorType::InvalidChar('\u{eee2}'),
            pos: CharPosition { line: 1, column: 6 }
        })
    );
}

#[test]
fn keyword() {
    let ret = Tokenizer::tokenize("let");
    assert_eq!(
        ret,
        Ok(vec![Token {
            kind: TokenKind::Let,
            pos: CharPosition { line: 1, column: 1 },
            repr: None,
        }])
    );
}

#[test]
fn keyword_and_identifier() {
    let ret = Tokenizer::tokenize("let hello");
    assert_eq!(
        ret,
        Ok(vec![
            Token {
                kind: TokenKind::Let,
                pos: CharPosition { line: 1, column: 1 },
                repr: None,
            },
            Token {
                kind: TokenKind::Identifier,
                pos: CharPosition { line: 1, column: 5 },
                repr: Some(Box::from("hello")),
            }
        ])
    );
}

#[test]
fn number() {
    let ret = Tokenizer::tokenize("69420");
    assert_eq!(
        ret,
        Ok(vec![Token {
            kind: TokenKind::Integer,
            pos: CharPosition { line: 1, column: 1 },
            repr: Some(Box::from(69420.to_string())),
        }])
    );
}

#[test]
fn simple_double() {
    let ret = Tokenizer::tokenize("420.69");
    assert_eq!(
        ret,
        Ok(vec![Token {
            kind: TokenKind::Float,
            pos: CharPosition { line: 1, column: 1 },
            repr: Some(Box::from(420.69.to_string())),
        }])
    );
}

#[test]
fn multiple_dots_double() {
    let ret = Tokenizer::tokenize("420.69.111.222").unwrap();
    assert_eq!(
        ret,
        [
            Token {
                kind: TokenKind::Float,
                pos: CharPosition { line: 1, column: 1 },
                repr: Some(Box::from("420.69")),
            },
            Token {
                kind: TokenKind::Dot,
                pos: CharPosition { line: 1, column: 7 },
                repr: None,
            },
            Token {
                kind: TokenKind::Float,
                pos: CharPosition { line: 1, column: 8 },
                repr: Some(Box::from("111.222")),
            }
        ]
    );
}

#[test]
fn double_multiline() {
    // it's not a double actually
    let ret = Tokenizer::tokenize("420.\n69").unwrap();
    assert_eq!(
        ret,
        [
            Token {
                kind: TokenKind::Integer,
                pos: CharPosition { line: 1, column: 1 },
                repr: Some(Box::from("420")),
            },
            Token {
                kind: TokenKind::Dot,
                pos: CharPosition { line: 1, column: 4 },
                repr: None,
            },
            Token {
                kind: TokenKind::Integer,
                pos: CharPosition { line: 2, column: 1 },
                repr: Some(Box::from("69")),
            }
        ]
    );
}

#[test]
fn number_and_member() {
    let ret = Tokenizer::tokenize("69.print()").unwrap();
    assert_eq!(
        ret,
        [
            Token {
                kind: TokenKind::Integer,
                pos: CharPosition { line: 1, column: 1 },
                repr: Some(Box::from("69"))
            },
            Token {
                kind: TokenKind::Dot,
                pos: CharPosition { line: 1, column: 3 },
                repr: None,
            },
            Token {
                kind: TokenKind::Identifier,
                pos: CharPosition { line: 1, column: 4 },
                repr: Some(Box::from("print")),
            },
            Token {
                kind: TokenKind::LParen,
                pos: CharPosition { line: 1, column: 9 },
                repr: None,
            },
            Token {
                kind: TokenKind::RParen,
                pos: CharPosition {
                    line: 1,
                    column: 10
                },
                repr: None,
            }
        ]
    );
}

#[test]
fn multiline_number() {
    let ret = Tokenizer::tokenize("69420\n66666\n424242");
    assert_eq!(
        ret,
        Ok(vec![
            Token {
                kind: TokenKind::Integer,
                pos: CharPosition { line: 1, column: 1 },
                repr: Some(Box::from(69420.to_string())),
            },
            Token {
                kind: TokenKind::Integer,
                pos: CharPosition { line: 2, column: 1 },
                repr: Some(Box::from(66666.to_string())),
            },
            Token {
                kind: TokenKind::Integer,
                pos: CharPosition { line: 3, column: 1 },
                repr: Some(Box::from(424242.to_string())),
            },
        ])
    );
}

#[test]
fn one_symbol() {
    // TODO: i haven't implemented parsing symbols yet.
    let ret = Tokenizer::tokenize("+");
    assert_eq!(
        ret,
        Ok(vec![Token {
            kind: TokenKind::Plus,
            pos: CharPosition { line: 1, column: 1 },
            repr: None,
        }])
    );
}

#[test]
fn symbols_on_multi_line() {
    let ret = Tokenizer::tokenize("=\n=").unwrap();
    assert_eq!(
        ret,
        [
            Token {
                kind: TokenKind::Equal,
                pos: CharPosition { line: 1, column: 1 },
                repr: None,
            },
            Token {
                kind: TokenKind::Equal,
                pos: CharPosition { line: 2, column: 1 },
                repr: None,
            }
        ]
    );
}

#[test]
fn comparisons() {
    let ret = Tokenizer::tokenize("<= >=").unwrap();
    assert_eq!(
        ret,
        [
            Token {
                kind: TokenKind::LessEqual,
                pos: CharPosition { line: 1, column: 1 },
                repr: None,
            },
            Token {
                kind: TokenKind::GreaterEqual,
                pos: CharPosition { line: 1, column: 4 },
                repr: None,
            },
        ]
    );
}

#[test]
fn conditional() {
    let ret = Tokenizer::tokenize("or and eq neq not").unwrap();
    assert_eq!(
        ret,
        [
            Token {
                kind: TokenKind::Or,
                pos: CharPosition { line: 1, column: 1 },
                repr: None,
            },
            Token {
                kind: TokenKind::And,
                pos: CharPosition { line: 1, column: 4 },
                repr: None,
            },
            Token {
                kind: TokenKind::Eq,
                pos: CharPosition { line: 1, column: 8 },
                repr: None,
            },
            Token {
                kind: TokenKind::Neq,
                pos: CharPosition {
                    line: 1,
                    column: 11
                },
                repr: None,
            },
            Token {
                kind: TokenKind::Not,
                pos: CharPosition {
                    line: 1,
                    column: 15
                },
                repr: None,
            },
        ]
    );
}

#[test]
fn bitwise_line_separated() {
    let ret = Tokenizer::tokenize("|\n|").unwrap();
    assert_eq!(
        ret,
        [
            Token {
                kind: TokenKind::Beam,
                pos: CharPosition { line: 1, column: 1 },
                repr: None,
            },
            Token {
                kind: TokenKind::Beam,
                pos: CharPosition { line: 2, column: 1 },
                repr: None,
            }
        ]
    );
}

#[test]
fn bitwise_whitespace_separated() {
    let ret = Tokenizer::tokenize("| |").unwrap();
    assert_eq!(
        ret,
        [
            Token {
                kind: TokenKind::Beam,
                pos: CharPosition { line: 1, column: 1 },
                repr: None,
            },
            Token {
                kind: TokenKind::Beam,
                pos: CharPosition { line: 1, column: 3 },
                repr: None,
            }
        ]
    );
}

#[test]
fn comment() {
    let ret = Tokenizer::tokenize("# hello this is a comment").unwrap();
    assert!(ret.is_empty());
}

#[test]
fn comment_and_line() {
    let ret =
        Tokenizer::tokenize("# hello this is a comment\n\"hello\"").unwrap();
    assert_eq!(
        ret,
        [Token {
            kind: TokenKind::String,
            pos: CharPosition { line: 2, column: 2 },
            repr: Some(Box::from(Box::from("hello"))),
        }]
    );
}

#[test]
fn multiple_exprs() {
    let ret = Tokenizer::tokenize("\"hello\";\"goodbye\"").unwrap();
    assert_eq!(
        ret,
        [
            Token {
                kind: TokenKind::String,
                pos: CharPosition { line: 1, column: 2 },
                repr: Some(Box::from("hello")),
            },
            Token {
                kind: TokenKind::Semicolon,
                pos: CharPosition { line: 1, column: 8 },
                repr: None,
            },
            Token {
                kind: TokenKind::String,
                pos: CharPosition {
                    line: 1,
                    column: 10
                },
                repr: Some(Box::from("goodbye")),
            },
        ]
    );
}
