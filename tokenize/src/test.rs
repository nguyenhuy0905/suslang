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
            kind: TokenKind::Char(' '),
            pos: CharPosition { line: 1, column: 2 }
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
            kind: TokenKind::Char('\u{eb54}'),
            pos: CharPosition { line: 1, column: 2 }
        }])
    );
}

#[test]
fn simple_string() {
    let ret = Tokenizer::tokenize("\"hello\"");
    assert_eq!(
        ret,
        Ok(vec![Token {
            kind: TokenKind::String(Box::from("hello")),
            pos: CharPosition { line: 1, column: 2 }
        }])
    );
}

#[test]
fn unicode_string() {
    let ret = Tokenizer::tokenize("\"hello\u{f4a2}!\"");
    assert_eq!(
        ret.as_ref().map(|ret| ret.first()),
        Ok(Some(&Token {
            kind: TokenKind::String(Box::from("hello\u{f4a2}!")),
            pos: CharPosition { line: 1, column: 2 }
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
            kind: TokenKind::String(Box::from("")),
            pos: CharPosition { line: 1, column: 2 }
        }])
    );
}

#[test]
fn single_quote_inside_string() {
    let ret = Tokenizer::tokenize("\"'hello'\"");
    assert_eq!(
        ret,
        Ok(vec![Token {
            kind: TokenKind::String(Box::from("'hello'")),
            pos: CharPosition { line: 1, column: 2 }
        }])
    );
}

#[test]
fn simple_identifier() {
    let ret = Tokenizer::tokenize("hello");
    assert_eq!(
        ret,
        Ok(vec![Token {
            kind: TokenKind::Identifier(Box::from("hello")),
            pos: CharPosition { line: 1, column: 1 }
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
            pos: CharPosition { line: 1, column: 1 }
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
                pos: CharPosition { line: 1, column: 1 }
            },
            Token {
                kind: TokenKind::Identifier(Box::from("hello")),
                pos: CharPosition { line: 1, column: 5 }
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
            kind: TokenKind::Integer(69420),
            pos: CharPosition { line: 1, column: 1 }
        }])
    );
}

#[test]
fn simple_double() {
    let ret = Tokenizer::tokenize("420.69");
    assert_eq!(
        ret,
        Ok(vec![Token {
            kind: TokenKind::Float(420.69),
            pos: CharPosition { line: 1, column: 1 }
        }])
    );
}

#[test]
fn too_many_dot_double() {
    let ret = Tokenizer::tokenize("420.69.111");
    assert_eq!(
        ret,
        Err(TokenizeError {
            err_type: TokenizeErrorType::InvalidChar('.'),
            pos: CharPosition { line: 1, column: 7 }
        })
    );
}

#[test]
fn double_multiline_err() {
    let ret = Tokenizer::tokenize("420.\n69");
    assert_eq!(
        ret,
        Err(TokenizeError {
            err_type: TokenizeErrorType::InvalidChar('\n'),
            pos: CharPosition { line: 1, column: 5 }
        })
    );
}

#[test]
fn multiline_number() {
    let ret = Tokenizer::tokenize("69420\n66666\n424242");
    assert_eq!(
        ret,
        Ok(vec![
            Token {
                kind: TokenKind::Integer(69420),
                pos: CharPosition { line: 1, column: 1 }
            },
            Token {
                kind: TokenKind::Integer(66666),
                pos: CharPosition { line: 2, column: 1 }
            },
            Token {
                kind: TokenKind::Integer(424242),
                pos: CharPosition { line: 3, column: 1 }
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
            pos: CharPosition { line: 1, column: 1 }
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
                pos: CharPosition { line: 1, column: 1 }
            },
            Token {
                kind: TokenKind::Equal,
                pos: CharPosition { line: 2, column: 1 }
            }
        ]
    );
}

#[test]
fn comparisons() {
    let ret = Tokenizer::tokenize("<= >= !=").unwrap();
    assert_eq!(
        ret,
        [
            Token {
                kind: TokenKind::LessEqual,
                pos: CharPosition { line: 1, column: 1 }
            },
            Token {
                kind: TokenKind::GreaterEqual,
                pos: CharPosition { line: 1, column: 4 }
            },
            Token {
                kind: TokenKind::BangEqual,
                pos: CharPosition { line: 1, column: 7 }
            }
        ]
    );
}

#[test]
fn conditional() {
    let ret = Tokenizer::tokenize("|| &&").unwrap();
    assert_eq!(
        ret,
        [
            Token {
                kind: TokenKind::BeamBeam,
                pos: CharPosition { line: 1, column: 1 }
            },
            Token {
                kind: TokenKind::AmpersandAmpersand,
                pos: CharPosition { line: 1, column: 4 }
            }
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
                pos: CharPosition { line: 1, column: 1 }
            },
            Token {
                kind: TokenKind::Beam,
                pos: CharPosition { line: 2, column: 1 }
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
                pos: CharPosition { line: 1, column: 1 }
            },
            Token {
                kind: TokenKind::Beam,
                pos: CharPosition { line: 1, column: 3 }
            }
        ]
    );
}

#[test]
fn multi_word_symbol() {
    let ret = Tokenizer::tokenize("===").unwrap();
    assert_eq!(
        ret,
        [
            Token {
                kind: TokenKind::EqualEqual,
                pos: CharPosition { line: 1, column: 1 }
            },
            Token {
                kind: TokenKind::Equal,
                pos: CharPosition { line: 1, column: 3 }
            }
        ]
    );
}

#[test]
fn comment() {
    let ret = Tokenizer::tokenize("// hello this is a comment").unwrap();
    assert!(ret.is_empty());
}

#[test]
fn comment_and_line() {
    let ret =
        Tokenizer::tokenize("// hello this is a comment\n\"hello\"").unwrap();
    assert_eq!(
        ret,
        [Token {
            kind: TokenKind::String(Box::from("hello")),
            pos: CharPosition { line: 2, column: 2 }
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
                kind: TokenKind::String("hello".into()),
                pos: CharPosition { line: 1, column: 2 }
            },
            Token {
                kind: TokenKind::Semicolon,
                pos: CharPosition { line: 1, column: 8 }
            },
            Token {
                kind: TokenKind::String("goodbye".into()),
                pos: CharPosition {
                    line: 1,
                    column: 10
                }
            },
        ]
    );
}
