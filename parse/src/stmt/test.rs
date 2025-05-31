use super::NameResolve;
use crate::{new_name_resolve, new_test_deque, ParseError, ResolveStep};
use std::collections::VecDeque;
use tokenize::{Token, TokenType};

#[allow(unused)]
macro_rules! assert_stmt_ast_eq {
    ($actual:expr,$expect:expr) => {
        assert_eq!($actual, DeclStmtBoxWrap::new($expect))
    };
}

#[test]
fn name_resolve() {
    // simplest possible
    {
        let mut deque =
            new_test_deque![TokenType::Identifier("sus".to_string())];
        let name_resolve =
            NameResolve::parse(&mut deque, 1, 1).map(|t| t.0).unwrap();
        assert_eq!(
            name_resolve,
            new_name_resolve![ResolveStep::Child("sus".to_string())]
        )
    }
    // with global
    {
        let mut deque = new_test_deque![
            TokenType::ColonColon,
            TokenType::Identifier("sus".to_string())
        ];
        let name_resolve =
            NameResolve::parse(&mut deque, 1, 1).map(|t| t.0).unwrap();
        assert_eq!(
            name_resolve,
            new_name_resolve![
                ResolveStep::Global,
                ResolveStep::Child("sus".to_string()),
            ]
        )
    }
    // with parent reference
    {
        let mut deque = new_test_deque![
            TokenType::ColonColon,
            TokenType::Identifier("sus".to_string()),
            TokenType::ColonColon,
            TokenType::Overlord,
            TokenType::ColonColon,
            TokenType::Identifier("sy".to_string()),
        ];
        let name_resolve =
            NameResolve::parse(&mut deque, 1, 1).map(|t| t.0).unwrap();
        assert_eq!(
            name_resolve,
            new_name_resolve![
                ResolveStep::Global,
                ResolveStep::Child("sus".to_string()),
                ResolveStep::Parent,
                ResolveStep::Child("sy".to_string()),
            ]
        )
    }
    // Empty global error
    {
        let mut deque = new_test_deque![TokenType::ColonColon,];
        let ret = NameResolve::parse(&mut deque, 1, 1);
        assert_eq!(ret, Err(ParseError::ExpectedToken { line: 1, pos: 1 }));
    }
    // Overlord-of-global
    {
        let mut deque =
            new_test_deque![TokenType::ColonColon, TokenType::Overlord,];
        let ret = NameResolve::parse(&mut deque, 1, 1);
        assert_eq!(
            ret,
            Err(ParseError::UnexpectedToken(Token::new(
                TokenType::Overlord,
                1,
                2
            )))
        );
    }
    // Unmatched ColonColon
    {
        let mut deque =
            new_test_deque![TokenType::Overlord, TokenType::ColonColon];
        let ret = NameResolve::parse(&mut deque, 1, 1);
        assert_eq!(ret, Err(ParseError::ExpectedToken { line: 1, pos: 2 }));
    }
}
