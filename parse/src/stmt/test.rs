use crate::new_name_resolve;
use crate::new_test_deque;
use crate::ResolveStep;
use crate::TypeInfoKind;
use std::collections::VecDeque;
use tokenize::Token;
use tokenize::TokenType;

use super::NameResolve;
use super::TypeParse;

#[test]
fn name_resolve() {
    // simplest possible
    {
        let mut deque =
            new_test_deque![TokenType::Identifier("sus".to_string())];
        let name_resolve = NameResolve::parse(&mut deque).unwrap();
        assert_eq!(
            name_resolve,
            TypeInfoKind::Reference(new_name_resolve![ResolveStep::Child(
                "sus".to_string()
            )])
        )
    }
    // with global
    {
        let mut deque = new_test_deque![
            TokenType::ColonColon,
            TokenType::Identifier("sus".to_string())
        ];
        let name_resolve = NameResolve::parse(&mut deque).unwrap();
        assert_eq!(
            name_resolve,
            TypeInfoKind::Reference(new_name_resolve![
                ResolveStep::Global,
                ResolveStep::Child("sus".to_string()),
            ])
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
        let name_resolve = NameResolve::parse(&mut deque).unwrap();
        assert_eq!(
            name_resolve,
            TypeInfoKind::Reference(new_name_resolve![
                ResolveStep::Global,
                ResolveStep::Child("sus".to_string()),
                ResolveStep::Parent,
                ResolveStep::Child("sy".to_string()),
            ])
        )
    }
}
