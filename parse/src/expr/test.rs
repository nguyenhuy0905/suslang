use super::*;
use arith::*;
use std::ops::Deref;
use tokenize::TokenType;
mod arith_test;
mod block_test;
mod cond_test;

/// The first side must be an `ExprBoxWrap`, and the second must implement
/// `AstCmp`, that is, implementing `Ast` and also have trait `PartialEq`.
#[macro_export]
macro_rules! assert_ast_eq {
    ($first:expr,$second:expr) => {
        assert_eq!(
            $first.deref() as &dyn ExprAstImpl,
            &$second as &dyn ExprAstImpl
        )
    };
}

/// Creates a new test `VecDeque`.
/// Each element passed in must be of type `TokenType`
///
/// # Example
///
/// ```
/// let mut new_deq = new_test_deque![
///   TokenType::Integer("3".to_string()),
///   TokenType::Star,
/// ];
/// ```
#[macro_export]
macro_rules! new_test_deque {
    ($toktyp1:expr$(,$toktyp:expr)*$(,)?) => {
        {
            let mut counter = 0;
            Into::<VecDeque<_>>::into([$toktyp1 $(,$toktyp)*]
            .map(|typ| Token::new(typ, 1, {counter += 1; counter})))
        }
    }
}

#[test]
fn expr_test() {
    // fallthrough.
    // I mean, the only thing Expr really does is falling through.
    {
        let mut deque = new_test_deque![
            TokenType::Ya,
            TokenType::AmpersandAmpersand,
            TokenType::Na,
        ];
        let (expr, ln, pos) = Expr::parse(&mut deque, 1, 1).unwrap();
        assert_ast_eq!(
            expr,
            new_logic_and_expr![
                PrimaryExpr::Boolean(true),
                PrimaryExpr::Boolean(false),
            ]
        );
        assert_eq!(ln, 1);
        assert_eq!(pos, 3);
    }
}
