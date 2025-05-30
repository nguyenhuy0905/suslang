use super::ProcParams;
use super::{NameResolve, Scope, StmtParse, TypeParse, VarDeclStmt};
use crate::StmtAstBoxWrap;
use crate::{
    new_name_resolve, new_test_deque, new_var_decl_expr, ParseError,
    PrimaryExpr, ResolveStep, TypeInfoKind,
};
use crate::{new_proc_params, ExprBoxWrap};
use std::collections::{HashMap, VecDeque};
use tokenize::{Token, TokenType};

macro_rules! assert_stmt_ast_eq {
    ($actual:expr,$expect:expr) => {
        assert_eq!($actual, StmtAstBoxWrap::new($expect))
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
        let name_resolve =
            NameResolve::parse(&mut deque, 1, 1).map(|t| t.0).unwrap();
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
        let name_resolve =
            NameResolve::parse(&mut deque, 1, 1).map(|t| t.0).unwrap();
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
    // Empty global error
    {
        let mut deque = new_test_deque![TokenType::ColonColon,];
        let ret = NameResolve::parse_no_vtable(&mut deque, 1, 1);
        assert_eq!(ret, Err(ParseError::ExpectedToken { line: 1, pos: 1 }));
    }
    // Overlord-of-global
    {
        let mut deque =
            new_test_deque![TokenType::ColonColon, TokenType::Overlord,];
        let ret = NameResolve::parse_no_vtable(&mut deque, 1, 1);
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
        let ret = NameResolve::parse_no_vtable(&mut deque, 1, 1);
        assert_eq!(ret, Err(ParseError::ExpectedToken { line: 1, pos: 2 }));
    }
}

#[test]
fn var_decl_stmt() {
    // simplest
    {
        let mut deque = new_test_deque![
            TokenType::Let,
            TokenType::Identifier("num".to_string()),
            TokenType::Equal,
            TokenType::Integer(1.to_string())
        ];
        let mut scope = Scope {
            symbols: HashMap::new(),
            name: "hello".to_string(),
            parent_idx: None,
        };
        let (vardecl, .., pos) =
            VarDeclStmt::parse(&mut deque, &mut scope, 1, 1).unwrap();
        assert_stmt_ast_eq!(
            vardecl,
            new_var_decl_expr!("num", None, PrimaryExpr::Integer(1))
        );
        assert!(scope.symbols.contains_key("num"));
        // the last token's position
        assert_eq!(pos, 4);
    }
    // with type anno
    {
        let mut deque = new_test_deque![
            TokenType::Let,
            TokenType::Identifier("num".to_string()),
            TokenType::Colon,
            TokenType::Identifier("Urmom".to_string()),
            TokenType::Equal,
            TokenType::String("fat".to_string())
        ];
        let mut scope = Scope {
            symbols: HashMap::new(),
            name: "hello".to_string(),
            parent_idx: None,
        };
        let (vardecl, ..) =
            VarDeclStmt::parse(&mut deque, &mut scope, 1, 1).unwrap();
        assert_stmt_ast_eq!(
            vardecl,
            new_var_decl_expr!(
                "num",
                vec![ResolveStep::Child("Urmom".to_string())],
                PrimaryExpr::String("fat".to_string())
            )
        );
        assert!(scope.symbols.contains_key("num"));
    }
    // only LHS
    {
        let mut deque = new_test_deque![
            TokenType::Let,
            TokenType::Identifier("urmom".to_string())
        ];
        let mut scope = Scope {
            symbols: HashMap::new(),
            name: "hello".to_string(),
            parent_idx: None,
        };
        let vardecl = VarDeclStmt::parse(&mut deque, &mut scope, 1, 1);
        assert_eq!(vardecl, Err(ParseError::ExpectedToken { line: 1, pos: 2 }));
        assert!(!scope.symbols.contains_key("urmom"));
    }
    // lacking a type annotation after the colon
    {
        let mut deque = new_test_deque![
            TokenType::Let,
            TokenType::Identifier("urmom".to_string()),
            TokenType::Colon,
            TokenType::Equal,
            TokenType::Integer(1.to_string()),
        ];
        let mut scope = Scope {
            symbols: HashMap::new(),
            name: "hello".to_string(),
            parent_idx: None,
        };
        let vardecl = VarDeclStmt::parse(&mut deque, &mut scope, 1, 1);
        assert_eq!(
            vardecl,
            Err(ParseError::UnexpectedToken(Token::new(
                TokenType::Equal,
                1,
                4
            )))
        );
        assert!(!scope.symbols.contains_key("urmom"));
    }
    // using a keyword in place of an identifier
    {
        let mut deque = new_test_deque![
            TokenType::Let,
            TokenType::Overlord,
            TokenType::Equal,
            TokenType::Integer(69.to_string()),
        ];
        let mut scope = Scope {
            symbols: HashMap::new(),
            name: "hello".to_string(),
            parent_idx: None,
        };
        let vardecl = VarDeclStmt::parse(&mut deque, &mut scope, 1, 1);
        assert_eq!(
            vardecl,
            Err(ParseError::UnexpectedToken(Token::new(
                TokenType::Overlord,
                1,
                2
            )))
        );
        assert!(!scope.symbols.contains_key("overlord"));
    }
    // symbol already defined
    {
        // no semicolon.
        let mut deque = new_test_deque![
            TokenType::Let,
            TokenType::Identifier("urmom".to_string()),
            TokenType::Equal,
            TokenType::String("hehe".to_string()),
            TokenType::Let,
            TokenType::Identifier("urmom".to_string()),
            TokenType::Equal,
            TokenType::String("hehe".to_string()),
        ];
        let mut scope = Scope {
            symbols: HashMap::new(),
            name: "hello".to_string(),
            parent_idx: None,
        };
        let (_, ln, pos) =
            VarDeclStmt::parse(&mut deque, &mut scope, 1, 1).unwrap();
        let errvar = VarDeclStmt::parse(&mut deque, &mut scope, ln, pos);
        assert_eq!(
            errvar,
            Err(ParseError::SymbolAlreadyExists {
                name: "urmom".to_string(),
                line: 1,
                pos: 2,
            })
        );
        assert!(scope.symbols.contains_key("urmom"));
    }
}

#[test]
fn proc_params() {
    // empty params list
    {
        let mut deque = VecDeque::<Token>::new();
        let (params, ..) = ProcParams::parse(&mut deque, 1, 1).unwrap();
        assert_eq!(
            params,
            ProcParams {
                params: HashMap::new(),
            }
        )
    }
    // single param
    {
        let mut deque = new_test_deque![
            TokenType::Identifier("urmom".to_string()),
            TokenType::Colon,
            TokenType::Identifier("Fat".to_string())
        ];
        let (params, .., pos) = ProcParams::parse(&mut deque, 1, 1).unwrap();
        assert_eq!(
            params,
            new_proc_params![(
                "urmom",
                new_name_resolve!(ResolveStep::Child("Fat".to_string()))
            )]
        );
        assert_eq!(pos, 3);
    }
    // comma-separated
    {
        let mut deque = new_test_deque![
            TokenType::Identifier("urmom".to_string()),
            TokenType::Colon,
            TokenType::Identifier("Fat".to_string()),
            TokenType::Comma,
            TokenType::Identifier("urdad".to_string()),
            TokenType::Colon,
            TokenType::Identifier("Unemployed".to_string()),
        ];
        let (params, .., pos) = ProcParams::parse(&mut deque, 1, 1).unwrap();
        assert_eq!(
            params,
            new_proc_params![
                (
                    "urmom",
                    new_name_resolve!(ResolveStep::Child("Fat".to_string()))
                ),
                (
                    "urdad",
                    new_name_resolve!(ResolveStep::Child(
                        "Unemployed".to_string()
                    ))
                )
            ]
        );
        assert_eq!(pos, 7);
    }
    // TODO: test error cases
}
