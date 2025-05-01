// TODO: remove allow dead_code on top of file
#![allow(unused)]

use std::collections::VecDeque;
use std::fmt::Debug;
use tokenize::{Token, TokenType};
#[cfg(test)]
mod test;

#[derive(Debug)]
#[repr(u8)]
pub enum ParseErrorType {
    UnexpectedToken(tokenize::TokenType),
    ExpectedExpr,
}

#[derive(Debug)]
pub struct ParseError {
    typ: ParseErrorType,
    line: usize,
    pos: usize,
}

/// Peak programming
///
/// For each defined node, do the following:
/// - Define the struct using ``$name`` and ``$blk``, with the defined
///   attributes.
/// - Define a variant in the enum [`NodeType`] that has the name of the
///   struct.
/// - Implement trait [`AstNode`] where [`AstNode::node_type`] simply returns
///   ``NodeType::$name``
/// - Implement trait [`AstNodeTypeCmp`] whose function just returns ``true``
///   for the ``Self`` overload.
///
/// # Note
/// - All structs are ``pub`` here.
/// - The usefulness of the [`AstNode`] and the comparison derive is,
///   questionable. If they are not actually necessary, this macro will be
///   removed.
macro_rules! decl_nodes {
    ($($(#[$attr:meta])* $name:ident $blk:tt)+) => (
        // swapped around so that highlighting works as intended.
        $($(#[$attr])*
            #[derive(Debug, Clone, PartialEq, Eq)]
            pub struct $name $blk)*

        /// All the node types, flagged. Only useful as a way to check the type
        /// of node without relying on downcasting.
        #[derive(Clone, Copy, Debug, PartialEq, Eq)]
        #[repr(u8)]
        pub enum NodeType {
            $($name),*
        }
        $(impl AstNodeType for $name {
            fn node_type(&self) -> NodeType {
                NodeType::$name
            }
        })*
        $(impl AstNodeTypeCmp<Self> for $name {
            fn is(&self) -> bool {
                true
            }
        })*
    )
}

/// A simple trait to get the node type.
///
/// Auto-implemented by [`decl_nodes!`] macro.
trait AstNodeType {
    fn node_type(&self) -> NodeType;
}

/// A less simple trait to check if a node if of a specified type.
///
/// Auto-implemented by [`decl_nodes!`] macro.
trait AstNodeTypeCmp<T: AstNodeType> {
    #[must_use]
    fn is(&self) -> bool {
        false
    }
}

/// Node.rs. No vtable allowed.
trait AstNode: AstNodeType + std::marker::Sized {
    /// Parses the list of [`Token`]s. If succeed, returns an instance of
    /// `Self`, otherwise:
    /// - If parse fails due to end-of-file, the error is `None`.
    /// - Otherwise, it's an actual error.
    ///
    /// * `tokens`: A [`VecDeque`] of tokens. Obtained from calling
    ///   [`tokenize::tokenize`].
    fn parse(tokens: &mut VecDeque<Token>) -> Result<Self, Option<ParseError>>;
}

decl_nodes!(

/// A program consists of one or more statements, for now.
///
/// Of course, only certain types of statements may (or must) not be inside a
/// block; say, a function declaration.
///
/// # Rule
///
/// \<program\> ::= \<stmt\>+
///
/// # See also
///
/// [`Stmt`]
Program {
    stmts: Vec<Stmt>,
}

/// A statement evaluates to the void type, if it ends with a semicolon.
///
/// A statement without semicolon is, for now, an expression. Hah.
Stmt {}

/// An expression evaluates to a specific type.
///
/// Sometimes, that may include the void type.
Expr {
    term: TermExpr,
}

// TODO: rules for LHS of either arithmetic or boolean expression,
// then a smaller rule for RHS of arithmetic and RHS of boolean expressions.

/// Add or minus.
///
/// # Rule
/// \<term-expr\> ::= \<factor-expr\> (\<term-op\> <\factor-expr\>)*
/// \<term-op\> ::= "+" | "-"
///
/// # See also
/// - [`FactorExpr`]
TermExpr {
    first_term: FactorExpr,
    follow_terms: Vec<(TermOp, FactorExpr)>
}

/// Multiply or divide
///
/// # Rule
/// \<factor-expr\> ::= \<arith-unary-expr\>
///                     (\<factor-op\> <\arith-unary-expr\>)*
///
/// \<factor-op\> ::= "*" | "/"
///
/// # See also
/// - [`ArithUnaryExpr`]
FactorExpr {
    first_factor: ArithUnaryExpr,
    follow_factors: Vec<(FacOp, ArithUnaryExpr)>,
}

// TODO: finish writing the docs

// TODO: type-check the expression.
//
// Simplest way I could think of is, well, another enum. All expressions needs
// to be manually implemented the trait that returns that enum.
//
// Most of the expression types will just forward the type request to its child
// anyways.

/// Either just a wrapper around a primary expression, or a numerical primary
/// expression with an unary operator.
///
/// # Rule
/// \<arith-unary-expr\> ::= ("+" | "-")? \<prim-expr\>
///
/// # Note
/// - If there is "+" or "-" at the beginning, \<prim-expr\>'s type must be
///   number.
///
/// # See also
/// [`PrimaryExpr`]
ArithUnaryExpr {
    primary: PrimaryExpr,
    unary_op: ArithUnOp,
}

PrimaryExpr {
    typ: PrimaryExprType,
}

);

// a bunch of variants.
// God damn it, where is my anonymous enum?

#[derive(Debug, PartialEq, Eq, Clone)]
enum PrimaryExprType {
    LiteralNum(u64),
    LiteralString(String),
    // must be a box otherwise it's an infinite definition recursion.
    GroupedExpr(Box<Expr>),
}

/// Operators for [`TermExpr`].
///
/// # Rule
/// \<term-op\> ::= "+" | "-"
#[derive(Debug, PartialEq, Eq, Clone)]
#[repr(u8)]
enum TermOp {
    Plus,
    Minus,
}

/// Operators for [`FactorExpr`].
///
/// # Rule
/// \<factor-op\> ::= "*" | "/"
#[derive(Debug, PartialEq, Eq, Clone)]
#[repr(u8)]
enum FacOp {
    Multiply,
    Divide,
}

#[repr(u8)]
#[derive(Debug, PartialEq, Eq, Clone)]
enum ArithUnOp {
    Negate,
    Plus,
}

// TODO: actually start recursively descending. Let's go.

impl AstNode for PrimaryExpr {
    fn parse(tokens: &mut VecDeque<Token>) -> Result<Self, Option<ParseError>> {
        let Some((tok_type, line, pos)) = tokens.pop_front().map(Token::bind)
        else {
            return Err(None);
        };

        match tok_type {
            TokenType::Integer(s_in) => {
                debug_assert!(s_in.parse::<u64>().is_ok());
                Ok(Self {
                    typ: PrimaryExprType::LiteralNum(s_in.parse().unwrap()),
                })
            }
            TokenType::String(s) => Ok(Self {
                typ: PrimaryExprType::LiteralString(s),
            }),
            TokenType::LPBrace => todo!(
                "PrimaryExpr parse: Grouped expression not implemented yet"
            ),
            _ => Err(Some(ParseError {
                typ: ParseErrorType::UnexpectedToken(tok_type),
                line,
                pos,
            })),
        }
    }
}

impl AstNode for ArithUnaryExpr {
    fn parse(tokens: &mut VecDeque<Token>) -> Result<Self, Option<ParseError>> {
        let Some((tok_type, line, pos)) = tokens.pop_front().map(Token::bind)
        else {
            return Err(None);
        };

        // TODO: change this sloppy type-checking once I have a nicer
        // type-checking trait.
        let primary = PrimaryExpr::parse(tokens)
            .and_then(|exp| match exp.typ {
                PrimaryExprType::LiteralNum(_)
                | PrimaryExprType::GroupedExpr(_) => Ok(exp),
                PrimaryExprType::LiteralString(_) => Err(Some(ParseError {
                    typ: ParseErrorType::UnexpectedToken(tok_type.clone()),
                    line,
                    pos,
                })),
            })
            .map_err(|e| {
                if e.is_none() {
                    Some(ParseError {
                        typ: ParseErrorType::ExpectedExpr,
                        line,
                        pos,
                    })
                } else {
                    e
                }
            })?;

        if tok_type == TokenType::Dash {
            Ok(Self {
                unary_op: ArithUnOp::Negate,
                primary,
            })
        } else if tok_type == TokenType::Plus {
            Ok(Self {
                unary_op: ArithUnOp::Plus,
                primary,
            })
        } else {
            tokens.push_front(Token::new(tok_type, line, pos));
            todo!()
        }
    }
}
