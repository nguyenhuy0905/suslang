// TODO: remove allow dead_code on top of file
#![allow(dead_code)]

#[repr(u8)]
pub enum ParseError {
    Done, // not exactly an error, but it's convenient
    UnexpectedToken,
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
///
/// # Example
/// ```rust
/// decl_nodes!(
/// /// Some docs
/// #[derive(Copy)]
/// Statement {
///     sussy: Vec<u8>,
/// }
///
/// /// Some more docs
/// Expression {
///    more_sussy: Vec<i16>,
/// }
/// )
/// ```
macro_rules! decl_nodes {
    ($($(#[$attr:meta])* $name:ident $blk:tt)+) => (
        // swapped around so that highlighting works as intended.
        $($(#[$attr])*
            pub struct $name $blk)*

        /// All the node types, flagged. Only useful as a way to check the type
        /// of node without relying on downcasting.
        #[derive(Clone, Copy, Debug, PartialEq, Eq)]
        #[repr(u8)]
        pub enum NodeType {
            $($name),*
        }
        $(impl AstNode for $name {
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
trait AstNode {
    fn node_type(&self) -> NodeType;
}

/// A less simple trait to check if a node if of a specified type.
trait AstNodeTypeCmp<T: AstNode> {
    #[must_use]
    fn is(&self) -> bool {
        false
    }
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
/// \<factor-op\> ::= "*" | "/"
///
/// # See also
/// - [`ArithUnaryExpr`]
FactorExpr {
    first_factor: ArithUnaryExpr,
    follow_factors: Vec<(FacOp, ArithUnaryExpr)>,
}

// TODO: finish writing the docs

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

/// Types of literals supported by the language.
enum Literal {
    Num(u64),
    String(String),
}

enum PrimaryExprType {
    Literal(Literal),
    // must be a box otherwise it's an infinite definition recursion.
    GroupedExpr(Box<Expr>),
}

/// Operators for [`TermExpr`].
///
/// # Rule
/// \<term-op\> ::= "+" | "-"
#[repr(u8)]
enum TermOp {
    Plus,
    Minus,
}

/// Operators for [`FactorExpr`].
///
/// # Rule
/// \<factor-op\> ::= "*" | "/"
#[repr(u8)]
enum FacOp {
    Multiply,
    Divide,
}

#[repr(u8)]
enum ArithUnOp {
    Negate,
    Plus,
}

// TODO: actually start recursively descending. Let's go.
