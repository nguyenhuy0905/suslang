pub use std::collections::HashSet;
pub use std::rc::Rc;

mod parser;
#[cfg(test)]
mod test;
#[allow(unused)]
pub use parser::*;

/// Expression
///
/// \<expr\> ::= \<no-block-expr\> ";" | \<with-block-expr\> ";"?
///
/// \<no-block-expr\> must be followed by a semicolon, while
/// \<with-block-expr\> may or may not be followed by a semicolon.
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    WithBlock(WithBlockExpr),
    NoBlock(NoBlockExpr),
}

/// Expression without block.
///
/// \<no-block-expr\> ::= \<binary\>
#[derive(Debug, Clone, PartialEq)]
pub enum NoBlockExpr {
    /// \<lit\> ::= STRING
    ///            | NUMBER
    ///            | FLOAT
    ///            | "(" \<expr\> ")"
    ///
    /// If matching the rule "(" \<expr\> ")", forward to [`NoBlockExpr`]'s rule.
    /// Otherwise, return this variant if parsing succeeds.
    Literal(LiteralExpr),
    /// \<proc-call\> ::= \<lit\> (\<param-lst\> | "." IDENTIFIER)
    ///
    /// \<param-lst\> ::= "(" \<params\>? ")"
    /// \<params\> ::= \<expr\> ("," \<expr\>)*
    ///
    /// If matching the rule "." IDENTIFIER, forward to [`NoBlockExpr::Access`]
    /// rule. Otherwise, return this variant if parsing succeeds.
    ProcCall(ProcCallExpr),
    /// \<unary\> ::= ("+" | "-" | "!")? \<lit\>
    ///
    /// If there is ("+" | "-" | "!"), return this variant if parsing succeeds,
    /// otherwise, forward to [`NoBlockExpr::Literal`]'s rule. Otherwise,
    /// return this variant if parsing succeeds.
    Unary(UnaryExpr),
    /// \<binary\> ::= \<assignment\>
    /// \<assignment\> ::= \<or-or\> ("=" \<expr\>)?
    /// \<or-or\> ::= \<and-and\> ("||" \<and-and\>)?
    /// \<and-and\> ::= \<bit-or\> ("&&" \<bit-or\>)?
    /// \<bit-or\> ::= \<bit-xor\> ("|" \<bit-xor\>)?
    /// \<bit-xor\> ::= \<bit-and\> ("^" \<bit-and\>)?
    /// \<bit-and\> ::= \<term\> ("&" \<term\>)?
    /// \<term\> ::= \<factor\> (("+" | "-") \<factor\>)?
    /// \<factor\> ::= \<unary\> (("*" | "/") \<unary\>)?
    ///
    /// If only a single \<unary\> is matched, forward to [`NoBlockExpr::Unary`]'s
    /// rule. Otherwise, return this variant if parsing succeeds.
    Binary(BinaryExpr),
}

#[derive(Debug, Clone, PartialEq)]
pub enum LiteralExpr {
    String(Box<str>),
    Integer(u64),
    Float(f64),
    Identifier(Box<str>),
    Char(char),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ProcCallExpr {
    pub id: Box<str>,
    pub params: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AccessExpr {
    pub lhs: Box<Expr>,
    pub rhs: Box<str>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryExpr {
    pub op: UnaryOp,
    pub val: Box<Expr>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Plus,
    Minus,
    Negate,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryExpr {
    pub op: BinaryOp,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    /// +
    Plus,
    /// -
    Minus,
    /// *
    Mul,
    /// /
    Div,
    /// &
    And,
    /// ^
    XOr,
    /// |
    Or,
    /// ==
    Eq,
    /// !=
    Neq,
    /// <
    Lt,
    /// >
    Gt,
    /// <=
    Le,
    /// >=
    Ge,
    /// =
    Assign,
    /// .
    Member,
}

/// Expression with block(s).
///
/// \<with-block-expr\> ::= \<block\> | \<if\> | \<while\>
#[derive(Debug, Clone, PartialEq)]
pub enum WithBlockExpr {
    /// \<block\> ::= "{" \<stmt\>* "}"
    Block(BlockExpr),
    /// \<if\> ::= "if" \<expr\> \<block\>
    ///                ("elif" \<expr\> \<block\>)*
    ///                ("else" \<block\>)?
    If(IfExpr),
    /// \<while\> ::= "while" \<expr\> \<block\>
    While(WhileExpr),
}

#[derive(Debug, Clone, PartialEq)]
pub struct BlockExpr {
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfExpr {
    pub if_branch: IfBranch,
    // maybe we should use the same type to represent both if and elif branches.
    pub elif_branches: Vec<ElifBranch>,
    // there isn't a condition for `ElseBranch`.
    pub else_block: Option<Box<Expr>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfBranch {
    pub cond: Box<Expr>,
    pub block: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ElifBranch {
    pub cond: Box<Expr>,
    pub block: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct WhileExpr {}

/// Statement.
#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Expr(ExprStmt),
    Decl(DeclStmt),
}

#[derive(Debug, Clone, PartialEq)]
pub enum DeclStmt {
    Let(LetStmt),
    LetMut(MutableLetStmt),
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprStmt {
    NoBlock,
    Block,
}

// Using Rc<str> for identifier name here instead of Box<str> because we will
// also be putting the names inside a HashMap.

#[derive(Debug, Clone, PartialEq)]
pub struct LetStmt {
    pub id: Rc<str>,
    pub val: LetDefn,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MutableLetStmt {
    pub id: Rc<str>,
    pub val: Expr,
}

/// Valid right-hand side for [`LetStmt`].
/// Includes [`Expr`] and type/procedure definitions.
#[derive(Debug, Clone, PartialEq)]
pub enum LetDefn {
    Expr(Expr),
    Proc(ProcDefn),
    Struct(StructDefn),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ProcDefn {
    pub name: Rc<str>,
    pub params: Vec<Rc<str>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructDefn {
    pub name: Rc<str>,
    pub members: HashSet<Rc<str>>,
}
