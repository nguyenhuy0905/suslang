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
    ///            | \<block\>
    ///
    /// If matching the rule "(" \<expr\> ")", forward to [`NoBlockExpr`]'s rule.
    /// Otherwise, return this variant if parsing succeeds.
    /// If matching the rule \<block\>, forward to [`WithBlockExpr`]'s rule.
    Primary(PrimaryExpr),
    /// \<proc-call\> ::= \<expr\> "(" \<params\>")"
    /// \<params\> ::= (\<expr\> ("," \<expr\>)* ","?)?
    ///
    /// Yes, you can do something like `if 1 == 1 {a} else {b} ("hello")`
    ProcCall(ProcCallExpr),
    /// \<unary\> ::= ("+" | "-" | "!")? \<lit\>
    ///
    /// If there is ("+" | "-" | "!"), return this variant if parsing succeeds,
    /// otherwise, forward to [`NoBlockExpr::Literal`]'s rule. Otherwise,
    /// return this variant if parsing succeeds.
    Unary(UnaryExpr),
    /// \<binary\> ::= \<unary\> \<bin-op\> \<unary\>
    /// \<bin-op\> ::= "+"
    ///              | "-"
    ///              | "*"
    ///              | "/"
    ///              | "&"
    ///              | "|"
    ///              | "^"
    ///              | "eq"
    ///              | "neq"
    ///              | ">"
    ///              | "<"
    ///              | ">="
    ///              | "<="
    ///              | "and"
    ///              | "or"
    ///              | "not"
    ///              | "."
    Binary(BinaryExpr),
}

#[derive(Debug, Clone, PartialEq)]
pub enum PrimaryExpr {
    String(Box<str>),
    Integer(u64),
    Float(f64),
    Identifier(Box<str>),
    Char(char),
    Bool(bool),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ProcCallExpr {
    pub id_expr: Box<Expr>,
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
    BitAnd,
    /// ^
    BitXOr,
    /// |
    BitOr,
    /// and
    And,
    /// or
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
pub struct WhileExpr {
    pub cond: Box<Expr>,
    pub block: Box<Expr>,
}

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

impl DeclStmt {
    #[must_use]
    pub fn get_id(&self) -> &str {
        match self {
            DeclStmt::Let(let_stmt) => let_stmt.id.as_ref(),
            DeclStmt::LetMut(mutable_let_stmt) => mutable_let_stmt.id.as_ref(),
        }
    }
}

impl std::hash::Hash for DeclStmt {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        // we only allow one definition for each ID.
        match self {
            DeclStmt::Let(let_stmt) => let_stmt.id.hash(state),
            DeclStmt::LetMut(mutable_let_stmt) => {
                mutable_let_stmt.id.hash(state);
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprStmt {
    /// should have a semicolon at the end.
    NoBlock(NoBlockExprStmt),
    /// doesn't need to be followed by a semicolon.
    Block(WithBlockExpr),
}

#[derive(Debug, Clone, PartialEq)]
pub enum NoBlockExprStmt {
    Return(ReturnStmt),
    Expr(NoBlockExpr),
    BlockReturn(BlockReturnStmt),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ReturnStmt {
    pub val: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BlockReturnStmt {
    pub val: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LetStmt {
    pub id: Box<str>,
    pub val: LetDefn,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MutableLetStmt {
    pub id: Box<str>,
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
    pub params: Vec<Box<str>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructDefn {
    pub members: HashSet<Box<str>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Scope {
    pub stmts: Vec<Stmt>,
    pub decls: Vec<Rc<DeclStmt>>,
}
