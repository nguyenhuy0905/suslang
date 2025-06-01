use crate::Stmt;

#[derive(Debug, Clone, PartialEq)]
pub struct BlockExpr {
    pub statements: Vec<Stmt>,
}
