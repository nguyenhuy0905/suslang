use crate::*;
pub mod arith;
#[cfg(test)]
mod test;
#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    pub expr: AstBoxWrap,
}

impl Ast for Expr {}
