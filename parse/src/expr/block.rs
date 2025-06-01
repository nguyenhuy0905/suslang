use std::collections::VecDeque;

use tokenize::Token;

use crate::{ParseError, Stmt};

use super::{ExprAst, ExprBoxWrap, ExprParse};

#[derive(Debug, Clone, PartialEq)]
pub struct BlockExpr {
    pub statements: Vec<Stmt>,
}

impl ExprAst for BlockExpr {}

impl ExprParse for BlockExpr {
    fn parse(
        tokens: &mut VecDeque<Token>,
        line: usize,
        pos: usize,
    ) -> Result<(ExprBoxWrap, usize, usize), Option<ParseError>> {
        todo!()
    }
}
