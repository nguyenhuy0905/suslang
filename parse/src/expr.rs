#[allow(clippy::wildcard_imports)]
use crate::*;
pub mod arith;
pub mod cond;
#[cfg(test)]
mod test;
pub use arith::*;
#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    pub expr: AstBoxWrap,
}

impl Ast for Expr {}

impl AstParse for Expr {
    // TODO: change Expr::parse to something new as new rules are added.
    fn parse(
        tokens: &mut VecDeque<Token>,
    ) -> Result<AstBoxWrap, Option<ParseError>> {
        Ok(AstBoxWrap::new(Self {
            expr: TermExpr::parse(tokens)?,
        }))
    }
}
