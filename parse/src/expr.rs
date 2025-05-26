#[allow(clippy::wildcard_imports)]
use crate::*;
pub mod arith;
pub mod cond;
#[cfg(test)]
mod test;
pub use arith::*;
pub use cond::*;
/// Dummy expression struct
///
/// # Rule (for now)
/// \<expr\> ::= \<logic-or-expr\>
///
/// # See also
/// [`LogicOrExpr`]
#[derive(Debug, Clone, PartialEq)]
pub struct Expr {}

impl Ast for Expr {}

impl AstParse for Expr {
    fn parse(
        tokens: &mut VecDeque<Token>,
    ) -> Result<AstBoxWrap, Option<ParseError>> {
        LogicOrExpr::parse(tokens)
    }
}
