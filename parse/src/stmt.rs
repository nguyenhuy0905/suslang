use crate::*;
use tokenize::{Token, TokenType};
#[cfg(test)]
mod test;

/// A statement
///
/// # Rule
/// \<stmt\> ::= (\<var-decl\>) ";"
///
/// # See also
/// [`VarDeclStmt`]
#[derive(Debug, PartialEq, Clone)]
pub struct Stmt {}

impl Ast for Stmt {}

impl AstParse for Stmt {
    /// The caller needs to make sure that `tokens` is not empty.
    /// Why, otherwise it's hard to know where the error position is.
    fn parse(
        tokens: &mut std::collections::VecDeque<Token>,
    ) -> Result<AstBoxWrap, Option<ParseError>> {
        assert!(tokens.front().is_some());
        let (tok_typ, line, pos) = tokens.front().map(Token::bind_ref).unwrap();
        let ret = match tok_typ {
            &TokenType::Let => VarDeclStmt::parse(tokens),
            _ => todo!(
                "Invalid or undefined statements starting with {:#?}",
                Token::new(tok_typ.clone(), line, pos)
            ),
        }?;

        // check for the final semicolon
        let Some((tok_typ, line, pos)) = tokens.pop_front().map(Token::bind)
        else {
            return Err(Some(ParseError::UnendedStmt { line, pos }));
        };
        if !matches!(tok_typ, TokenType::Semicolon) {
            return Err(Some(ParseError::UnexpectedToken(Token::new(
                tok_typ, line, pos,
            ))));
        }
        Ok(ret)
    }
}

/// Variable declaration statement.
///
/// I will probably use this form for declaration of literally everything.
///
/// # Rule
/// \<var-decl\> ::= "let" IDENTIFIER "=" \<expr\>
///
/// # See also
/// [`Expr`]
/// [`TokenType`]
#[derive(Debug, PartialEq, Clone)]
pub struct VarDeclStmt {
    pub name: String,
    pub val: AstBoxWrap,
}

#[macro_export]
macro_rules! new_var_decl_stmt {
    ($name:expr, $val:expr) => {
        VarDeclStmt {
            name: $name.to_owned(),
            val: AstBoxWrap::new($val),
        }
    };
}

impl Ast for VarDeclStmt {}

impl AstParse for VarDeclStmt {
    /// The caller needs to make sure that `tokens` is not empty.
    /// Why, otherwise it's hard to know where the error position is.
    fn parse(
        tokens: &mut std::collections::VecDeque<tokenize::Token>,
    ) -> Result<AstBoxWrap, Option<crate::ParseError>> {
        assert!(tokens.front().is_some());

        // check the "let"
        let (tok_typ, line, pos) = tokens.pop_front().map(Token::bind).unwrap();
        if !matches!(tok_typ, TokenType::Let) {
            return Err(Some(ParseError::UnexpectedToken(Token::new(
                tok_typ, line, pos,
            ))));
        }

        // get the name
        let Some((tok_typ, line, pos)) = tokens.pop_front().map(Token::bind)
        else {
            // this is the previous line and pos binds, not inside this let-else.
            return Err(Some(ParseError::ExpectedToken { line, pos }));
        };
        // fuck you borrow checker
        let Some(name) = (match &tok_typ {
            TokenType::Identifier(s) => Some(s.clone()),
            _ => None,
        }) else {
            return Err(Some(ParseError::UnexpectedToken(Token::new(
                tok_typ, line, pos,
            ))));
        };

        // check the "="
        let Some((tok_typ, line, pos)) = tokens.pop_front().map(Token::bind)
        else {
            // this is the previous line and pos binds, not inside this let-else.
            return Err(Some(ParseError::ExpectedToken { line, pos }));
        };
        if !matches!(tok_typ, TokenType::Equal) {
            return Err(Some(ParseError::UnexpectedToken(Token::new(
                tok_typ, line, pos,
            ))));
        }

        // get the value
        let val = Expr::parse(tokens)?;
        Ok(AstBoxWrap::new(Self { name, val }))
    }
}
