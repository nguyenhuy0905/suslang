use std::{
    any::Any,
    collections::{HashMap, VecDeque},
    fmt::Debug,
    hash::{DefaultHasher, Hash, Hasher},
};
use tokenize::{Token, TokenType};

use crate::{Expr, ExprBoxWrap, ExprParse, ParseError};
#[cfg(test)]
mod test;

/// Contains info about a scope: what symbols are declared, what other scopes
/// are pulled in, what the parent scope is.
///
/// A child module inherits everything from its parent (in this goofy language
/// that's called "overlord" for some reason).
///
/// This struct is used for both block scope and module scope. If you think
/// about it, a module is pretty much in the form of, "mod modname {}".
/// If that doesn't look like a block scope to me, I dunno what does.
#[derive(Debug, Clone, PartialEq)]
pub struct Scope {
    // map of (symbol name, symbol type).
    // At parsing stage, some auto-inferred declarations cannot be inferred
    // yet.
    pub symbols: HashMap<String, Option<TypeInfoKind>>,
    // name of this scope.
    pub name: String,
    // parent module.
    pub parent_idx: Option<NameResolve>,
}

/// A type information can either be a definition or a reference of a type.
///
/// - `Reference`: contains the name to resolve.
/// - `Definition`: contains the name of the defined type and its definition.
#[derive(Debug, PartialEq, Eq)]
pub enum TypeInfoKind {
    Definition(Box<dyn TypeImpl>),
    Reference(NameResolve),
}

impl Hash for TypeInfoKind {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Self::Definition(defn) => {
                state.write_u64(defn.get_hash_value());
            }
            Self::Reference(typename) => typename.hash(state),
        }
    }
}

impl Clone for TypeInfoKind {
    fn clone(&self) -> Self {
        match self {
            TypeInfoKind::Definition(defn) => {
                Self::Definition(defn.boxed_clone())
            }
            TypeInfoKind::Reference(typename) => {
                Self::Reference(typename.clone())
            }
        }
    }
}

/// Scope and type resolution.
/// The first step can be `ResolveStep::Global`, but all the other can only be
/// `Parent` or `Child`.
///
/// Unless the first step is `Global`, the resolution starts from the current
/// module.
///
/// # Rule
/// \<type-ref\> ::= "::"? ID ("::" ID)*
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NameResolve {
    pub resolve: Vec<ResolveStep>,
}

#[macro_export]
macro_rules! new_name_resolve {
    ($first:expr $(,$follow:expr)* $(,)?) => {
        NameResolve {
            resolve: vec![$first $(,$follow)*]
        }
    };
}

impl NameResolve {
    fn parse_no_vtable(
        tokens: &mut VecDeque<Token>,
        line: usize,
        pos: usize,
    ) -> Result<(Self, usize, usize), ParseError> {
        // first (one or two) token(s) must be some type or module, or "::"
        // followed by a type or module.
        // If the next token is "::" then its next token must be another type
        // or module.
        // If the next token is something else then we could say name
        // resolution ends.

        let mut resolve = Vec::new();
        // get the first resolve step
        let (tok_typ, line, pos) = tokens
            .pop_front()
            .ok_or_else(|| ParseError::ExpectedToken { line, pos })
            .map(Token::bind)
            .unwrap();
        match tok_typ {
            TokenType::ColonColon => {
                resolve.push(ResolveStep::Global);
                // So, bringing global module to scope is kinda stupid.
                // Since it's in scope already anyways.
                let Some((tok_typ, line, pos)) =
                    tokens.pop_front().map(Token::bind)
                else {
                    return Err(ParseError::ExpectedToken { line, pos });
                };
                if let TokenType::Identifier(s) = tok_typ {
                    resolve.push(ResolveStep::Child(s));
                } else {
                    return Err(ParseError::UnexpectedToken(Token::new(
                        tok_typ, line, pos,
                    )));
                }
            }
            TokenType::Identifier(s) => resolve.push(ResolveStep::Child(s)),
            TokenType::Overlord => resolve.push(ResolveStep::Parent),
            _ => {
                return Err(ParseError::UnexpectedToken(Token::new(
                    tok_typ, line, pos,
                )));
            }
        }
        // get the following resolve steps, if any
        let (line, pos) = {
            let (mut line, mut pos) = (line, pos);
            while matches!(
                tokens.front().map(Token::token_type),
                Some(&TokenType::ColonColon)
            ) {
                // remove the colon
                (line, pos) = tokens
                    .pop_front()
                    .map(|tok| (tok.line_number(), tok.line_position()))
                    .unwrap();
                // following a ColonColon must be an Identifier, or Overlord
                (line, pos) = tokens
                    .pop_front()
                    .ok_or_else(|| ParseError::ExpectedToken { line, pos })
                    .map(Token::bind)
                    .and_then(|(typ, line, pos)| {
                        Ok((
                            (if let TokenType::Identifier(name) = typ {
                                Ok(Some(name))
                            } else if let TokenType::Overlord = typ {
                                Ok(None)
                            } else {
                                Err(ParseError::UnexpectedToken(Token::new(
                                    typ, line, pos,
                                )))
                            })?,
                            line,
                            pos,
                        ))
                    })
                    .map(|(name, line, pos)| {
                        // if it's an identifier (name is Some)
                        name.map(|name| resolve.push(ResolveStep::Child(name)))
                            // otherwise
                            .unwrap_or_else(|| {
                                resolve.push(ResolveStep::Parent);
                            });
                        (line, pos)
                    })?;
            }
            (line, pos)
        };

        Ok((Self { resolve }, line, pos))
    }
}

impl Type for NameResolve {}

impl TypeParse for NameResolve {
    /// Need to make sure `tokens` is not empty before calling this function.
    fn parse(
        tokens: &mut VecDeque<Token>,
        line: usize,
        pos: usize,
    ) -> Result<(TypeInfoKind, usize, usize), ParseError> {
        Self::parse_no_vtable(tokens, line, pos)
            .map(|(ret, line, pos)| (TypeInfoKind::Reference(ret), line, pos))
    }
}

/// A single step in name resolution.
///
/// - `Parent`: go to the parent scope.
/// - `Child`: go to the child scope with the specified name.
/// - `Global`: can only appear as the very first step. Start searching at the
///   global scope.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ResolveStep {
    Parent,
    Global,
    Child(String),
}

/// A tag that a struct is a `Type`. Must implement `Debug`. Y'know, so that
/// we can assert_eq and all.
///
/// Must also implement PartialEq + Hash + Clone in order to get `TypeImpl`
/// auto-implemented.
pub trait Type: Any + Debug {}

/// Auto-impl of stuff for any `Type`.
pub trait TypeImpl: Type {
    /// Double-dispatch shenanigan to compare two `Type`s.
    fn accept_cmp(&self, other: &dyn TypeImpl) -> bool;
    /// Double-dispatch shenanigan to clone the type.
    fn boxed_clone(&self) -> Box<dyn TypeImpl>;
    /// Double-dispatch shenanigan to hash the type.
    fn get_hash_value(&self) -> u64;
}

/// How to parse a type.
pub trait TypeParse: TypeImpl {
    fn parse(
        tokens: &mut VecDeque<Token>,
        line: usize,
        pos: usize,
    ) -> Result<(TypeInfoKind, usize, usize), ParseError>;
}

impl<T> TypeImpl for T
where
    T: Type + PartialEq + Hash + Clone,
{
    fn accept_cmp(&self, other: &dyn TypeImpl) -> bool {
        (other as &dyn Any)
            .downcast_ref::<T>()
            .map_or(false, |typ| self == typ)
    }
    fn boxed_clone(&self) -> Box<dyn TypeImpl> {
        Box::new(self.clone())
    }

    fn get_hash_value(&self) -> u64 {
        let mut hasher = DefaultHasher::new();
        self.hash(&mut hasher);
        hasher.finish()
    }
}

impl PartialEq for dyn TypeImpl {
    fn eq(&self, other: &Self) -> bool {
        self.accept_cmp(other)
    }
}

impl Eq for dyn TypeImpl {}

/// Statement AST node tag.
///
/// Must also implement traits Debug, Clone, PartialEq for blanket [`StmtImpl`]
/// implementation.
pub trait StmtAst: Any + Debug {}

/// Blanket implementation for [`StmtAst`]
pub trait StmtImpl: StmtAst {
    /// Double-dispatch comparison.
    fn accept_cmp(&self, other: &dyn StmtImpl) -> bool;
    /// Dispatched clone.
    fn boxed_clone(&self) -> Box<dyn StmtImpl>;
}

// TODO: I really should give the (line, pos) thingy a struct.
pub trait StmtParse: StmtImpl {
    /// Parse the tokens into a statement, and update the scope passed in if
    /// any definition or block is parsed.
    ///
    /// # Parameters
    /// - `tokens` A [`VecDeque`] of [`Token`] to be parsed.
    /// - `scope` The current scope being parsed.
    /// - `line`, `pos`: line number and position of the token *right before*
    ///   the first token in `tokens`. If `tokens` is empty, they should either
    ///   be `1, 1` or that of the token popped *before* passing `tokens` into
    ///   the function.
    ///
    /// # Return
    /// - On success, return a tuple:
    ///   - First element is the parsed statement.
    ///   - Second and third element are the line number and position of the
    ///     last token parsed.
    ///
    /// # Errors
    /// - If the input tokens cannot form a valid statement, return a
    ///   [`ParseError`] corresponding to the failing reason.
    fn parse(
        tokens: &mut VecDeque<Token>,
        scope: &mut Scope,
        line: usize,
        pos: usize,
    ) -> Result<(StmtAstBoxWrap, usize, usize), ParseError>;
}

impl<T> StmtImpl for T
where
    T: StmtAst + Clone + PartialEq,
{
    fn accept_cmp(&self, other: &dyn StmtImpl) -> bool {
        (other as &dyn Any)
            .downcast_ref::<T>()
            .map_or(false, |val| self == val)
    }

    fn boxed_clone(&self) -> Box<dyn StmtImpl> {
        Box::new(self.clone())
    }
}

/// Wrapper around a `dyn StmtImpl`.
pub struct StmtAstBoxWrap {
    pub val: Box<dyn StmtImpl>,
}

impl StmtAstBoxWrap {
    pub fn new<T: StmtImpl>(v: T) -> Self {
        Self { val: Box::new(v) }
    }
}

impl PartialEq for StmtAstBoxWrap {
    fn eq(&self, other: &Self) -> bool {
        self.val.accept_cmp(other.val.as_ref())
    }
}

impl Clone for StmtAstBoxWrap {
    fn clone(&self) -> Self {
        Self {
            val: self.val.boxed_clone(),
        }
    }
}

impl AsRef<dyn StmtImpl> for StmtAstBoxWrap {
    fn as_ref(&self) -> &dyn StmtImpl {
        self.val.as_ref()
    }
}

/// Variable declaration statement.
///
/// # Rule
/// \<var-decl\> ::= "let" "mut"? ID (":" \<type-ref\>) "=" \<expr\>
///
/// # Note
/// - When parsing, TYPE_ID is equivalent to [`TypeInfoKind::Reference`].
/// - How do we know if a `TypeInfoKind` is a `Reference` or a `Definition`?
///   - We should have a keyword for each kind of type
///     definition. For example, [`proc`](tokenize::TokenType::Proc) before a
///     procedure definition. Tokens that are identifiers will be type
///     references.
/// - And, when parsing, some auto-inferred types are not yet inferrable.
///
/// # See also
/// [`Expr`](crate::Expr)
#[derive(Debug, PartialEq, Clone)]
pub struct VarDeclStmt {
    // symbol name
    pub name: String,
    // type of symbol. If symbol type not immediately resolvable, this value is
    // None.
    pub typ: Option<NameResolve>,
    // initial value of the declaration.
    pub init_val: ExprBoxWrap,
}

impl StmtAst for VarDeclStmt {}

impl StmtParse for VarDeclStmt {
    fn parse(
        tokens: &mut VecDeque<Token>,
        scope: &mut Scope,
        line: usize,
        pos: usize,
    ) -> Result<(StmtAstBoxWrap, usize, usize), ParseError> {
        // check for the "let"
        let (line, pos) = tokens
            .pop_front()
            .ok_or(ParseError::ExpectedToken { line, pos })
            .map(Token::bind)
            .and_then(|(typ, new_line, new_pos)| {
                if typ != TokenType::Let {
                    Err(ParseError::UnexpectedToken(Token::new(
                        typ, new_line, new_pos,
                    )))
                } else {
                    Ok((new_line, new_pos))
                }
            })?;

        // check for the identifier
        let (line, pos) = tokens
            .pop_front()
            .ok_or(ParseError::ExpectedToken { line, pos })
            .map(Token::bind)
            .and_then(|(typ, new_line, new_pos)| {
                if typ != TokenType::Let {
                    Err(ParseError::UnexpectedToken(Token::new(
                        typ, new_line, new_pos,
                    )))
                } else {
                    Ok((new_line, new_pos))
                }
            })?;

        // get the identifier
        let (name, line, pos) = tokens
            .pop_front()
            .ok_or(ParseError::ExpectedToken { line, pos })
            .map(Token::bind)
            .and_then(|(typ, line, pos)| {
                Ok((
                    if let TokenType::Identifier(name) = typ {
                        Ok(name)
                    } else {
                        Err(ParseError::UnexpectedToken(Token::new(
                            typ, line, pos,
                        )))
                    }?,
                    line,
                    pos,
                ))
            })?;

        // Get the type if there's the annotation
        let type_anno = tokens
            .front()
            .ok_or_else(|| ParseError::ExpectedToken { line, pos })
            .map(|tok| {
                // if there's a Colon
                if tok.token_type() == &TokenType::Colon {
                    // mark this to proceed with getting the type
                    Some((tok.line_number(), tok.line_position()))
                } else {
                    // or stop otherwise
                    None
                }
            })
            // then remove that Colon
            .inspect(|_| {
                tokens.pop_front();
            })
            // change to Option<Result<...>>
            .transpose()
            // if Some then check for the name identifier
            .and_then(|tok_res| {
                Some(tok_res.and_then(|(line, pos)| {
                    // if there's no token after this, it's an error.
                    tokens
                        .front()
                        .ok_or_else(|| ParseError::ExpectedToken { line, pos })
                        // otherwise, it must be a valid NameResolve
                        .map(|tok| (tok.line_number(), tok.line_position()))
                        .and_then(|(line, pos)| {
                            NameResolve::parse_no_vtable(tokens, line, pos)
                        })
                }))
                // otherwise just return the same error
            })
            // change back to Result<Option<...>>
            .transpose()?;

        let (typ, line, pos) = type_anno
            .map(|(type_anno, line, pos)| (Some(type_anno), line, pos))
            .unwrap_or((None, line, pos));
        let init_val = Expr::parse(tokens).map_err(|e| match e {
            None => ParseError::ExpectedToken { line, pos },
            Some(e) => e,
        })?;

        scope.symbols.insert(
            name.clone(),
            typ.clone().map(|t| TypeInfoKind::Reference(t)),
        );
        Ok((
            StmtAstBoxWrap::new(Self {
                name,
                typ,
                init_val,
            }),
            line,
            pos,
        ))
    }
}

/// Type definition statement.
///
/// Includes type and procedure definition and alias.
/// Aliases can be type aliases or module aliases. Or module import.
///
/// # Note
/// - At the parsing stage, a typedef or a module reference is no different.
///
/// # Rule
/// \<type-defn\> ::= "def" (ID = (\<defn\> | \<type-ref\>)
///                         | "import" = \<type-ref\>)
/// \<defn\> ::= \<type-defn\> | \<module-defn\>
#[derive(Debug, Clone, PartialEq)]
pub struct TypeDefnStmt {}

impl StmtAst for TypeDefnStmt {}

// TODO: impl StmtParse for TypeDefnStmt {}
