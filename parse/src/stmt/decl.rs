use tokenize::{Token, TokenType};

use crate::{Expr, ExprBoxWrap, ExprParse, ParseError};

use super::{Type, TypeImpl};
use std::{
    any::Any,
    collections::{HashMap, VecDeque},
    fmt::Debug,
    hash::{Hash, Hasher},
};

/// Statement AST node tag.
///
/// Must also implement traits [`Debug`], [`Clone`], [`PartialEq`] for blanket [`StmtImpl`]
/// implementation.
pub trait DeclStmtAst: Any + Debug {}

/// Blanket implementation for [`DeclStmtAst`]
pub trait DeclStmtImpl: DeclStmtAst {
    /// Double-dispatch comparison.
    fn accept_cmp(&self, other: &dyn DeclStmtImpl) -> bool;
    /// Dispatched clone.
    fn boxed_clone(&self) -> Box<dyn DeclStmtImpl>;
}

impl<T> DeclStmtImpl for T
where
    T: DeclStmtAst + Clone + PartialEq,
{
    fn accept_cmp(&self, other: &dyn DeclStmtImpl) -> bool {
        (other as &dyn Any).downcast_ref::<T>() == Some(self)
    }

    fn boxed_clone(&self) -> Box<dyn DeclStmtImpl> {
        Box::new(self.clone())
    }
}

/// Wrapper around a `dyn` [`DeclStmtImpl`].
pub struct DeclStmtBoxWrap {
    pub val: Box<dyn DeclStmtImpl>,
}

impl DeclStmtBoxWrap {
    pub fn new<T: DeclStmtImpl>(v: T) -> Self {
        Self { val: Box::new(v) }
    }
}

impl Debug for DeclStmtBoxWrap {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.val.fmt(f)
    }
}

impl PartialEq for DeclStmtBoxWrap {
    fn eq(&self, other: &Self) -> bool {
        self.val.accept_cmp(other.val.as_ref())
    }
}

impl Clone for DeclStmtBoxWrap {
    fn clone(&self) -> Self {
        Self {
            val: self.val.boxed_clone(),
        }
    }
}

impl AsRef<dyn DeclStmtImpl> for DeclStmtBoxWrap {
    fn as_ref(&self) -> &dyn DeclStmtImpl {
        self.val.as_ref()
    }
}

pub trait DeclStmtParse: DeclStmtImpl {
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
    ) -> Result<(DeclStmtBoxWrap, usize, usize), ParseError>;
}

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
    // map of (symbol name, (line, position, symbol type)).
    // At parsing stage, some auto-inferred declarations cannot be inferred
    // yet; in which case, symbol type is None.
    pub symbols: HashMap<String, (usize, usize, Option<TypeInfoKind>)>,
    // parent module.
    pub parent_idx: Option<NameResolve>,
}

/// A type information can either be a definition or a reference of a type.
///
/// - `Reference`: contains the name to resolve.
/// - `Definition`: contains the name of the defined type and its definition.
#[derive(Debug, PartialEq, Eq)]
pub enum TypeInfoKind {
    // Defines a type
    Definition(Box<dyn TypeImpl>),
    // Refers to a type defined somewhere.
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
    /// Parses the token list `tokens` into a `NameResolve`.
    ///
    /// # Parameters
    /// - `tokens` The token list
    /// - `line`, `pos` The position of the token before the first token in
    ///   `tokens` (aka, the token popped) just before `tokens` is passed into
    ///   this function.
    ///   If there's no token before the first in `tokens`, put `1, 1`
    ///   in.
    ///
    /// # Errors
    /// - If a `ColonColon` is not followed by "overlord" or an `Identifier`.
    /// - If the very first token parsed is a `ColonColon`, but the token
    ///   following that is not an `Identifier`.
    ///   - No one in their right mind globs the entire global module.
    #[allow(clippy::missing_panics_doc)]
    pub fn parse(
        tokens: &mut VecDeque<Token>,
        mut line: usize,
        mut pos: usize,
    ) -> Result<(Self, usize, usize), ParseError> {
        // first (one or two) token(s) must be some type or module, or "::"
        // followed by a type or module.
        // If the next token is "::" then its next token must be another type
        // or module.
        // If the next token is something else then we could say name
        // resolution ends.

        // Assuming the parser just matches a Global resolve step. This
        // function pushes the Global resolve onto `acc_vec`, then checks the
        // next token, taken from `tok_lst`.
        //
        // If that token does not correspond to Child resolve step, returns an
        // error; otherwise, return the position of the token just resolved.
        let resolve_global = |tok_lst: &mut VecDeque<Token>,
                              acc_vec: &mut Vec<ResolveStep>|
         -> Result<(usize, usize), ParseError> {
            // Global resolve must be the first item
            debug_assert!(acc_vec.is_empty());
            acc_vec.push(ResolveStep::Global);
            tok_lst
                .pop_front()
                .ok_or(ParseError::ExpectedToken { line, pos })
                .and_then(|tok| match tok.tok_typ {
                    TokenType::Identifier(s) => {
                        acc_vec.push(ResolveStep::Child(s));
                        Ok((tok.line_number, tok.line_position))
                    }
                    _ => Err(ParseError::UnexpectedToken(tok)),
                })
        };
        // Check `first_tok` to see which type of name resolution it asks for.
        // For Global resolution, requires one extra token from `tok_lst`.
        let init_resolve_vec = |first_tok: Token,
                                tok_lst: &mut VecDeque<Token>|
         -> Result<
            (Vec<ResolveStep>, usize, usize),
            ParseError,
        > {
            let mut acc = Vec::new();
            match first_tok.tok_typ {
                TokenType::Identifier(s) => {
                    acc.push(ResolveStep::Child(s));
                    Ok((acc, first_tok.line_number, first_tok.line_position))
                }
                TokenType::Overlord => {
                    acc.push(ResolveStep::Parent);
                    Ok((acc, first_tok.line_number, first_tok.line_position))
                }
                TokenType::ColonColon => resolve_global(tok_lst, &mut acc)
                    .map(|(tok_ln, tok_pos)| (acc, tok_ln, tok_pos)),
                _ => Err(ParseError::UnexpectedToken(first_tok)),
            }
        };

        // actual logic starts here

        let (mut resolve, new_ln, new_pos): (Vec<_>, usize, usize) = tokens
            .pop_front()
            // must have at least 1 token
            .ok_or(ParseError::ExpectedToken { line, pos })
            .and_then(|tok| init_resolve_vec(tok, tokens))?;
        (line, pos) = (new_ln, new_pos);

        while let Some(TokenType::ColonColon) =
            tokens.front().map(Token::token_type)
        {
            // get line and position for error-handling
            (line, pos) = tokens
                .pop_front()
                .map(|tok| (tok.line_number, tok.line_position))
                // read the while-condition
                .unwrap();
            tokens
                .pop_front()
                .ok_or(ParseError::ExpectedToken { line, pos })
                .and_then(|tok| match tok.tok_typ {
                    TokenType::Identifier(s) => {
                        resolve.push(ResolveStep::Child(s));
                        Ok(())
                    }
                    TokenType::Overlord => {
                        resolve.push(ResolveStep::Parent);
                        Ok(())
                    }
                    _ => Err(ParseError::UnexpectedToken(tok)),
                })?;
        }

        Ok((Self { resolve }, line, pos))
    }
}

impl Type for NameResolve {}

/// A single step in name resolution.
///
/// - `Parent`: go to the parent scope.
/// - `Child`: go to the child scope (or type) with the specified name.
/// - `Global`: can only appear as the very first step. Start searching at the
///   global scope.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ResolveStep {
    Parent,
    Global,
    Child(String),
}
