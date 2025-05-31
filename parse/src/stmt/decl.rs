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

/// Variable declaration statement.
///
/// # Rule
/// \<var-decl\> ::= "let" "mut"? ID (":" \<type-ref\>)? "=" \<expr\>
///
/// # Note
/// - When parsing, `TYPE_ID` is equivalent to [`TypeInfoKind::Reference`].
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

#[macro_export]
macro_rules! new_var_decl_expr {
    ($name:expr, None, $init:expr) => {
        VarDeclStmt {
            name: $name.into(),
            typ: None,
            init_val: ExprBoxWrap::new($init),
        }
    };
    ($name:expr, $typ:expr, $init:expr) => {
        VarDeclStmt {
            name: $name.into(),
            typ: Some(NameResolve { resolve: $typ }),
            init_val: ExprBoxWrap::new($init),
        }
    };
}

impl DeclStmtAst for VarDeclStmt {}

impl DeclStmtParse for VarDeclStmt {
    #[allow(clippy::too_many_lines)]
    fn parse(
        tokens: &mut VecDeque<Token>,
        scope: &mut Scope,
        mut line: usize,
        mut pos: usize,
    ) -> Result<(DeclStmtBoxWrap, usize, usize), ParseError> {
        // check for the "let"
        let (new_ln, new_pos) = tokens
            .pop_front()
            .ok_or(ParseError::ExpectedToken { line, pos })
            .map(Token::bind)
            .and_then(|(typ, new_line, new_pos)| {
                if typ == TokenType::Let {
                    Ok((new_line, new_pos))
                } else {
                    Err(ParseError::UnexpectedToken(Token::new(
                        typ, new_line, new_pos,
                    )))
                }
            })?;

        // get the identifier
        let (name, new_ln, new_pos) = tokens
            .pop_front()
            .ok_or(ParseError::ExpectedToken {
                line: new_ln,
                pos: new_pos,
            })
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
        // set position of line and pos to that of the identifier
        (line, pos) = (new_ln, new_pos);

        // Get the type if there's the annotation
        let type_anno: Option<(NameResolve, usize, usize)> = tokens
            .front()
            .ok_or(ParseError::ExpectedToken {
                line: new_ln,
                pos: new_pos,
            })
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
            .inspect(|opt| {
                opt.inspect(|_| {
                    tokens.pop_front();
                });
            })
            // change to Option<Result<...>>
            .transpose()
            // if Some then check for the name identifier
            .map(|tok_res| {
                tok_res.and_then(|(line, pos)| {
                    // if there's no token after this, it's an error.
                    tokens
                        .front()
                        .ok_or(ParseError::ExpectedToken { line, pos })
                        // otherwise, it must be a valid NameResolve
                        .map(|tok| (tok.line_number(), tok.line_position()))
                        .and_then(|(line, pos)| {
                            NameResolve::parse(tokens, line, pos)
                        })
                })
                // otherwise just return the same error
            })
            // change back to Result<Option<...>>
            .transpose()?; // and try get the Option<...> inside

        let (typ, new_ln, new_pos): (Option<NameResolve>, _, _) = type_anno
            .map(|(type_anno, line, pos)| (Some(type_anno), line, pos))
            .unwrap_or((None, new_ln, new_pos));
        if let Some((def_ln, def_pos, _)) = scope.symbols.insert(
            name.clone(),
            (line, pos, typ.clone().map(TypeInfoKind::Reference)),
        ) {
            return Err(ParseError::SymbolAlreadyExists {
                name,
                line: def_ln,
                pos: def_pos,
            });
        }

        // check for the equal
        tokens
            .pop_front()
            .ok_or(ParseError::ExpectedToken {
                line: new_ln,
                pos: new_pos,
            })
            .and_then(|tok| {
                if matches!(tok.tok_typ, TokenType::Equal) {
                    Ok(())
                } else {
                    Err(ParseError::UnexpectedToken(tok))
                }
            })?;

        let (init_val, init_ln, init_pos) =
            Expr::parse(tokens).map_err(|e| match e {
                None => ParseError::ExpectedToken {
                    line: new_ln,
                    pos: new_pos,
                },
                Some(e) => e,
            })?;

        Ok((
            DeclStmtBoxWrap::new(Self {
                name,
                typ,
                init_val,
            }),
            init_ln,
            init_pos,
        ))
    }
}

/// Procedure definition
///
/// # Rule
/// \<proc-defn\> ::= "proc" ID "(" \<proc-params\> ")" \<block-expr\>
///
/// TODO: \<proc-params\> and \<block-expr\>
#[derive(Debug, Clone, PartialEq)]
pub struct ProcDefnStmt {}

impl DeclStmtAst for ProcDefnStmt {}

/// Procedure parameters
///
/// # Rule
/// \<proc-params\> ::= (ID ":" \<type-ref\> ("," ID ":" \<type-ref\>)* )? ","?
///
/// # See also
/// - [`NameResolve`]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProcParams {
    pub params: HashMap<String, NameResolve>,
}

#[macro_export(local_inner_macros)]
macro_rules! new_proc_params {
    (($first_name:expr,$first_resolve:expr) $(, ($name:expr,$resolve:expr))*) => {
        ProcParams {
            params: HashMap::from([(
                String::from($first_name),
                $first_resolve,
            ), $((String::from($name), $resolve),)*]),
        }
    };
}

impl ProcParams {
    /// Parses into a `ProcParams` using the input tokens.
    ///
    /// # Parameters
    /// - `tokens` The input tokens.
    /// - `line`, `pos` The position of the token just before the first in
    ///   `tokens`. If no token has been popped out, use `(line, pos) = (1, 1)`.
    ///
    /// # Errors
    /// - If it fails, it fails, returning a `ParseError`. Currently, there are
    ///   some ways through which it could fail:
    ///   - If the token after an `Identifier` is nonexistent or is not a `Colon`,
    ///     it's an `ExpectedToken` if empty or `UnexpectedToken` if not.
    ///   - After that, if parsing [`NameResolve`] fails, the error of calling
    ///     [`NameResolve::parse`] is percolated up.
    pub fn parse(
        tokens: &mut VecDeque<Token>,
        line: usize,
        pos: usize,
    ) -> Result<(Self, usize, usize), ParseError> {
        // Convenient function (closure-that-captures-nothing) to get an
        // identifier.
        //
        // If the next token is an identifier indeed, return Some((identifier-name,
        // identifier-line, identifier-position)). Otherwise, return None.
        let get_id =
            |tokens: &mut VecDeque<Token>| -> Option<(String, usize, usize)> {
                tokens
                    .front()
                    .and_then(|tok| match tok.tok_typ {
                        // we have an identifier yay
                        TokenType::Identifier(_) => Some(()),
                        // if nothing then it's the end of us
                        _ => None,
                    })
                    .and_then(|()| tokens.pop_front())
                    .map(|tok| {
                        (
                            match tok.tok_typ {
                                TokenType::Identifier(s) => s,
                                _ => unreachable!(),
                            },
                            tok.line_number,
                            tok.line_position,
                        )
                    })
            };
        // Should be called after an identifier. First detects if the next token is Colon, and
        // errors out otherwise. If it is indeed a Colon, pop it out and try parse (some of) the
        // following tokens into a `NameResolve`, returning the result of calling
        // `NameResolve::parse`
        //
        // # Parameters
        // - `tokens` The same token list passed in.
        // - `line` `pos` Same stuff returned by `get_id`
        let get_type_anno =
            |tokens: &mut VecDeque<Token>,
             line: usize,
             pos: usize|
             -> Result<(NameResolve, usize, usize), ParseError> {
                tokens
                    .pop_front()
                    // if empty, it's an error
                    .ok_or(ParseError::ExpectedToken { line, pos })
                    // otherwise, it must be a Colon
                    .and_then(|tok| {
                        if matches!(tok.tok_typ, TokenType::Colon) {
                            Ok((tok.line_number, tok.line_position))
                        } else {
                            Err(ParseError::UnexpectedToken(tok))
                        }
                    })
                    // if the stars align, try parse the other tokens into a
                    // `NameResolve`
                    .and_then(|(old_ln, old_pos)| {
                        NameResolve::parse(tokens, old_ln, old_pos)
                    })
            };

        let Some((first_id, first_ln, first_pos)) = get_id(tokens) else {
            return Ok((
                Self {
                    params: HashMap::new(),
                },
                line,
                pos,
            ));
        };
        let (first_anno, first_anno_ln, first_anno_pos) =
            get_type_anno(tokens, first_ln, first_pos)?;

        let mut params = HashMap::from([(first_id, first_anno)]);
        let (mut ret_ln, mut ret_pos) = (first_anno_ln, first_anno_pos);
        while let Some((comma_ln, comma_pos)) = tokens
            // if next token is comma,
            .front()
            .and_then(|tok| {
                if matches!(tok.tok_typ, TokenType::Comma) {
                    Some((tok.line_number, tok.line_position))
                } else {
                    None
                }
            })
            // pop that token
            .inspect(|_| {
                tokens.pop_front();
            })
        // and go on with the loop
        {
            let Some((next_id, id_ln, id_pos)) = get_id(tokens) else {
                return Ok((Self { params }, comma_ln, comma_pos));
            };
            let (next_anno, anno_ln, anno_pos) =
                get_type_anno(tokens, id_ln, id_pos)?;
            params.insert(next_id, next_anno);
            (ret_ln, ret_pos) = (anno_ln, anno_pos);
        }
        Ok((Self { params }, ret_ln, ret_pos))
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

impl DeclStmtAst for TypeDefnStmt {}

// TODO: impl StmtParse for TypeDefnStmt {}
