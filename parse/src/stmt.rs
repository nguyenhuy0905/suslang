use std::{
    any::Any,
    collections::{HashMap, VecDeque},
    fmt::Debug,
    hash::{DefaultHasher, Hash, Hasher},
};
use tokenize::{Token, TokenType};

use crate::{Expr, ExprBoxWrap, ExprParse, ParseError};
pub mod decl;
pub mod expr;
pub use decl::*;
pub use expr::*;
#[cfg(test)]
mod test;

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
pub trait DeclStmtParse: StmtImpl {
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

impl Debug for StmtAstBoxWrap {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.val.fmt(f)
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
/// \<var-decl\> ::= "let" "mut"? ID (":" \<type-ref\>)? "=" \<expr\>
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

impl StmtAst for VarDeclStmt {}

impl DeclStmtParse for VarDeclStmt {
    fn parse(
        tokens: &mut VecDeque<Token>,
        scope: &mut Scope,
        mut line: usize,
        mut pos: usize,
    ) -> Result<(StmtAstBoxWrap, usize, usize), ParseError> {
        // check for the "let"
        let (new_ln, new_pos) = tokens
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
            .ok_or_else(|| ParseError::ExpectedToken {
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
            .and_then(|tok_res| {
                Some(tok_res.and_then(|(line, pos)| {
                    // if there's no token after this, it's an error.
                    tokens
                        .front()
                        .ok_or_else(|| ParseError::ExpectedToken { line, pos })
                        // otherwise, it must be a valid NameResolve
                        .map(|tok| (tok.line_number(), tok.line_position()))
                        .and_then(|(line, pos)| {
                            NameResolve::parse(tokens, line, pos)
                        })
                }))
                // otherwise just return the same error
            })
            // change back to Result<Option<...>>
            .transpose()?; // and try get the Option<...> inside

        let (typ, new_ln, new_pos): (Option<NameResolve>, _, _) = type_anno
            .map(|(type_anno, line, pos)| (Some(type_anno), line, pos))
            .unwrap_or((None, new_ln, new_pos));
        if let Some((def_ln, def_pos, _)) = scope.symbols.insert(
            name.clone(),
            (line, pos, typ.clone().map(|t| TypeInfoKind::Reference(t))),
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
            .ok_or_else(|| ParseError::ExpectedToken {
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
            StmtAstBoxWrap::new(Self {
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

impl StmtAst for ProcDefnStmt {}

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
                    .and_then(|_| tokens.pop_front())
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
        // NameResolve::parse_no_vtable
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
                    .ok_or_else(|| ParseError::ExpectedToken { line, pos })
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

impl StmtAst for TypeDefnStmt {}

// TODO: impl StmtParse for TypeDefnStmt {}
