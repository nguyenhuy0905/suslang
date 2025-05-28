use std::{
    any::Any,
    collections::{HashMap, VecDeque},
    hash::{DefaultHasher, Hash, Hasher},
};

use tokenize::Token;

use crate::ParseError;
#[cfg(test)]
mod test;

/// Contains info about a scope: what symbols are declared, what other scopes
/// are pulled in, what the parent scope is.
///
/// This struct is used for both block scope and module scope. If you think
/// about it, a module is pretty much in the form of, "mod modname {}".
/// If that doesn't look like a block scope to me, I dunno what does.
#[derive(Debug, Clone, PartialEq)]
pub struct Scope {
    // TODO: think of a better way to store env
    pub symbols: HashMap<String, TypeInfoKind>,
    // name of this scope.
    pub name: String,
    // imported module scopes.
    pub imported_idxs: Vec<NameResolve>,
    pub parent_idx: Option<NameResolve>,
}

/// A type information can either be a definition or a reference of a type.
///
/// - `Reference`: contains the name to resolve.
/// - `Definition`: contains the name of the defined type and its definition.
#[derive(Debug, PartialEq, Eq)]
pub enum TypeInfoKind {
    Definition {
        name: String,
        defn: Box<dyn TypeImpl>,
    },
    Reference(NameResolve),
}

impl Hash for TypeInfoKind {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Self::Definition { name, defn } => {
                name.hash(state);
                state.write_u64(defn.get_hash_value());
            }
            Self::Reference(typename) => typename.hash(state),
        }
    }
}

impl Clone for TypeInfoKind {
    fn clone(&self) -> Self {
        match self {
            TypeInfoKind::Definition { name, defn } => Self::Definition {
                name: name.clone(),
                defn: defn.boxed_clone(),
            },
            TypeInfoKind::Reference(typename) => {
                Self::Reference(typename.clone())
            }
        }
    }
}

/// Scope and type resolution.
/// The first step can be `ResolveStep::Global`, but all the other can only be
/// `Parent` or `Child`.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NameResolve {
    pub resolve: Vec<ResolveStep>,
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
pub trait Type: Any + std::fmt::Debug {}

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
    fn parse(tokens: &mut VecDeque<Token>) -> Result<TypeInfoKind, ParseError>;
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

/// Variable declaration statement.
///
/// # Rule
/// \<var-decl\> ::= "let" "mut"? ID (":" TYPE_ID) "=" \<expr\>
///
/// # Note
/// - When parsing, TYPE_ID is equivalent to [`TypeInfoKind::Reference`].
/// - How do we know if a `TypeInfoKind` is a `Reference` or a `Definition`?
///   - We should have a keyword for each kind of type
///     definition. For example, [`proc`](tokenize::TokenType::Proc) before a
///     procedure definition. Tokens that are identifiers will be type
///     references.
///
/// # See also
/// [`Expr`](crate::Expr)
#[derive(Debug, PartialEq, Hash, Clone)]
pub struct VarDeclStmt {}

impl Type for VarDeclStmt {}

/// Type definition statement
///
/// # Rule
/// \<type-defn\> ::= "type" ID "=" \<defn\>
///
/// # TODO
/// - Define the \<defn\> grammar rule.
pub struct TypeDefnStmt {}
