use std::{
    any::Any,
    fmt::Debug,
    hash::{DefaultHasher, Hash, Hasher},
};

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
