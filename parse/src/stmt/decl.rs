use std::{any::Any, fmt::Debug};

/// Statement AST node tag.
///
/// Must also implement traits [`Debug`], [`Clone`], [`PartialEq`] for blanket
/// [`DeclStmtImpl`]
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
