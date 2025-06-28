use crate::{ScopedBlock, new_scoped_block};
use parse::{
    BlockExpr, DeclStmtBoxWrap, ExprBoxWrap, ExprStmtBoxWrap, LetStmt,
    LetStmtMut, PrimaryExpr, Stmt, new_assign_stmt, new_block_expr,
    new_let_stmt, new_stmt, stmt::AssignStmt,
};
use std::rc::Rc;

#[test]
fn scoped_block() {
    // simplest, block with nothing inside
    {
        // {}
        let block = new_block_expr!();
        let scoped = ScopedBlock::from(block.clone());
        assert_eq!(scoped, new_scoped_block!((), block));
    }
    // block with no declaration
    {
        // {
        //   hello = goodbye;
        // }
        let block = new_block_expr!(new_stmt!(Expr(new_assign_stmt!(
            "hello",
            PrimaryExpr::Identifier("goodbye".to_string())
        ))));
        let scoped = ScopedBlock::from(block.clone());
        assert_eq!(scoped, new_scoped_block!((), block));
    }
    // block with only declaration
    {
        // {
        //   let hello = goodbye;
        // }
        let block = new_block_expr!(new_stmt!(Decl(new_let_stmt!(
            "hello",
            PrimaryExpr::Identifier("goodbye".to_string()),
            LetStmtMut::Immutable
        ))));
        let scoped = ScopedBlock::from(block.clone());
        assert_eq!(scoped, new_scoped_block!(("hello"), block));
    }
    // a mix with declaration
    {
        // {
        //   let hello = goodbye;
        //   hello = goodbye;
        // }
        let block = new_block_expr!(
            new_stmt!(Decl(new_let_stmt!(
                "hello",
                PrimaryExpr::Identifier("goodbye".to_string()),
                LetStmtMut::Immutable
            ))),
            new_stmt!(Expr(new_assign_stmt!(
                "hello",
                PrimaryExpr::Identifier("goodbye".to_string())
            )))
        );
        let scoped = ScopedBlock::from(block.clone());
        assert_eq!(scoped, new_scoped_block!(("hello"), block));
    }
}
