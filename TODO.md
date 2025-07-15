## Todo

- This is another codebase nuke.
- The OOP-ish way before irritated me quite a lot. Now Imma enum.
  - Why, because I *need* to know the underlying type.
  - I can try to cope by adding all the `Into<SomeType>` stuff. But that's
    a lot of maintenance: every time I add a new struct, I need to, for all
    other structs, add another `Into`.
  - Or, a visitor, whose best implementation is, well, an enum.
- Anyways, enough rant, onto the actual to-do.

- [x] Enum `Expr`:
  - [x] Fix: 2 types of expressions, with and without blocks
  - [x] `NoBlockExpr`:
    - [x] `Primary`, which is another enum:
      - [x] `Identifier`
      - [x] `String`
      - [x] `Integer`
      - [x] `Float`
      - [x] `Boolean`
      - [x] ~`ProcCall`~
      - If meeting a `()`, simply delegate back to `Expr`'s parse function.
    - [x] `ProcCall`
    - ~`Unary`~
    - ~`Factor`~
    - ~`Term`~
    - ~`BitAnd`~
    - ~`BitXor`~
    - ~`BitOr`~
    - ~`Comparison`~
    - ~`LogicAnd`~
    - ~`LogicOr`~
    - [x] `Binary`, encoding all the rules above.
  - [x] `WithBlockExpr`:
    - [x] `Block`
    - [x] `If`
    - [x] `While`

- [x] Enum `Stmt`:
  - [x] `Expr` statement
    - [x] `Return`, `BlockReturn`
    - [x] `Semicolon` (an `Expr` followed by a semicolon) (new name, just,
          `Expr`)
  - [x] `Let` statement
    - Do I add a `Def`/`Const` statement for all immutable types (say, function
      decl)?
    - [x] Or, `Let` with `Mut`?
      - [x] If not `Mut`, allow some more declaration types?
        - [x] Need a new enum for that probably (`LetDefn`):
          - [x] Procedure
          - [x] Struct (need new keyword)

- [ ] Parsing
  - [ ] Primary expr (not done until `Expr` can be parsed)
  - [x] Unary expr
  - [x] Binary expr
    - [x] This time let's try Pratt, instead of recursive descend; I'm creating
          a lot of rules with nonsense names with the latter already.
  - [ ] Some expressions starting with keywords (e.g. `if`) should have the
        same precedence as any `PrimaryExpr`. Add them.
  - [x] Procedure call expr
    - [x] Kind-of done, but cannot be thoroughly tested until
          `Expr::parse_tokens` is ready to go.
    - [x] Be very careful. One possible form of this is `(<expr>)(<expr>)`,
          which looks *very* similar to two `PrimaryExpr` next to one another.
    - [x] Also, I may rename that to `PrimaryExpr`, since `(<expr>)` isn't really
      a primary expr.
  - [x] Block expressions
    - At this point, let's mark a TODO on `Stmt::parse_tokens`.
  - [ ] Statements
    - [ ] No-block
      - [x] `Return` and `BlockReturn`.
      - [x] Check for the semicolon at end.
      - [ ] An expression followed by semicolon.
