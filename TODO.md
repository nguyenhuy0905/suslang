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
    - `Binary`, encoding all the rules above.
  - [x] `WithBlockExpr`:
    - [x] `Block`
    - [x] `If`
    - [x] `While`

- [ ] Enum `Stmt`:
  - [ ] `Expr` statement
    - [ ] `Return`, `BlockReturn`
    - [ ] `Semicolon` (an `Expr` followed by a semicolon)
  - [ ] `Let` statement
    - Do I add a `Def`/`Const` statement for all immutable types (say, function
      decl)?
    - [x] Or, `Let` with `Mut`?
      - [x] If not `Mut`, allow some more declaration types?
        - [x] Need a new enum for that probably (`LetDefn`):
          - [x] Procedure
          - [ ] Struct (need new keyword)

- [ ] Parsing
  - [ ] Literal expr (not done until `Expr` is parsed)
  - [x] Unary expr
  - [ ] Binary expr
    - [ ] This time let's try Pratt, instead of recursive descend; I'm creating
          a lot of rules with nonsense names with the latter already.
  - [ ] Block expressions
  - TODO: fill this later.
