## Todo

- This is another codebase nuke.
- The OOP-ish way before irritated me quite a lot. Now Imma enum.
  - Why, because I *need* to know the underlying type.
  - I can try to cope by adding all the `Into<SomeType>` stuff. But that's
    a lot of maintenance: every time I add a new struct, I need to, for all
    other structs, add another `Into`.
  - Or, a visitor, whose best implementation is, well, an enum.
- Anyways, enough rant, onto the actual to-do.

- [ ] Enum `Expr`:
  - [ ] `Primary`, which is another enum:
    - [ ] `Identifier`
    - [ ] `String`
    - [ ] `Integer`
    - [ ] `Float`
    - [ ] `Boolean`
    - [ ] `ProcCall`
    - If meeting a `()`, simply delegate back to `Expr`'s parse function.
  - [ ] `Unary`
  - [ ] `Factor`
  - [ ] `Term`
  - [ ] `BitAnd`
  - [ ] `BitXor`
  - [ ] `BitOr`
  - [ ] `Comparison`
  - [ ] `LogicAnd`
  - [ ] `LogicOr`

- [ ] Enum `Stmt`:
  - [ ] `Expr` statement
    - [ ] `Return`, `BlockReturn`
    - [ ] `Semicolon` (an `Expr` followed by a semicolon)
  - [ ] `Let` statement
    - Do I add a `Def`/`Const` statement for all immutable types (say, function
      decl)?
    - Or, `Let` with `Mut`?
      - If not `Mut`, allow some more declaration types?
        - Need a new enum for that probably.
      - If `Mut`, just same-old expression.

- [ ] Extend `Expr`:
  - [ ] `Block`
    - Should I start assigning block scope here?
    - At this stage, there should be enough info already. I can look for any
      `Let` statements (or `Def`/`Const` if I also add them).
  - [ ] `Proc`
    - If I do go the route of `Def` stmt, `Proc` should be in a different enum
      type than `Expr`.
    - In its place here can be, say, `Lambda`.
  - [ ] `If`
    - [ ] `IfBranch`
    - [ ] `ElseBranch`
    - [ ] `ElifBranch`
  - [ ] `While`

- Should `Assignment` be an `Expr` or `Stmt`?

- Do I add type info?
  - Do I add type info *during* tokenization instead?
  - If yes, think about module or namespace system, and generics
    - How to declare a module/namespace?
      - Explicit declaration?
      - Or based on file name?
    - How to refer to a type?
      - I will need to modify how `Identifier` is parsed.
      - In the same module/namespace?
      - In a different module/namespace?
        - Import a module/namespace?
        - Relative path to refer to children modules/namespaces?
        - Aliasing? With `Let` or `Def`/`Const`?
    - Sizes of types?
    - Generics? Conversion between types? Or simply type annotation that gets
      checked in runtime?
      - Name mangling?
  - If I wanna make this into a query language (with flares), Imma just rely on
    runtime type checking.

- [ ] Semantics analysis
  - [ ] Assign scope to blocks
    - Or, do I do this way up during parsing a `Block`?
