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
