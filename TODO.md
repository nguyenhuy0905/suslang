## Todo

- Last updated June 11th 2025.
  - [x] Add error test cases for LetStmt.
  - [x] Add error test cases for BlockExpr.
  - [x] Implement a proc-call expr with equal precedence to the current highest
    (that is, PrimaryExpr).
  - [x] Add more error test cases for ProcCallExpr (specifically, trailing-comma
    case).
  - [x] Implement if-elif-else expr.
    - [x] Define structs and grammar rules.
    - [x] Define `new_from` for all types of branches,
      - [x] `IfBranch`
      - [x] `ElifBranch`
      - [x] `ElseBranch`
    - [x] Test it out!
      - [x] Make nice macros like I have always done.
        - This time, it's not as nice, but that's because this construct is
          quite complex
      - [x] Then write tests.
        - [x] Write simplest-case checks.
        - [x] Write error cases
  - [x] Implement while expr.
    - [x] Add a new keyword, "while".
    - [x] Implement the thing.
    - [x] Test it out:
      - [x] Non-error case.
      - [x] Error case.
  - [x] Implement a block-escape statement.
    - [x] Add a new keyword, "block_return".
    - [x] Add BlockReturnStmt. Technically similar to ReturnStmt, but the keyword
          is "block_return"
    - [x] Test it out
      - [x] Just copy-paste ReturnStmt's tests but change the keyword, dawg.
  - [ ] Quick fix: assignment statement
    - [x] This has some overlap with `ExprValStmt`, so, make a new rule that
          simply delegates.
    - [x] Then implement `AssignStmt`.
    - [ ] Then write some tests.
      - [x] Add some normie cases.
      - [ ] Add some error cases.
  - [ ] Start rolling the dummy semantics analysis system.
    - [ ] Assign blocks to scopes.
      - Given my current implementation, what I'm thinking of is:
        - [ ] Create a new type `ScopedBlock`. Which is still an `ExprAst`, but
              this time is NOT an `ExprParse`. Well, because one shouldn't be able
              to parse it.
        - [ ] And, convert all `BlockExpr` to `ScopedBlock`.
          - [ ] If it's wrapped behind an `ExprBoxWrap`, tryna downcast it.
            - Man, I shoulda used enums instead.
        - Of course, this is a noble cause for "refactoring," but I'm not gonna
          refactor again until I finish everything I set out to do.
          - At least now I learn that this OOP-styled thingy kinda stinks.
    - Note: no need for type check, even if I have type annotation. That stuff
      happens run-time.
  - What's next: finish all the items above first.
