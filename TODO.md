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
  - [ ] Implement a block-escape statement. With probably a new keyword.
  - [ ] Start rolling the dummy semantics analysis system.
    - [ ] Assign blocks to scopes.
    - [ ] Note: no need for type check, even if I have type annotation. That stuff
      happens run-time.
  - [ ] What's next: finish all the items above first.
