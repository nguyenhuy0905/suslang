## Todo

- Last updated June 3rd 2025.
  - Add error test cases for LetStmt. DONE
  - Add error test cases for BlockExpr.
  - Implement a proc-call expr with equal precedence to the current highest
    (that is, PrimaryExpr).
  - Implement if-elif-else expr.
  - Implement while expr.
  - Implement a block-escape statement. With probably a new keyword.
  - Start rolling the dummy semantics analysis system.
    - Assign blocks to scopes.
    - Note: no need for type check, even if I have type annotation. That stuff
      happens run-time.
  - What's next: finish all the items above first.
