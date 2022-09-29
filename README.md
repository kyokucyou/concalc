# concalc
Console calculator written in Rust.

### Supported features
- predefined constants
- variables[^1]
- predefined functions
- user-defined functions
  - trigonometric (sin, cos, tan)
  - angle conversion (deg, rad)
  - logarithm (for ln, pass only 1 parameter to log)
- basic 4 arithmetic operations and exponentiation, respecting precedence
- ability to save and load environment including user-defined variables and functions
- lazy evaluation of function parameters of certain built-in functions, such as "if", in order to enable recursive function definitions

### Planned features
- ...

[^1]: Lexical scoping is supported. A new scope is created when a function is called, and its parameters are bound in this new scope. If less parameters than required are supplied to a user-defined functions, the parameters will be resolved within the next innermost scope instead, making it possible to use "global" variables as defaults for function parameters.
