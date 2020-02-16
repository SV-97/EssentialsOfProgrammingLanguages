# EssentialsOfProgrammingLanguages

Exercises and implementations from the book "Essentials of programming languages"

- LET is the LET language from the book
- LETI is a modification of LET where I added error handling and tried using the State monad. It also supports lists.
- LETREC is a modification of LETI that supports recursive bindings. I've departed from the book and implemented it via Fixed-Point Expressions and Values.
- PROC is the PROC language from the book supporting lists and nonrecursive functions
- LECAD is a language implementing lexical adressing
- LECADrs is a Rust implementation of LECAD
- LAZY_LECAD is like LECAD but with lazy evaluation (no memoization)
- CIP is a continuation passing interpreter based on PROC
