# Fe compiler architecture

The Fe project is split into multiple crates:

- `fe`: The command-line interface. Mostly calls out to the "driver" crate to compile the given fe code.

- `driver`: Contains the main `compile` function that takes in a file path, and returns a compiled program.
This calls code from the parser, analyzer, lowering, yulgen, and yulc crates to do the actual work.

- `parser`: The lexer splits the program text into discrete tokens. The parser checks whether the stream of tokens compose a syntactically correct program, and build an abstract syntax tree (AST)

- `analyzer`: Checks whether the AST is semantically correct, and collects the types and attribute information
needed by later stages.

- `lowering`: Replaces some high level fe code (syntactic sugar) with its lower-level equivalent.
For example, tuples are replaced with equivalent structs.

- `yulgen`: Generates yul code from the lowered fe AST, using types and attributes gathered by the analyzer.
Yul is an intermediate representation language defined by the solidity project, designed to be easy
to read, and easy to compile to EVM bytecode.

- `yulc`: Compiles yul code down to EVM bytecode (using the solidity compiler)

- `common`: Contains some code that's shared by the other crates, like code for generating and printing diagnostics (errors and warnings for problems in the user's fe code).

- `test-files`: Contains example fe source code files that are used in tests.
- `test-utils`: Code for executing fe (and solidity) on the evm, for testing purposes.
- `ests`: High-level tests of fe functionality, tested by running fe code on the evm.
Tests of specific parser, analyzer, etc functionality are in the individual crates.


## Lexer/tokenizer

Tokens and the rules for lexing them are specified in `crates/parser/src/lexer/token.rs`. The actual lexing
is performed by the [logos crate](https://crates.io/crates/logos).

The rules for lexing tokens are specified as regular expressions in `parser/src/lexer/token.rs`. For example:
```rust
    #[regex("[a-zA-Z_][a-zA-Z0-9_]*")]
    Name,
    #[regex("[0-9]+")]
    Int,
    #[token("true")]
    True,
```

The `Lexer` struct (defined in `parser/src/lexer.rs` simply takes in a source string, then will iterate
through the tokens in that source string. The `Parser` struct owns the `Lexer`.

## Parser
The parsing is implemented as a fairly standard hand-written recursive descent parser.

The `Parser` struct defined in `parser/src/parser.rs` contains the parsing state (token stream, errors, etc)
and utility functions for peeking at the next token, taking the next token, emitting a syntax error, etc.

The actual fe code parsing logic is defined in the files within `parser/src/parser/grammar/`. Each of the parsing
functions takes a `&mut Parser`, consumes some number of tokens, and returns some piece of an AST. For example:
`pub fn parse_contract_def(par: &mut Parser) -> ParseResult<Node<Contract>>`.

The AST (abstract syntax tree) is defined in `parser/src/ast.rs`, as a series of (sometimes recursive)
enums and structs. The root of the ast is a `Module`, which is all of the code defined in a single file.

Expressions are parsed using "top-down operator precedence" (aka Pratt parsing) in `grammar/expressions.rs`.
This is a little tricky to grok at first; there's a nice explanation of the method here:
<https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html>

The parser is only allowed one token of "lookahead"; ie it can peak at the next token in the stream to decide
how to procede. For the most part, the parser doesn't backtrack. The one exception is caused by the fact that the syntax for
a call to a generic function looks a lot like a comparison operation. For example: `foo<10>(x)` could be parsed as
`(foo < 10) > x` ("foo is less than 10 is greater than x"). We resolve this ambiguity in favor of the
generic syntax, and don't allow chained comparison operations. While parsing an expression that starts with `foo<`,
we first try to parse a call to a generic function. If that fails, we backtrack and try to parse a comparison
operation. (Search for `as_bt_parser()` to see how this works.)

The current plan is for the parser to someday return a *concrete* syntax tree, which would provide access to the location of
every token in the source file (eg the parentheses in the expression `(1+2)*3`), so that error messages can easily point
at such characters in case of a syntax error. This concrete tree would then be wrapped by an abtract syntax tree.


## Analyzer
The analyzer uses the [salsa crate](https://crates.io/crates/salsa) to perform its analysis. The actual process of
analyzing a fe program is a bit obscured, unfortunately. To start, a fe module (ie the AST representing a single fe source file) is
"interned" into the salsa database, which gives us a `ModuleId`. We then ask if there are any diagnostics (errors or warnings
to report about the fe code) for that module. This kicks off a series of salsa database "queries" which analyze the
code in the module, and collect any diagnostics.

All high-level language "items" (contracts, structs, functions, etc) are interned into the salsa db and referred to
by their ids, defined in `crates/analyzer/src/namespace/items.rs`.

Each id type has member functions that provide information about that item. These functions take a `&dyn AnalyzerDb`,
and call some query function on that db. `AnalyzerDb` is defined in `src/db.rs`, and the actual query functions
are implemented in `src/db/queries/`. These query functions often call other queries, forming a graph of query dependencies.
One of the benefits of using salsa for all of this, is that it caches the results of these queries automatically,
so we don't have to worry about the cost of recomputing this information, or storing the results somewhere for future use.

Eg to get the functions defined in a contract,
you'd call `some_contract_id.functions(db)`,
which calls `db.contract_function_map(*self)`,
which calls `db.contract_all_functions(..)`, which walks the contract AST structure, interns each function definition,
and collects the resulting `FunctionIds` in a vec.

To then get the return type of one of those functions, you'd do something like `some_fn_id.signature(db)?.return_type?`.

Most of the work of analyzing a program is analyzing the actual code inside fe function bodies,
where types need to be checked, semantic rules need to be enforced, etc.  `fn function_body`
in `src/db/queries/functions.rs` calls out to code in `src/traversal/*.rs`, which does the actual work of recursively
walking the AST structures, deducing the types of expressions, etc.


## Lowering

The interesting stuff here is in `src/mappers/`, which contains functions that map AST structures into other AST structures,
replacing some high-level language constructs to their lower-level equivalents. For example, the tuple type `(u8, u16, bool)`
becomes the struct type
```
struct _tuple_u8_u16_bool_:
  item0: u8
  item1: u16
  item2: bool
```

This is done to make the process of compiling fe code into yul easier. The yulgen code assumes that all tuples have been
replaced by structs, so we only need to generate code for structs and can panic if we see a tuple.

Someday, the lowering phase will likely target some simplified subset of the fe language which has its own simplified AST.


## Yulgen

TODO
