
## TODO:

- should idx be a reserved keyword? (it is right now)
- is `a, b, c = 42` allowed?
- `while: .. else: ..`
- `for: .. else: ..`
- array slices: `a[x:y:z]`, `a[x:y,z]`?
- should import statements only be allowed at the top of a file?
- map syntax? `{ 'a': 1, 'b': 2 }`
- lambda?
- assignment expr `foo(a := b)`
- elipsis?

## decided
- chained comparisons are not allowed.
  eg `a < b < c` must be written `a < b and b < c`
- Python's newline escaping for multiline expressions is not supported.
Expressions can span multiple lines if they're wrapped in parens.

Instead of
```
a = 1 + 2 \
    + 3
```
use
```
a = (1 + 2
     + 3)
```

- event defs should be allowed at the module level

### tuples

- Tuples must be wrapped in parentheses. Python's unwrapped tuples aren't supported.

Instead of `a = 1, 2`, use `a = (1, 2)`.
Instead of `a = 1,`, use `a = (1,)` (singleton tuple)

TODO: do we have a use for singleton tuples?


- Tuple desctructuring. Rust style is supported:

`(a, b): (u256, u256) = (1, 2)`
`(quo, rem): (u64, u64) = divmod(5, 2)`

Python style is not currently supported:

`a: u256, b: u256 = (1, 2)`

- `a, b: u256 = 42` is not supported.

### Contract definitions
- fields must come first
- events and fn defs can be intermingled below fields
