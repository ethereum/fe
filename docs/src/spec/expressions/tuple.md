# Tuple expressions

> **<sup>Syntax</sup>**\
> _TupleExpression_ :\
> &nbsp;&nbsp; `(` _TupleElements_<sup>?</sup> `)`
>
> _TupleElements_ :\
> &nbsp;&nbsp; ( [_Expression_] `,` )<sup>+</sup> [_Expression_]<sup>?</sup>

A *tuple expression* constructs [tuple values].

The syntax for tuple expressions is a parenthesized, comma separated list of expressions, called the *tuple initializer operands*. The number of tuple initializer operands is the *arity* of the constructed tuple.

1-ary tuple expressions require a comma after their tuple initializer operand to be disambiguated with a parenthetical expression.

Tuple expressions without any tuple initializer operands produce the unit tuple.

For other tuple expressions, the first written tuple initializer operand initializes the field `item0` and subsequent operands initializes the next highest field.

For example, in the tuple expression `(true, false, 1)`, `true` initializes the value of the field `item0`, `false` field `item1`, and `1` field `item2`.

Examples of tuple expressions and their types:

| Expression           | Type         |
| -------------------- | ------------ |
| `()`                 | `()` (unit)  |
| `(0, 4)`         | `(u256, u256)` |
| `(true, )` | `(bool, )`  |
| `(true, -1, 1)`| `(bool, i256, u256)` |

A tuple field can be accessed via an [attribute expression].

Example:

```fe
contract Foo {
    pub fn bar() {
        // Creating a tuple via a tuple expression
        let some_tuple: (u256, bool) = (1, false)

        // Accessing the first tuple field via the `item0` field
        baz(input: some_tuple.item0)
    }
    pub fn baz(input: u256) {}
}
```

[_Expression_]: ./index.md
[expression]: ./index.md
[IDENTIFIER]: ../lexical_structure/identifiers.md
[tuple values]: ../type_system/types/tuple.md
[attribute expression]: ./attribute.md
