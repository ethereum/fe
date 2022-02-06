# Tuple Types

> **<sup>Syntax</sup>**\
> _TupleType_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; `(` `)`\
> &nbsp;&nbsp; | `(` ( [_Type_] `,` )<sup>+</sup> [_Type_]<sup>?</sup> `)`

*Tuple types* are a family of structural types[^1] for heterogeneous lists of other types.

The syntax for a tuple type is a parenthesized, comma-separated list of types.

A tuple type has a number of fields equal to the length of the list of types.
This number of fields determines the *arity* of the tuple.
A tuple with `n` fields is called an *n-ary tuple*.
For example, a tuple with 2 fields is a 2-ary tuple.

Fields of tuples are named using increasing numeric names matching their position in the list of types.
The first field is `item0`.
The second field is `item1`.
And so on.
The type of each field is the type of the same position in the tuple's list of types.

For convenience and historical reasons, the tuple type with no fields (`()`) is often called *unit* or *the unit type*.
Its one value is also called *unit* or *the unit value*.

Some examples of tuple types:

* `()` (also known as the *unit* or *zero-sized type*)
* `(u8, u8)`
* `(bool, i32)`
* `(i32, bool)` (different type from the previous example)

Values of this type are constructed using a [tuple expression].
Furthermore, various expressions will produce the unit value if there is no other meaningful value for it to evaluate to.
Tuple fields can be accessed via an [attribute expression].

[^1]: Structural types are always equivalent if their internal types are equivalent.


[_Type_]: ./index.md
[tuple expression]: ../../expressions/tuple.md
[attribute expression]: ../../expressions/attribute.md
