# Fe Language Specification

<div class="warning">
  Warning: This is a work in progress document. It is incomplete and specifications aren't stable yet.
</div>

## Arithmetic Operators

> **<sup>Syntax</sup>**\
> _ArithmeticExpression_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; [_Expression_] `+` [_Expression_]\
> &nbsp;&nbsp; | [_Expression_] `-` [_Expression_]\
> &nbsp;&nbsp; | [_Expression_] `*` [_Expression_]\
> &nbsp;&nbsp; | [_Expression_] `/` [_Expression_]\
> &nbsp;&nbsp; | [_Expression_] `%` [_Expression_]\
> &nbsp;&nbsp; | [_Expression_] `**` [_Expression_]\
> &nbsp;&nbsp; | [_Expression_] `&` [_Expression_]\
> &nbsp;&nbsp; | [_Expression_] `|` [_Expression_]\
> &nbsp;&nbsp; | [_Expression_] `^` [_Expression_]\
> &nbsp;&nbsp; | [_Expression_] `<<` [_Expression_]\
> &nbsp;&nbsp; | [_Expression_] `>>` [_Expression_]

Binary operators expressions are all written with [infix notation](https://en.wikipedia.org/wiki/Infix_notation).
This table summarizes the behavior of arithmetic and logical binary operators on
primitive types.

| Symbol | Integer                 | Status      | Discussions    |
|--------|-------------------------|-------------|----------------|
| `+`    | Addition                | IMPLEMENTED |                |
| `-`    | Subtraction             | IMPLEMENTED |                |
| `*`    | Multiplication          | IMPLEMENTED |                |
| `/`    | Division*               | IMPLEMENTED |                |
| `%`    | Remainder               | IMPLEMENTED |                |
| `**`   | Exponentiation          | IMPLEMENTED |                |
| `&`    | Bitwise AND             | IMPLEMENTED |                |
| <code>&#124;</code> | Bitwise OR | IMPLEMENTED |                |
| `^`    | Bitwise XOR             | IMPLEMENTED |                |
| `<<`   | Left Shift              | IMPLEMENTED |                |
| `>>`   | Right Shift             | IMPLEMENTED |                |

\* Integer division rounds towards zero.


Here are examples of these operators being used.

```
3 + 6 == 9
6 - 3 == 3
2 * 3 == 6
6 / 3 == 2 TODO: Rest
5 % 4 == 1
2 ** 4 == 16
12 & 25 == 8
12 | 25 == 29
12 ^ 25 == 21
212 << 1 == 424
212 >> 1 == 106
```