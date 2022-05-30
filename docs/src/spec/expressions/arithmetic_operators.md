# Arithmetic Operators

> **<sup>Syntax</sup>**\
> _ArithmeticExpression_ :\
> &nbsp;&nbsp;&nbsp;&nbsp; [_Expression_] `+` [_Expression_]\
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

| Symbol | Integer                 |
|--------|-------------------------|
| `+`    | Addition                |
| `-`    | Subtraction             |
| `*`    | Multiplication          |
| `/`    | Division*               |
| `%`    | Remainder               |
| `**`   | Exponentiation          |
| `&`    | Bitwise AND             |
| <code>&#124;</code> | Bitwise OR |
| `^`    | Bitwise XOR             |
| `<<`   | Left Shift              |
| `>>`   | Right Shift             |

\* Integer division rounds towards zero.


Here are examples of these operators being used.

```fe,ignore
3 + 6 == 9
6 - 3 == 3
2 * 3 == 6
6 / 3 == 2
5 % 4 == 1
2 ** 4 == 16
12 & 25 == 8
12 | 25 == 29
12 ^ 25 == 21
212 << 1 == 424
212 >> 1 == 106
```


[_Expression_]: ./index.md
