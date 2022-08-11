# Unary Operators

> **<sup>Syntax</sup>**\
> _UnaryExpression_ :\
> &nbsp;&nbsp;&nbsp;&nbsp; `not` [_Expression_]\
> &nbsp;&nbsp; | `-` [_Expression_]\
> &nbsp;&nbsp; | `~` [_Expression_]\

The unary operators are used to negate expressions. The unary `-` (minus) operator yields the negation of its numeric argument. The unary `~` (invert) operator yields the *bitwise* inversion of its integer argument. The unary `not` operator yields the inversion of its boolean argument.

Example:

```fe
fn f() {
  let x: bool = not true  // false
  let y: i256 = -1
  let z: i256 = i256(~1)  // -2
}
```

[_Expression_]: ../expressions/index.md
