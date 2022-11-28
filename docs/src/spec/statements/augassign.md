# Augmenting Assignment statement


> **<sup>Syntax</sup>**\
> _AssignmentStatement_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; [_Expression_] `=` [_Expression_]\
> &nbsp;&nbsp; | [_Expression_] `+=` [_Expression_]\
> &nbsp;&nbsp; | [_Expression_] `-=` [_Expression_]\
> &nbsp;&nbsp; | [_Expression_] `%=` [_Expression_]\
> &nbsp;&nbsp; | [_Expression_] `**=` [_Expression_]\
> &nbsp;&nbsp; | [_Expression_] `<<=` [_Expression_]\
> &nbsp;&nbsp; | [_Expression_] `>>=` [_Expression_]\
> &nbsp;&nbsp; | [_Expression_] `|=` [_Expression_]\
> &nbsp;&nbsp; | [_Expression_] `^=` [_Expression_]\
> &nbsp;&nbsp; | [_Expression_] `&=` [_Expression_]\

*Augmenting assignment statements* combine arithmetic and logical binary operators with assignment statements.

An augmenting assignment statement consists of an expression that holds a mutable place, followed by one of the arithmetic or logical binary operators, followed by an equals sign (=) and a value expression.


Example:

```fe
fn example() -> u8 {
    let mut a: u8 = 1
    let b: u8 = 2
    a += b
    a -= b
    a *= b
    a /= b
    a %= b
    a **= b
    a <<= b
    a >>= b
    a |= b
    a ^= b
    a &= b
    return a
}
```

[_Expression_]: ../expressions/index.md
