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

```python
contract Foo:

    pub fn add(a: u256, b: u256) -> u256:
        a += b
        return a

    pub fn sub(a: u256, b: u256) -> u256:
        a -= b
        return a

    pub fn mul(a: u256, b: u256) -> u256:
        a *= b
        return a

    pub fn div(a: u256, b: u256) -> u256:
        a /= b
        return a

    pub fn mod(a: u256, b: u256) -> u256:
        a %= b
        return a

    pub fn pow(a: u256, b: u256) -> u256:
        a **= b
        return a

    pub fn lshift(a: u8, b: u8) -> u8:
        a <<= b
        return a

    pub fn rshift(a: u8, b: u8) -> u8:
        a >>= b
        return a

    pub fn bit_or(a: u8, b: u8) -> u8:
        a |= b
        return a

    pub fn bit_xor(a: u8, b: u8) -> u8:
        a ^= b
        return a

    pub fn bit_and(a: u8, b: u8) -> u8:
        a &= b
        return a
```

[_Expression_]: expressions.md
