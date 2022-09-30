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
contract Foo {

    pub fn add(mut a: u256, b: u256) -> u256 {
        a += b
        return a
    }

    pub fn sub(mut a: u256, b: u256) -> u256 {
        a -= b
        return a
    }

    pub fn mul(mut a: u256, b: u256) -> u256 {
        a *= b
        return a
    }

    pub fn div(mut a: u256, b: u256) -> u256 {
        a /= b
        return a
    }

    pub fn mod(mut a: u256, b: u256) -> u256 {
        a %= b
        return a
    }

    pub fn pow(mut a: u256, b: u256) -> u256 {
        a **= b
        return a
    }

    pub fn lshift(mut a: u8, b: u8) -> u8 {
        a <<= b
        return a
    }

    pub fn rshift(mut a: u8, b: u8) -> u8 {
        a >>= b
        return a
    }

    pub fn bit_or(mut a: u8, b: u8) -> u8 {
        a |= b
        return a
    }

    pub fn bit_xor(mut a: u8, b: u8) -> u8 {
        a ^= b
        return a
    }

    pub fn bit_and(mut a: u8, b: u8) -> u8 {
        a &= b
        return a
    }
}
```

[_Expression_]: ../expressions/index.md
