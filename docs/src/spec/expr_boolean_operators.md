# Boolean Operators

> **<sup>Syntax</sup>**\
> _BooleanExpression_ :\
> &nbsp;&nbsp;&nbsp;&nbsp; [_Expression_] `or` [_Expression_]\
> &nbsp;&nbsp; | [_Expression_] `and` [_Expression_]\

The operators `or` and `and` may be applied to operands of boolean type. The `or` operator denotes logical 'or', and the `and` operator denotes logical 'and'.

<div class="warning">

Warning:
These operators do currently not evaluate lazily which is likely to change in the future. ([See GitHub issue](https://github.com/ethereum/fe/issues/488))

</div>

Example:

```
let x: bool = false or true # true
```

[_Expression_]:expressions.md