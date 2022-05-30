# `let` statement


> **<sup>Syntax</sup>**\
> _LetStatement_ :\
> &nbsp;&nbsp; `let` [IDENTIFIER] | _TupleTarget_ `:` [_Type_] `=` [_Expression_]\
>
> _TupleTarget_ :\
> &nbsp;&nbsp; `(` _TupleTargetItem_ (`,` _TupleTargetItem_) <sup>+</sup> `)`\
>
> _TupleTargetItem_ :\
> &nbsp;&nbsp; [IDENTIFIER] | _TupleTarget_\

A `let` statement introduces a new set of variables. Any variables introduced by a variable declaration are visible from the point of declaration until the end of the enclosing block scope.

> Note: Support for nested tuples isn't yet implemented but can be tracked via this [GitHub issue](https://github.com/ethereum/fe/issues/427).


Example:

```fe
contract Foo {

  pub fn bar() {
    let val1: u256 = 1
    let (val2):(u256) = (1,)
    let (val3, val4):(u256, bool) = (1, false)
    let (val5, val6, (val7, val8)):(u256, bool, (u256, u256)) = (1, false, (2, 4))
  }
}
```


[IDENTIFIER]: ../lexical_structure/identifiers.md
[_Expression_]: ../expressions/index.md
[_Type_]: ../type_system/types/index.md
