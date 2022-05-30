# `const` statement


> **<sup>Syntax</sup>**\
> _ConstStatement_ :\
> &nbsp;&nbsp; `const` [IDENTIFIER]`:` [_Type_] `=` [_Expression_]\
>

A `const` statement introduces a named constant value. Constants are either directly inlined wherever they are used or loaded from the contract code depending on their type.


Example:

```fe
const TEN: u256 = 10
const HUNDO: u256 = TEN * TEN

contract Foo {
  pub fn bar() -> u256 {
    return HUNDO
  }
}
```


[IDENTIFIER]: ../lexical_structure/identifiers.md
[_Expression_]: ../expressions/index.md
[_Type_]: ../type_system/types/index.md
