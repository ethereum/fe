# `const` statement


> **<sup>Syntax</sup>**\
> _ConstStatement_ :\
> &nbsp;&nbsp; `const` [IDENTIFIER]`:` [_Type_] `=` [_Expression_]\
>

A `const` statement introduces a named constant value. Constants are either directly inlined wherever they are used or loaded from the contract code depending on their type.

> Note: Constants do only support literals at this point


Example:

```python

const TEN: u256 = 10

contract Foo:

  pub fn bar() -> u256:
    return TEN * 5
```


[IDENTIFIER]: ../lexical_structure/identifiers.md
[_Expression_]: ../expressions/index.md
[_Type_]: ../type_system/types/index.md
