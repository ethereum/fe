# Assignment statement


> **<sup>Syntax</sup>**\
> _AssignmentStatement_ :\
> &nbsp;&nbsp; [_Expression_] `=` [_Expression_]\

An assignment statement moves a value into a specified place. An assignment statement consists of an expression that holds a mutable place, followed by an equals sign (=) and a value expression.

Example:

```python
contract Foo:
  some_array: Array<u256, 10>

  pub fn bar(self):
    let val1: u256 = 10
    # Assignment of stack variable
    val1 = 10

    let values: (u256, u256) = (1, 2)
    # Assignment of tuple item
    values.item0 = 3

    # Assignment of storage array slot
    self.some_array[5] = 1000
```

[_Expression_]: expressions.md
