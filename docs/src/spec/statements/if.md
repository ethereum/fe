# `if` statement


> **<sup>Syntax</sup>**\
> _IfStatement_ :\
> &nbsp;&nbsp; `if` [_Expression_]`:` [NEWLINE]\
> &nbsp;&nbsp; [INDENT]\
> &nbsp;&nbsp; ([_Statement_] | [_Expression_])<sup>+</sup>\
> &nbsp;&nbsp; [DEDENT]\
> &nbsp;&nbsp; (`else` `:` [NEWLINE]\
> &nbsp;&nbsp; [INDENT]\
> &nbsp;&nbsp; ([_Statement_] | [_Expression_])<sup>+</sup>\
> &nbsp;&nbsp; [DEDENT])<sup>?</sup>\


Example:

```python
contract Foo:

    pub fn bar(val: u256) -> u256:
        if val > 5:
            return 1
        else:
            return 2
```

The `if` statement is used for conditional execution.


[NEWLINE]: ../lexical_structure/tokens.md#newline
[INDENT]: ../lexical_structure/tokens.md#indent
[DEDENT]: ../lexical_structure/tokens.md#dedent
[_Expression_]: ../expressions/index.md
[_Statement_]: ./index.md
[struct]: ../items/structs.md
[EIP-838]: https://github.com/ethereum/EIPs/issues/838