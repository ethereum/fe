# `if` statement


> **<sup>Syntax</sup>**\
> _IfStatement_ :\
> &nbsp;&nbsp; `if` [_Expression_]`{`
> &nbsp;&nbsp; ([_Statement_] | [_Expression_])<sup>+</sup>\
> &nbsp;&nbsp; `}`\
> &nbsp;&nbsp; (`else` `{`\
> &nbsp;&nbsp; ([_Statement_] | [_Expression_])<sup>+</sup>\
> &nbsp;&nbsp; `}`)<sup>?</sup>\


Example:

```fe
contract Foo {
    pub fn bar(val: u256) -> u256 {
        if val > 5 {
            return 1
        } else {
            return 2
        }
    }
}
```

The `if` statement is used for conditional execution.


[NEWLINE]: ../lexical_structure/tokens.md#newline
[_Expression_]: ../expressions/index.md
[_Statement_]: ./index.md
[struct]: ../items/structs.md
