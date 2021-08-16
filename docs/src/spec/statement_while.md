# `while` statement


> **<sup>Syntax</sup>**\
> _WhileStatement_ :\
> &nbsp;&nbsp; `while` [_Expression_] `:` [NEWLINE]\
> &nbsp;&nbsp; [INDENT]\
> &nbsp;&nbsp; ([_Statement_] | [_Expression_])<sup>+</sup>\
> &nbsp;&nbsp; [DEDENT]\

A `while` loop begins by evaluation the [boolean] loop conditional expression. If the loop conditional expression evaluates to `true`, the loop body block executes, then control returns to the loop conditional expression. If the loop conditional expression evaluates to `false`, the `while` expression completes.

Example:

```python
contract Foo:

    pub fn bar() -> u256:
        let sum: u256
        while sum < 10:
            sum += 1
        
        return sum
```

[NEWLINE]: tokens.md#newline
[INDENT]: tokens.md#indent
[DEDENT]: tokens.md#dedent
[_Expression_]: expressions.md
[_Statement_]: statements.md
[boolean]: boolean_type.md
