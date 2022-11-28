# `while` statement


> **<sup>Syntax</sup>**\
> _WhileStatement_ :\
> &nbsp;&nbsp; `while` [_Expression_] `{`\
> &nbsp;&nbsp; ([_Statement_] | [_Expression_])<sup>+</sup>\
> &nbsp;&nbsp; `}`\

A `while` loop begins by evaluation the [boolean] loop conditional expression. If the loop conditional expression evaluates to `true`, the loop body block executes, then control returns to the loop conditional expression. If the loop conditional expression evaluates to `false`, the `while` expression completes.

Example:

```fe
contract Foo {

    pub fn bar() -> u256 {
        let mut sum: u256 = 0
        while sum < 10 {
            sum += 1
        }
        return sum
    }
}
```

[NEWLINE]: ../lexical_structure/tokens.md#newline
[_Expression_]: ../expressions/index.md
[_Statement_]: ./index.md
[boolean]: ../type_system/types/boolean.md
