# `for` statement


> **<sup>Syntax</sup>**\
> _ForStatement_ :\
> &nbsp;&nbsp; `for` [IDENTIFIER] `in` [_Expression_] `:` [NEWLINE]\
> &nbsp;&nbsp; [INDENT]\
> &nbsp;&nbsp; ([_Statement_] | [_Expression_])<sup>+</sup>\
> &nbsp;&nbsp; [DEDENT]\

A `for` statement is a syntactic construct for looping over elements provided by an [array type].

An example of a `for` loop over the contents of an array:

Example:

```python
contract Foo:

    pub fn bar(values: Array<u256, 10>) -> u256:
        let sum: u256
        for i in values:
            sum = sum + i

        return sum
```

[NEWLINE]: ../lexical_structure/tokens.md#newline
[INDENT]: ../lexical_structure/tokens.md#indent
[DEDENT]: ../lexical_structure/tokens.md#dedent
[IDENTIFIER]: ../lexical_structure/identifiers.md
[_Expression_]: ../expressions/index.md
[array type]: ../type_system/types/array.md
[_Statement_]: ./index.md
