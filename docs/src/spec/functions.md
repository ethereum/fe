# Functions

> **<sup>Syntax</sup>**\
> _Function_ :\
> &nbsp;&nbsp; _FunctionQualifiers_ `def` [IDENTIFIER]\
> &nbsp;&nbsp; &nbsp;&nbsp; `(` _FunctionParameters_<sup>?</sup> `)`\
> &nbsp;&nbsp; &nbsp;&nbsp; _FunctionReturnType_<sup>?</sup>\
> &nbsp;&nbsp; &nbsp;&nbsp; `:` [NEWLINE]\
> &nbsp;&nbsp; &nbsp;&nbsp; [INDENT]\
> &nbsp;&nbsp; &nbsp;&nbsp; [_BlockExpression_]\
> &nbsp;&nbsp; &nbsp;&nbsp; [DEDENT]\
>
> _FunctionQualifiers_ :\
> &nbsp;&nbsp; `pub`<sup>?</sup>
>
>
>
> _FunctionParameters_ :\
> &nbsp;&nbsp; _FunctionParam_ (`,` _FunctionParam_)<sup>\*</sup> `,`<sup>?</sup>
>
> _FunctionParam_ :\
> &nbsp;&nbsp; [IDENTIFIER] `:` [_Type_]
>
> _FunctionReturnType_ :\
> &nbsp;&nbsp; `->` [_Type_]


A _function_ consists of a [block], along with a name and a set of parameters.
Other than a name, all these are optional. Functions are declared with the
keyword `def`. Functions may declare a set of *input* [*variables*][variables]
as parameters, through which the caller passes arguments into the function, and
the *output* [*type*][type] of the value the function will return to its caller
on completion.

When referred to, a _function_ yields a first-class *value* of the
corresponding zero-sized [*function item type*], which
when called evaluates to a direct call to the function.

A function header ends with a colon (`:`) after which the function body begins.

For example, this is a simple function:

```python
def answer_to_life_the_universe_and_everything() -> u256:
    return 42
```

[NEWLINE]: tokens.md#newline
[INDENT]: tokens.md#indent
[DEDENT]: tokens.md#dedent
[_BlockExpression_]: block_expression.md
[block]: block_expression.md
[IDENTIFIER]: identifiers.md
[_Type_]: types.md
[type]: types.md
[_function_]: function_item_types.md
[*function item type*]: function_item_types.md
[variables]: variables.md
