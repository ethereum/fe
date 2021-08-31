# Call expressions


> **<sup>Syntax</sup>**\
> _CallExpression_ :\
> &nbsp;&nbsp; [_Expression_] _GenericArgs_<sup>?</sup> `(` _CallParams_<sup>?</sup> `)`
>
> _GenericArgs_ :\
> &nbsp;&nbsp; `<` [IDENTIFIER] | [INTEGER_LITERAL]  (`,` [IDENTIFIER] | [INTEGER_LITERAL])<sup>*</sup> `>`
>
> _CallParams_ :\
> &nbsp;&nbsp; _CallArg_&nbsp;( `,` _CallArg_ )<sup>\*</sup> `,`<sup>?</sup>
>
> _CallArg_ :\
> &nbsp;&nbsp; (_CallArgLabel_ `=`)<sup>?</sup> [_Expression_]
>
> _CallArgLabel_ :\
> &nbsp;&nbsp; [IDENTIFIER]<sup>Label must correspond to the name of the called function argument at the given position. It can be omitted if parameter name and the name of the called function argument are equal.</sup>

A *call expression* calls a function. The syntax of a call expression is an [expression], followed by a parenthesized comma-separated list of call arguments. Call arguments are expressions which optionally may be labeled in which case the label must correspond to name of the function argument at the given position. Labels do **not allow** to change the order in which parameters can be applied to a function. If the function eventually returns, then the expression completes.


Example:

```python
contract Foo:

    pub fn baz():
        bar(100, val2=300)

    pub fn bar(val1: u256, val2: u256):
        pass
```

[_Expression_]: expressions.md
[expression]: expressions.md
[IDENTIFIER]: identifiers.md
[INTEGER_LITERAL]: tokens.md#integer-literals