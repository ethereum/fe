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

A *call expression* calls a function. The syntax of a call expression is an [expression], followed by a parenthesized comma-separated list of call arguments. Call arguments are expressions, and must be labeled and provided in the order specified in the [function definition]. If the function eventually returns, then the expression completes.


Example:

```fe
contract Foo {

    pub fn demo(self) {
        let ann: address = 0xaa
        let bob: address = 0xbb
        self.transfer(from: ann, to: bob, 25)
    }

    pub fn transfer(self, from: address, to: address, _ val: u256) {}
}
```

[_Expression_]: ./index.md
[expression]: ./index.md
[IDENTIFIER]: ../lexical_structure/identifiers.md
[INTEGER_LITERAL]: ../lexical_structure/tokens.md#integer-literals
[function definition]: ../items/functions.md
