# `emit` statement


> **<sup>Syntax</sup>**\
> _EmitStatement_ :\
> &nbsp;&nbsp; `emit` _EventLiteral_ `(` _CallParams_<sup>?</sup> `)`
>
> _EventLiteral_ :\
> &nbsp;&nbsp; [IDENTIFIER]<sup>Name of event defined in current module</sup>
>
> _CallParams_ :\
> &nbsp;&nbsp; _CallArg_&nbsp;( `,` _CallArg_ )<sup>\*</sup> `,`<sup>?</sup>
>
> _CallArg_ :\
> &nbsp;&nbsp; (_CallArgLabel_ `=`)<sup>?</sup> [_Expression_]
>
> _CallArgLabel_ :\
> &nbsp;&nbsp; [IDENTIFIER]<sup>Label must correspond to the name of the event property at the given position. It can be omitted if parameter name and event property name are equal.</sup>

The `emit` statement is used to create [log entries] in the blockchain. The `emit` keyword is followed by a literal that corresponds to a defined event, followed by a parenthesized comma-separated list of expressions.


Examples:

```python
contract Foo:
    event Mix:
        num1: u256
        idx addr: address
        num2: u256
        my_bytes: Array<u8, 100>

    pub fn emit_mix(addr: address, my_bytes: Array<u8, 100>):
        emit Mix(num1=26, addr, num2=42, my_bytes)
```

[_Expression_]: expressions.md
[IDENTIFIER]: identifiers.md
[struct]: structs.md
[EIP-838]: https://github.com/ethereum/EIPs/issues/838
[log entries]: https://ethereum.stackexchange.com/questions/12950/what-are-solidity-events-and-how-they-are-related-to-topics-and-logs/12951#12951