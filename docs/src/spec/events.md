# Events

> **<sup>Syntax</sup>**\
> _Event_ :\
> &nbsp;&nbsp; `event` [IDENTIFIER] `:` [NEWLINE]\
> &nbsp;&nbsp; [INDENT]\
> &nbsp;&nbsp; _EventField_<sup>\*</sup>\
> &nbsp;&nbsp; [DEDENT]\
>
> _EventField_ :\
> &nbsp;&nbsp; _EventIndexability_ [IDENTIFIER] `:` [_Type_]\
>
> _EventIndexability_ :\
> &nbsp;&nbsp; `idx`<sup>?</sup>

An _event_ is a nominal [event type] defined with the keyword `event`. It is emitted with the keyword `emit`.

An example of a `event` item and its use:

```python
contract Foo:
    event Transfer:
        idx sender: address
        idx receiver: address
        value: u256

    fn transfer(to : address, value : u256):
        # Heavy logic here
        # All done, log the event for listeners
        emit Transfer(sender=msg.sender, receiver=to, value)
```

[NEWLINE]: tokens.md#newline
[INDENT]: tokens.md#indent
[DEDENT]: tokens.md#dedent
[IDENTIFIER]: identifiers.md
[_Type_]: types.md
[event type]: event_types.md
