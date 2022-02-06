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

[NEWLINE]: ../lexical_structure/tokens.md#newline
[INDENT]: ../lexical_structure/tokens.md#indent
[DEDENT]: ../lexical_structure/tokens.md#dedent
[IDENTIFIER]: ../lexical_structure/identifiers.md
[_Type_]: ../type_system/types/index.md
[event type]: ../type_system/types/event.md
