




### Events

> **<sup>Syntax</sup>**\
> _Event_ :\
> &nbsp;&nbsp; `event`
>   [IDENTIFIER]&nbsp;
> &nbsp;&nbsp; [_EndOfHeader_]\
> &nbsp;&nbsp; _EventField_<sup>\*</sup>
>
> _EventField_ :\
> &nbsp;&nbsp; _EventIndexability_ [IDENTIFIER] `:` [_Type_]

> _EventIndexability_ :\
> &nbsp;&nbsp; `idx`<sup>?</sup>

An _event_ is a nominal [event type] defined with the keyword `event`. It is emitted with the keyword `emit`.

An example of a `event` item and its use:

```
event Transfer:
    idx sender: address
    idx receiver: address
    value: u256

def transfer(to : address, value : u256):
   # Heavy logic here
   # All done, log the event for listeners
   emit Transfer(msg.sender, _to, _value)
```
