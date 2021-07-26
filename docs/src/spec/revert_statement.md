
### 4.1.2 `revert` statement


> **<sup>Syntax</sup>**\
> _RevertStatement_ :\
> &nbsp;&nbsp; `revert`<sup>?</sup>

The revert statement is denoted with the keyword `revert`. Evaluating a `revert`
statement will cause to revert all state changes made by the call and return with an revert error to the caller.

An example of a `revert` statement:

```
def transfer(to : address, value : u256):
    if not self.in_whitelist(to):
        revert
    # more logic here
```