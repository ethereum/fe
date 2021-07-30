# Contracts

> **<sup>Syntax</sup>**\
> _Contract_ :\
> &nbsp;&nbsp; `contract` [IDENTIFIER] `:` [NEWLINE]\
> &nbsp;&nbsp; [INDENT]\
> &nbsp;&nbsp; _ContractMember_<sup>\*</sup>\
> &nbsp;&nbsp; [DEDENT]\
>
> _ContractMember_:\
> &nbsp;&nbsp; [_Visibility_]<sup>?</sup>\
> &nbsp;&nbsp; (\
> &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;&nbsp;  _ContractField_\
> &nbsp;&nbsp; &nbsp;&nbsp; | [_Function_]\
> &nbsp;&nbsp; &nbsp;&nbsp; | [_Struct_]\
> &nbsp;&nbsp; &nbsp;&nbsp; | [_Event_]\
> &nbsp;&nbsp; &nbsp;&nbsp; | [_Enumeration_]\
> &nbsp;&nbsp; )
>
> _Visibility_ :\
> &nbsp;&nbsp; `pub`<sup>?</sup>
>
> _ContractField_ :\
> &nbsp;&nbsp; [IDENTIFIER] `:` [_Type_]


A _contract_ in Fe is a collection of code that resides at a specific address on the Ethereum blockchain. It is defined with the keyword `contract`.

An example of a `contract`:

```
contract GuestBook:
    messages: Map<address, String<100>>

    event Signed:
        book_msg: String<100>

    pub def sign(book_msg: String<100>):
        self.messages[msg.sender] = book_msg
        emit Signed(book_msg=book_msg)

    pub def get_msg(addr: address) -> String<100>:
        return self.messages[addr].to_mem()
```

[NEWLINE]: tokens.md#newline
[INDENT]: tokens.md#indent
[DEDENT]: tokens.md#dedent
[IDENTIFIER]: identifiers.md
[_Visibility_]: visibility_and_privacy.md
[_Type_]: types.md
[type]: types.md
[_Function_]: function_item_types.md
[_Struct_]: structs.md
[_Event_]: events.md
[_Enumeration_]: enumeration.md