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


 A _contract_ is a piece of EVM Code associated with an Account. See *Appendix A.* in the [Yellow Paper](https://ethereum.github.io/yellowpaper/paper.pdf) for more info. In Fe, a contract is denoted using the `contract` keyword. A contract definition adds a new contract type to the module. This [contract type] may be used for calling existing contracts with the same interface or initializing new contracts with the create methods.

An example of a `contract`:

```python
contract GuestBook:
    messages: Map<address, String<100>>

    event Signed:
        book_msg: String<100>

    pub fn sign(self, book_msg: String<100>):
        self.messages[msg.sender] = book_msg
        emit Signed(book_msg=book_msg)

    pub fn get_msg(self, addr: address) -> String<100>:
        return self.messages[addr].to_mem()
```

[NEWLINE]: tokens.md#newline
[INDENT]: tokens.md#indent
[DEDENT]: tokens.md#dedent
[IDENTIFIER]: identifiers.md
[_Visibility_]: visibility_and_privacy.md
[_Type_]: types.md
[type]: types.md
[contract type]: contract_types.md
[_Function_]: function_item_types.md
[_Struct_]: structs.md
[_Event_]: events.md
[_Enumeration_]: enumeration.md