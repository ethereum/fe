# Contracts

> **<sup>Syntax</sup>**\
> _Contract_ :\
> &nbsp;&nbsp; `contract` [IDENTIFIER] `{`\
> &nbsp;&nbsp;_ContractMember_<sup>\*</sup>\
> &nbsp;&nbsp;_`}`
>
> _ContractMember_:\
> &nbsp;&nbsp; [_Visibility_]<sup>?</sup>\
> &nbsp;&nbsp; (\
> &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;&nbsp;  _ContractField_\
> &nbsp;&nbsp; &nbsp;&nbsp; | [_Function_]\
> &nbsp;&nbsp; &nbsp;&nbsp; | [_Struct_]\
> &nbsp;&nbsp; &nbsp;&nbsp; | [_Enum_]\
> &nbsp;&nbsp; )
>
> _Visibility_ :\
> &nbsp;&nbsp; `pub`<sup>?</sup>
>
> _ContractField_ :\
> &nbsp;&nbsp; [IDENTIFIER] `:` [_Type_]


 A _contract_ is a piece of EVM Code associated with an Account. See *Appendix A.* in the [Yellow Paper](https://ethereum.github.io/yellowpaper/paper.pdf) for more info. In Fe, a contract is denoted using the `contract` keyword. A contract definition adds a new contract type to the module. This [contract type] may be used for calling existing contracts with the same interface or initializing new contracts with the create methods.

An example of a `contract`:

```fe
struct Signed {
    pub book_msg: String<100>
}

contract GuestBook {
    messages: Map<address, String<100>>

    pub fn sign(mut self, mut ctx: Context, book_msg: String<100>) {
        self.messages[ctx.msg_sender()] = book_msg
        ctx.emit(Signed(book_msg: book_msg))
    }

    pub fn get_msg(self, addr: address) -> String<100> {
        return self.messages[addr].to_mem()
    }
}
```

## The `__init__` function

The `__init__` function is a special contract function that can only be called at *contract deployment time*. It is mostly used to set initial values to storage variables upon deployment.

```fe
pub contract Example {

    admin: address

    pub fn __init__(mut self, admin: address)  {
        self.admin = admin
    }
}
```

It is **not possible** to call `__init__` at runtime.

[NEWLINE]: ../lexical_structure/tokens.md#newline
[IDENTIFIER]: ../lexical_structure/identifiers.md
[_Visibility_]: ./visibility_and_privacy.md
[_Type_]: ../type_system/types/index.md
[contract type]: ../type_system/types/contract.md
[_Function_]: ../type_system/types/function.md
[_Struct_]: ./structs.md
[_Enum_]: ./enums.md
