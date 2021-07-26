### Contracts

> **<sup>Syntax</sup>**\
> _Contract_ :\
> &nbsp;&nbsp; `contract`
>   [IDENTIFIER]&nbsp;
> &nbsp;&nbsp; [_EndOfHeader_]\
> &nbsp;&nbsp; _ContractMember_<sup>\*</sup>
>
> _ContractMember_:\
> &nbsp;&nbsp; [_Visibility_]<sup>?</sup>\
> &nbsp;&nbsp; (\
> &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;&nbsp;  [_ContractField_]\
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
    pub guest_book: Map<address, bytes[100]>

    event Signed:
        idx book_msg: bytes[100]

    pub def sign(book_msg: bytes[100]):
        self.guest_book[msg.sender] = book_msg

        emit Signed(book_msg=book_msg)

    pub def get_msg(addr: address) -> bytes[100]:
        return self.guest_book[addr]
```

