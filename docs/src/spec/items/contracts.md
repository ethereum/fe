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


A _contract_ is a piece of executable code stored at an address on the blockchain. See *Appendix A.* in the [Yellow Paper](https://ethereum.github.io/yellowpaper/paper.pdf) for more info. Contracts can be written in high level languages, like Fe, and then compiled to EVM bytecode for deployment to the blockchain. 

Once the code is deployed to the blockchain, the contract's functions can be invoked by sending a transaction to the contract address (or a `call`, for functions that do not modify blockchain data). 
 
In Fe, contracts are defined in files with `.fe` extensions and compiled using `fe build`.

A contract is denoted using the `contract` keyword. A contract definition adds a new contract type to the module. This [contract type] may be used for calling existing contracts with the same interface or initializing new contracts with the create methods.

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

Multiple contracts can be compiled from a single `.fe` contract file.

## pragma

An optional `pragma` statement can be placed at the beginning of a contract. They are used to enable developers to express that certain code is meant to be compiled with a specific compiler version such that non-matching compiler versions will reject it.

Read more on [pragma](../statements/pragma.md)

## State variables

State variables are permanently stored in the contract storage on the blockchain. State variables must be declared inside the contract body but outside the scope of any individual contract function. 

```fe
pub contract Example {
    some_number: u256
    _some_string: String<100>
}
```

### Contract functions

Functions are executable blocks of code. Contract functions are defined inside the body of a contract, but functions defined at module scope (outside of any contract) can be called from within a contract as well. 

Individual functions can be called internally or externally depending upon their [visibility](../items/visibility_and_privacy.md) (either `private` or `public`).

Functions can modify either (or neither) the contract instance or the blockchain. This can be inferred from the function signature by the presence of combinations of `mut`, `self` and `Context`. If a function modifies the contract instance it requires `mut self` as its first argument. If a function modifies the blockchain it requires `Context` as an argument.

Read more on [functions](../../spec/items/functions/index.md).

### The `__init__()` function

The `__init__` function is a special contract function that can only be called at *contract deployment time*. It is mostly used to set initial values to state variables upon deployment. In other contexts, `__init__()` is commonly referred to as the `constructor` function.

```fe
pub contract Example {

    _some_number: u256
    _some_string: String<100>

    pub fn __init__(mut self, number: u256, string: String<100>)  {
        self._some_number=number;
        self._some_string=string;
    }
}
```

It is **not possible** to call `__init__` at runtime.


## Structs

Structs might also exist inside a contract file. These are declared outside of the contract body and are used to define a group of variables that can be used for some specific purpose inside the contract. In Fe structs are also used to represent an `Event` or an `Error`.

Read more on [structs](../../spec/items/structs.md).


[NEWLINE]: ../lexical_structure/tokens.md#newline
[IDENTIFIER]: ../lexical_structure/identifiers.md
[_Visibility_]: ./visibility_and_privacy.md
[_Type_]: ../type_system/types/index.md
[contract type]: ../type_system/types/contract.md
[_Function_]: ../type_system/types/function.md
[_Struct_]: ./structs.md
[_Enum_]: ./enums.md
