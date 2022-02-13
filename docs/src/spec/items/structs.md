# Structs

> **<sup>Syntax</sup>**\
> _Struct_ :\
> &nbsp;&nbsp; `struct` [IDENTIFIER] `:` [NEWLINE]\
> &nbsp;&nbsp; [INDENT]\
> &nbsp;&nbsp; _StructField_<sup>*</sup>\
> &nbsp;&nbsp; [DEDENT]\
>
> _StructField_ :\
> &nbsp;&nbsp; [IDENTIFIER] `:` [_Type_]


A _struct_ is a nominal [struct type][struct type] defined with the keyword `struct`.

An example of a `struct` item and its use:

```
struct Point:
    x: u256
    y: u256

p = Point(x: 10, y: 11)
px: u256 = p.x;
```


Builtin functions:

- `abi_encode()` encodes the struct as an ABI tuple and returns the encoded data as a fixed-size byte array that is equal in size to the encoding.


[NEWLINE]: ../lexical_structure/tokens.md#newline
[INDENT]: ../lexical_structure/tokens.md#indent
[DEDENT]: ../lexical_structure/tokens.md#dedent
[IDENTIFIER]: ../lexical_structure/identifiers.md
[struct type]: ../type_system/types/struct.md
[_Type_]: ../type_system/types/index.md
