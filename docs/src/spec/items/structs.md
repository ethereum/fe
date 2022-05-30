# Structs

> **<sup>Syntax</sup>**\
> _Struct_ :\
> &nbsp;&nbsp; `struct` [IDENTIFIER] `{`\
> &nbsp;&nbsp; _StructField_<sup>*</sup>\
> &nbsp;&nbsp; `}`
>
> _StructField_ :\
> &nbsp;&nbsp; `pub`? [IDENTIFIER] `:` [_Type_]


A _struct_ is a nominal [struct type][struct type] defined with the keyword `struct`.

An example of a `struct` item and its use:

```fe
struct Point {
    pub x: u256
    pub y: u256
}

fn pointy_stuff() {
    let p: Point = Point(x: 10, y: 11)
    let px: u256 = p.x
}
```


Builtin functions:

- `abi_encode()` encodes the struct as an ABI tuple and returns the encoded data as a fixed-size byte array that is equal in size to the encoding.


[NEWLINE]: ../lexical_structure/tokens.md#newline
[IDENTIFIER]: ../lexical_structure/identifiers.md
[struct type]: ../type_system/types/struct.md
[_Type_]: ../type_system/types/index.md
