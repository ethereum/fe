# Enumeration

> **<sup>Syntax</sup>**\
> _Enumeration_ :\
> &nbsp;&nbsp; `enum`
>   [IDENTIFIER]&nbsp;
> &nbsp;&nbsp; [_EndOfHeader_]\
> &nbsp;&nbsp; _EnumField_<sup>\*</sup>
>
> _EnumField_ :\
> &nbsp;&nbsp; [IDENTIFIER]`,`

An *enumeration*, also referred to as *enum* is a simultaneous definition of a
nominal [enumerated type], that can be used to create or pattern-match values of the corresponding enumerated type.

Enumerations are declared with the keyword `enum`.

An example of an `enum` item and its use:

```
enum Animal:
    Dog,
    Cat,

barker = Animal.Dog
```

[_EndOfHeader_]: end_of_header.md
[IDENTIFIER]: identifiers.md
[_Type_]: types.md
[enumerated type]:enumerated_types.md