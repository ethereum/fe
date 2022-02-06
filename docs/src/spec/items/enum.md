# Enum

> **<sup>Syntax</sup>**\
> _Enumeration_ :\
> &nbsp;&nbsp; `enum` [IDENTIFIER] `:` [NEWLINE]\
> &nbsp;&nbsp; [INDENT]\
> &nbsp;&nbsp; _EnumField_<sup>\*</sup>\
> &nbsp;&nbsp; [DEDENT]\
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

[NEWLINE]: ../lexical_structure/tokens.md#newline
[INDENT]: ../lexical_structure/tokens.md#indent
[DEDENT]: ../lexical_structure/tokens.md#dedent
[IDENTIFIER]: ../lexical_structure/identifiers.md
[_Type_]: ../type_system/types/index.md
[enumerated type]: ../type_system/types/enum.md