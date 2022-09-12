# Enum

> **<sup>Syntax</sup>**\
> _Enumeration_ :\
> &nbsp;&nbsp; `enum` [_IDENTIFIER_] `{`\
> &nbsp;&nbsp; &nbsp;&nbsp; _EnumField_<sup>\*</sup>\
> &nbsp;&nbsp; &nbsp;&nbsp; _EnumMethod_<sup>\*</sup>\
> &nbsp;&nbsp; `}`
>
> _EnumField_ :\
> &nbsp;&nbsp; [_IDENTIFIER_] | [_IDENTIFIER_]`(`_TupleElements_<sup>\?</sup>`)`\
>
> _EnumMethod_ :\
> &nbsp;&nbsp; [_Function_]\
>
> _TupleElements_ :\
> &nbsp;&nbsp; [_Type_] ( `,` [_Type_] )<sup>\*</sup>

An *enum*, also referred to as *enumeration* is a simultaneous definition of a
nominal [Enum type], that can be used to create or pattern-match values of the corresponding type.

Enumerations are declared with the keyword `enum`.

An example of an `enum` item and its use:

```fe,ignore
enum Animal {
    Dog
    Cat
    Bird(BirdType)
    
    pub fn bark(self) -> String<10> {
        match self {
            Animal::Dog => {
                return "bow"
            }

            Animal::Cat => {
                return "meow"
            }
            
            Animal::Bird(BirdType::Duck) => {
                return "quack"
            }
            
            Animal::Bird(BirdType::Owl) => {
                return "hoot"
            }
        }
    }
}

enum BirdType {
    Duck
    Owl
}

fn f() {
    let barker: Animal = Animal::Dog
    barker.bark()
}
```

[NEWLINE]: ../lexical_structure/tokens.md#newline
[_IDENTIFIER_]: ../lexical_structure/identifiers.md
[_Function_]: ./functions.md
[_Type_]: ../type_system/types/index.md
[Enum type]: ../type_system/types/enum.md
