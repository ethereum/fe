# `match` statement


> **<sup>Syntax</sup>**\
> _MatchStatement_ :\
> &nbsp;&nbsp; `match` [_Expression_] `{`\
> &nbsp;&nbsp; &nbsp;&nbsp; ( _Pattern_ `=>` `{` [_Statement_]<sup>\*</sup> `}` )<sup>\+</sup>\
> &nbsp;&nbsp; `}`\
> 
> _Pattern_ : \
> &nbsp;&nbsp; _PatternElem_ ( `|` _PatternElem_ )<sup>*</sup>\
>
> _PatternElem_ : \
> &nbsp;&nbsp; [_IDENTIFIER_] | [_BOOLEAN_LITERAL_] | `_` |  `..` | [_Path_] \|\
> &nbsp;&nbsp; [_Path_]`(` _TuplePatterns_<sup>?</sup> `)` |`(` _TuplePatterns_<sup>?</sup> `)` \|\
> &nbsp;&nbsp; [_Path_]`{` _StructPatterns_<sup>?</sup> `}`\
> 
> _TuplePatterns_ : \
> &nbsp;&nbsp; _Pattern_ ( `,` _Pattern_ )<sup>\*</sup>\
>
> _StructPatterns_ : \
> &nbsp;&nbsp; _Field_ ( `,` _Field_)<sup>\*</sup>(`,` `..`)<sup>?</sup>\
>
> _Field_ : \
> &nbsp;&nbsp; [_IDENTIFIER_] `:` _Pattern_\


A `match` statements compares `expression` with patterns, then executes body of the matched arm.

Example:

```fe
enum MyEnum {
    Unit
    Tuple(u32, u256, bool)
}

contract Foo {
    pub fn bar(self) -> u256 {
        let val: MyEnum = MyEnum::Tuple(1, 10, false)
        return self.eval(val)
    }
    
    fn eval(self, val: MyEnum) -> u256 {
        match val {
            MyEnum::Unit => {
                return 0
            }
            
            MyEnum::Tuple(.., false) => {
                return 1
            }
            
            MyEnum::Tuple(a, b, true) => {
                return u256(a) + b
            }
        }
    }
}
```

[NEWLINE]: ../lexical_structure/tokens.md#newline
[_IDENTIFIER_]: ../lexical_structure/identifiers.md
[_Expression_]: ../expressions/index.md
[_Statement_]: ./index.md
[_Path_]: ../expressions/path.md
[_BOOLEAN_LITERAL_]: ../lexical_structure/tokens.md#boolean-literals
[struct]: ../items/structs.md
