# Path expressions

> **<sup>Syntax</sup>**\
> _PathExpression_ :\
> &nbsp;&nbsp; [IDENTIFIER] ( `::` [IDENTIFIER] )<sup>\*</sup>

A *name expression* resolves to a local variable.

Example:

```fe,ignore
contract Foo {
    pub fn baz() {
        // CONST_VALUE is defined in another module `my_mod`.
        let foo: u32 = my_mod::CONST_VALUE
    }
}
```

[IDENTIFIER]: ../lexical_structure/identifiers.md
