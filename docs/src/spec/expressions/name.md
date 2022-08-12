# Name expressions

> **<sup>Syntax</sup>**\
> _NameExpression_ :\
> &nbsp;&nbsp; [IDENTIFIER]

A *name expression* resolves to a local variable.

Example:

```fe
contract Foo {
    pub fn baz(foo: u256) {
        // name expression resolving to the value of `foo`
        foo
    }
}
```

[IDENTIFIER]: ../lexical_structure/identifiers.md
