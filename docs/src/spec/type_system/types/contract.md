# Contract types

An *contract type* is the type denoted by the name of an [`contract` item].

A value of a given contract type carries the contract's public interface as
attribute functions. A new contract value can be created by either casting
an address to a contract type or by creating a new contract using the type
attribute functions `create` or `create2`.

Example:

```fe
contract Foo {
    pub fn get_my_num() -> u256 {
        return 42
    }
}

contract FooFactory {
    pub fn create2_foo(mut ctx: Context) -> address {
        // `0` is the value being sent and `52` is the address salt
        let foo: Foo = Foo.create2(ctx, 0, 52)
        return address(foo)
    }
}
```

[`contract` item]: ../../items/contracts.md
