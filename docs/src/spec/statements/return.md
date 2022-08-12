# `return` statement


> **<sup>Syntax</sup>**\
> _ReturnStatement_ :\
> &nbsp;&nbsp; `return` [_Expression_]<sup>?</sup>

The return statement is denoted with the keyword `return`. A `return` statement leaves the current function call with a return value which is either the value of the evaluated expression (if present) or `()` (unit type) if `return` is not followed by an expression explicitly.


An example of a `return` statement without explicit use of an expression:

```fe
contract Foo {
    fn transfer(self, to: address, value: u256) {
        if not self.in_whitelist(to) {
            return
        }
    }

    fn in_whitelist(self, to: address) -> bool {
        // revert used as placeholder for actual logic
        revert
    }
}
```

The above can also be written in a slightly more verbose form:

```fe
contract Foo {
    fn transfer(self, to: address, value: u256) -> () {
        if not self.in_whitelist(to) {
            return ()
        }
    }

    fn in_whitelist(self, to: address) -> bool {
        // revert used as placeholder for actual logic
        revert
    }
}
```

[_Expression_]: ../expressions/index.md
[struct]: ../items/structs.md
[EIP-838]: https://github.com/ethereum/EIPs/issues/838
