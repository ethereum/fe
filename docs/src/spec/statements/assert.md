# `assert` statement


> **<sup>Syntax</sup>**\
> _AssertStatement_ :\
> &nbsp;&nbsp; `assert` [_Expression_] (`,` [_Expression_])<sup>?</sup>

The `assert` statement is used express invariants in the code. It consists of a [boolean] expression optionally followed by a comma followed by a [string] expression.

If the [boolean] expression evaluates to `false`, the code reverts with a panic code of `0x01`. In the case that the first expression evaluates to `false` and a second [string] expression is given, the code reverts with the given string as the [error code].

<div class="warning">

Warning:
The current implementation of `assert` is under [active discussion](https://github.com/ethereum/fe/issues/516) and likely to change.

</div>

An example of a `assert` statement without the optional message:

```fe
contract Foo {
    fn bar(val: u256) {
        assert val > 5
    }
}
```

An example of a `assert` statement with an error message:

```fe
contract Foo {

    fn bar(val: u256) {
        assert val > 5, "Must be greater than five"
    }
}
```

[_Expression_]: ../expressions/index.md
[boolean]: ../type_system/types/boolean.md
[string]: ../type_system/types/string.md
[error code]: https://github.com/ethereum/EIPs/issues/838
