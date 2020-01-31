contract Foo:
    pub foo: map<u256, u256>

    pub def read_foo(key: u256) -> u256:
        return foo[key]
---
{}
