# Self

`self` is used to represent the specific instance of a Contract. It is used to access variables that are owned by that specific instance. This works the same way for Fe contracts as for, e.g. `self` in the context of classes in Python, or `this` in Javascript. `self` gives access to constants from the contract code and state variables from contract storage.

> Note: Here we focus on functions defined inside a contract, giving access to contract storage; however, `self` can also be used to read and write `struct` fields where functions are defined inside `structs`.

If a function takes self as a parameter, the function must be called via self. For example:

```rust
let ok: bool = self.transfer(from, to, value)
```

## Mutability

`self` is immutable and can be used for read-only operations on the contract storage (or `struct` fields). In order to write to the contract storage, you must use `mut self`. This makes the contract instance mutable and allows the contract storage to be updated.

## Examples

### Reading contract storage

```fe
contract example {

    value: u256;

    pub fn check_value(self) -> u256 {
        return self.value;
    }
}
```

### Writing contract storage

```fe
contract example {

    value: u256;

    pub fn update_value(mut self) {
        self.value += 1;
    }
}
```
