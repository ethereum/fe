# Map type

The type `Map<K, V>` is used to associate key values with data.

The following types can be used as key:

- [Unit type]
- [boolean type]
- [address type]
- [numeric types]

The values can be of any type including other maps, [structs], [tuples] or [arrays].

Example:

```python
contract Foo:
    bar: Map<address, Map<address, u256>>
    baz: Map<address, Map<u256, bool>>

    pub fn read_bar(self, a: address, b: address) -> u256:
        return self.bar[a][b]

    pub fn write_bar(self, a: address, b: address, value: u256):
        self.bar[a][b] = value

    pub fn read_baz(self, a: address, b: u256) -> bool:
        return self.baz[a][b]

    pub fn write_baz(self, a: address, b: u256, value: bool):
        self.baz[a][b] = value
```

[boolean type]: boolean_type.md
[address type]: address_type.md
[numeric types]: numeric_types.md
[structs]: structs.md
[tuples]: tuple_types.md
[arrays]: array_types.md

