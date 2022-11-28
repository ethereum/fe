# Map type

The type `Map<K, V>` is used to associate key values with data.

The following types can be used as key:

- [unit type]
- [boolean type]
- [address type]
- [numeric types]

The values can be of any type including other maps, [structs], [tuples] or [arrays].

Example:

```fe
contract Foo {
    bar: Map<address, Map<address, u256>>
    baz: Map<address, Map<u256, bool>>

    pub fn read_bar(self, a: address, b: address) -> u256 {
        return self.bar[a][b]
    }

    pub fn write_bar(mut self, a: address, b: address, value: u256) {
        self.bar[a][b] = value
    }

    pub fn read_baz(self, a: address, b: u256) -> bool {
        return self.baz[a][b]
    }

    pub fn write_baz(mut self, a: address, b: u256, value: bool) {
        self.baz[a][b] = value
    }
}
```

[unit type]: unit.md
[boolean type]: boolean.md
[address type]: address.md
[numeric types]: numeric.md
[structs]: struct.md
[tuples]: tuple.md
[arrays]: array.md
