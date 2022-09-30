# Index expressions

> **<sup>Syntax</sup>**\
> _IndexExpression_ :\
> &nbsp;&nbsp; [_Expression_] `[` [_Expression_] `]`

[Array] and [Map] types can be indexed by by writing a square-bracket-enclosed expression after them. For arrays, the type of the index key has to be `u256` whereas for [Map] types it has to be equal to the key type of the map.


Example:

```fe
contract Foo {

    balances: Map<address, u256>


    pub fn baz(mut self, mut values: Array<u256, 10>) {
        // Assign value at slot 5
        values[5] = 1000
        // Read value at slot 5
        let val1: u256 = values[5]

        // Assign value for address zero
        self.balances[address(0)] = 10000

        // Read balance of address zero
        let bal: u256 = self.balances[address(0)]
    }
}
```

[_Expression_]: ./index.md
[Array]: ../type_system/types/array.md
[Map]: ../type_system/types/map.md
