# Maps in storage

Maps are not assigned pointers, because they do not have a location in storage. They are instead
assigned a nonce that is used to derive the location of keyed values during runtime.

Example:

```fe
contract Foo {
  bar: Map<address, u256> // bar is assigned a static nonce by the compiler
  baz: Map<address, Map<address, u256>> // baz is assigned a static nonce by the compiler
}
```

The expression `bar[0x00]` would resolve to the hash of both bar's nonce and the key value
.i.e. `keccak256(<bar nonce>, 0x00)`. Similarly, the expression `baz[0x00][0x01]` would resolve to
a nested hash i.e. `keccak256(keccak256(<baz nonce>, 0x00), 0x01)`.
