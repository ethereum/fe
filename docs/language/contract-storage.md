# Contract storage layout

The layout gives each `mut` contract field a root slot, in declaration order.
Storage and transient fields are numbered by one counter, so no transient slot
number is also a storage one. A field takes as many consecutive slots as its
type needs. Struct fields narrower than a word are packed into shared slots
the way Solidity packs struct members. Immutable fields live in the
contract's code, numbered by their own counter.

A storage collection, such as a `StorageMap`, `SolArray` or `TSlot`, is an
owner at its slot. It keeps its contents at that slot number in its contents'
space: a map's entry for `key` lies at `keccak256(key ++ slot)` in storage,
and a `TSlot`'s value lies in transient storage at the slot's own number. A
collection nested in a struct or a map entry derives its slot from where it
lies, as Solidity nests mappings.

## Explicit slots

`#[slot(e)]` places a `mut` field at slot `e` instead of the next free one.
`e` is any constant `u256` expression, including `const fn` calls and
`core::keccak`. The field's nested structs and entries derive from `e` as
from any root.

```fe
use core::keccak
use std::evm::{Address, SolArray, StorageMap}

struct Main {
    owner: Address,
    total: u256,
}

pub contract Store {
    // ERC-7201 namespaced storage for the namespace `example.main`.
    #[slot(keccak(keccak("example.main") - 1) & !(0xff as u256))]
    mut main: Main,
    #[slot(0x7a9f)]
    mut legacy: SolArray<u256>,
    // Counted as usual, skipping the slots placed fields take.
    mut balances: StorageMap<Address, u256>,
}
```

- Fields without `#[slot]` are numbered as before, but skip the slots that
  placed fields take in the spaces they take slots in.
- Two placed fields whose slots overlap in one address space are an error.
  Storage and transient storage are separate spaces, so a storage field and
  a transient field may share a slot number. A field also takes its slots
  in the space its collections keep their contents in, so a `TSlot` at
  slot 5 also takes transient slot 5.
- `#[slot]` on an immutable field, which lives in code, or on a struct
  field is an error.
- The checker sees an ordinary root: the attribute changes only where the
  field's slots are.
