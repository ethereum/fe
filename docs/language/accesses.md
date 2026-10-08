# Accesses and projections

Fe has mutable value semantics: every value has one owner, and a reference
is never a value. Code reaches another owner's data through an *access*,
which exists only as a parameter, a binding, or the result of a projection,
and ends at its last use.

## Parameter modes

```fe
fn total(_ xs: [u256; 4]) -> u256 { .. }    // view: read the caller's place
fn bump(_ x: mut u256) { x += 1 }           // mut: exclusive for the call
fn consume(_ item: own Item) { .. }         // own: the callee takes the value
```

A view or `mut` argument is a place of the caller's (`bump(mut counter)`);
an owned argument is moved, or copied if its type is `Copy`.

A view argument that is a scalar or a handle (an integer, `bool`,
`Address`, a storage map, or any `core::marker::Snapshot` type) is a
*snapshot*: the callee gets a copy, and the caller may change the original
during the call. Every other value, including `Copy` arrays and structs, is
viewed in place: the caller cannot write it while the call, or a projection's
result over it, is in use.

```fe
fn first(_ xs: [u256; 4]) -> ref u256 { ref xs[0] }

let x = first(xs)
xs[1] = 5        // rejected: `x` views `xs` in place
```

## Access bindings

`let h = mut p.x` and `let r = ref p.x` bind an access to a place. It is
open from the binding to its last use, and while it is open no other access
may overlap it unless both read: `p.x` cannot be read while `h` is open, nor
written while `r` is. Pattern bindings follow the same rule: a `mut` binding
in `match p` opens a `mut` access to its part of `p`.

An access cannot be stored in a field, in storage, passed as `own` or
returned from an ordinary function. `ref` and `mut` do not appear in types.

## Projections

A function whose result is `ref T`, `mut T`, or a shape with such a
component, is a projection: it grants its caller an access.

```fe
fn first(mut self) -> mut T { mut self.items[0] }

fn entry(mut self, _ key: u256) -> mut u256 {
    let mut tmp = self.get(key)
    yield mut tmp
    self.set(key, tmp)
}
```

A projection yields once on each path that completes, implicitly at its
tail or with `yield`. The code after a `yield`, its *slide*, runs when the
caller's last use of the access has passed. While the caller uses the
access, the projection's session reserves the places its arguments named.

A yield of a value, such as `-> ref u32 { self.word.low() }`, grants a
temporary in the projection's frame holding it, as `let tmp = ..; ref tmp`
would. A `ref` yield of a place views the place; a `mut` yield grants a
place only when it is named `mut`, since `self.v` alone reads a copy. A
`mut` yield of a frame temporary with no slide to write it back is a
warning.

A shape groups owned values and accesses, `-> (usize, ref T)`, and the
caller destructures it with `let (i, x) = ..`. Two `mut` components must be
structurally disjoint, as in `(mut self.a, mut self.b)`. An optional shape,
`-> Option<mut T>` or `-> Result<mut T, E>`, is consumed with `if let`,
`match`, `let .. else` or `?`; its empty variant may only be returned before
the `yield`.

A tuple holds accesses only as such a shape. Accesses the checker cannot see
are disjoint form an *unsafe split*, an `unsafe` block whose tail is a tuple
of them: one session whose reservations are all of theirs, and whose grants
the block promises are disjoint. Two raw-slot collections are held at once
this way, and `MemArray::pair_mut` is one inside a library function:

```fe
let (a, b) = unsafe { (SolArray<u256>::at(x), SolArray<u256>::at(y)) }
a.push(1)                           // all storage stays reserved until both end
b.push(2)
```

## Views

A type marked `#[view]`, such as `MemSlice`, describes memory its owner
holds. It is only ever seen through a view: no `mut` binding, parameter or
result, no `own` parameter or owned result, no `Copy`, and no field of that
type. A span such as `buffer.span()` is a `ref` projection, so it keeps the
buffer reserved until its last use.

## Storage collections

A storage collection, such as a `StorageMap`, `StoragePackedArray` or
`TSlot`, is an owner at its storage place: the contract layout gives it a
slot, and its entries lie at slots derived from that one (a map's at
`keccak(key, slot)`). `m[k]` names an entry's place with no call
(`core::ops::PlaceIndex`), so `store.days[k].steps += 1` reads and writes one
slot. The access checker sees an entry as the collection's place indexed by a
key: entries at distinct literal keys are disjoint, and any other two
overlap. A `mut` access to a struct holding collections covers their
entries. A collection's methods take `self` or `mut self` and declare no
effects: the authority is the access to its place, as for any data.

A collection may pack small elements into lanes of shared slots, as a
`SolArray` does for Solidity's packed array elements. A lane is read and
written in place, but a `mut` binding, argument, provided effect or yield of
one works on a memory copy that is stored back into the lane when the access
ends.

```fe
let x = mut balances[from]
balances.set(key: to, value: 0)     // rejected: `x` holds an entry of `balances`
x -= amount
```

A type holding a collection is *storage-only* (`#[storage_only]`): never a
value. It is not constructed, copied, moved out of storage or held in a
memory aggregate; code reaches it through an access, a `mut` or view
parameter, or an effect. A raw-slot constructor,
`unsafe fn at(slot) -> mut Self uses (storage: mut RawStorage)`, places a
collection at a runtime slot, and its caller vouches that nothing else uses
that slot. A `with` block may install a provider naming storage only when its
place lies in one of the function's effects, under raw storage authority, or
in an `unsafe` block.

## External calls

A call that may start an external execution can reenter the contract and
access any of its storage, so it conflicts with every open storage access,
including those of the projections whose results the caller still uses.
Close storage accesses, or let their last use pass, before such a call.
