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

A shape groups owned values and accesses, `-> (usize, ref T)`, and the
caller destructures it with `let (i, x) = ..`. Two `mut` components must be
structurally disjoint, as in `(mut self.a, mut self.b)`. An optional shape,
`-> Option<mut T>` or `-> Result<mut T, E>`, is consumed with `if let`,
`match`, `let .. else` or `?`; its empty variant may only be returned before
the `yield`.

## Views

A type marked `#[view]`, such as `MemSlice`, describes memory its owner
holds. It is only ever seen through a view: no `mut` binding, parameter or
result, no `own` parameter or owned result, no `Copy`, and no field of that
type. A span such as `buffer.span()` is a `ref` projection, so it keeps the
buffer reserved until its last use.

## Storage handles

A storage handle, such as a `StorageMap`, is a `Copy` value naming the field
whose entries it reaches. An operation through a handle accesses that field
for its duration: passing it to a view parameter reads the field, and to a
`mut` or owned parameter writes it. A handle read from its field names that
field; a copy held anywhere else may name any field, so an operation through
it conflicts with every open storage access.

```fe
let x = mut balances[from]
balances.set(key: to, value: 0)     // rejected: `x`'s session holds `balances`
let copy = balances
let y = copy.get(to)                // rejected: the copy names the same field
x -= amount
```

A `with` block may install a handle copy as a provider only under the
function's own authority over its field, `uses (storage: Field(StorageMap<K,
V>))`, or raw storage authority.

## External calls

A call that may start an external execution can reenter the contract and
access any of its storage, so it conflicts with every open storage access,
including those of the projections whose results the caller still uses.
Close storage accesses, or let their last use pass, before such a call.
