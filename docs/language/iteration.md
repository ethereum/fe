# Iteration

A `for` loop traverses a collection by a cursor, a value the loop holds,
and projects each element from the collection as it goes.

```fe
for x in xs { total += x }              // a view of each element
for x in mut xs { x *= 2 }              // a `mut` access to each element
for (i, x) in xs by enumerate() { .. }  // a driver
for y in xs by map(|x: u256| x * 2) { .. }
```

`Collection` (`core::iter`) is the protocol: `start` and `next` move a
cursor, `at` projects the element at it. `CollectionMut` adds `at` with
`mut` access for `for .. in mut ..` loops, and `Bidirectional` traverses in
reverse. Arrays, `MemArray`, `MemSlice`, ranges, `SolArray` and
`SolEnumerableSet` are collections; a collection declares the effects its
methods need (`uses E`) and the space of the places it yields (`space S`).

## Drivers and producers

`for x in xs by d` traverses `xs` through the driver `d`, which keeps its own
state and never holds the collection. The standard drivers are
`reversed()`, `enumerate()`, `take(n)`, `skip(n)`, `step_by(k)` and
`filter(p)`. A driver that yields an extra value with each element, such as
`enumerate`, binds a pair: `for (i, x) in xs by enumerate()`. A producer,
such as `map(f)`, yields owned values instead of accesses.

Drivers combine as methods, `xs.reversed().enumerate().take(3)`, and a
method chain on a collection is sugar for `by`:
`for x in xs.filter(|x| x > 1)` is `for x in xs by filter(..)`.

A two-base driver traverses two bases in step: `for (x, y) in (a, b) by zip()`
views both until either ends, and `for (x, y) in (mut a, mut b) by zip()`
mutates both, which needs `a` and `b` disjoint.

## Higher-order operations

Collections provide `fold`, `any`, `count` and `find`. `fold` owns its
accumulator and views each element, so neither needs to be `Copy`:

```fe
let totals = accounts.fold(Totals { sum: 0 }, |acc, account| {
    Totals { sum: acc.sum + account.balance }
})
```

`MemArray::collect(xs, map(f))` collects a producer's values into an array.
See [closures](closures.md) for the callables these take.
