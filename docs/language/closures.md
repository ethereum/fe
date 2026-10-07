# Closures

A closure is a function value written inline:

```fe
let inc = |x: u256| x + 1
let total = xs.fold(0, |acc, x| acc + x)
let max = |a: own u256, b: u256| -> u256 { if b > a { b } else { a } }
```

Each parameter has a mode and a type, as a function parameter does
(`x: u256` is a view, `x: own u256` owned, `x: mut u256` exclusive). Either
may be left out: `|x|`, or `|x: own|` for a mode without a type. The return
type is optional too.

## Callable shapes

Because modes are not types, a closure's type is described by its *shape*,
the modes of its parameters and their types. Bounds name shapes directly:

```fe
fn fold<A, F: FnMut(own A, Self::Item) -> A>(self, _ init: own A, _ f: mut F) -> A
    uses (E, F::E)
```

`Fn(own A, B) -> R` is the core trait `Fn_ov<A, B, Out = R>`, whose
`call(self, ..)` views the callable; `FnMut(..)` is `FnMut_ov`, whose
`call_mut(mut self, ..)` mutates it; `Fn(own T) -> U` is `Fn<T, U>`. A closure
implements the `Fn` and `FnMut` shapes of its parameter modes, with `Out` its
result. Call it with `f.call(x)`, or `f.call_mut(x)` through an `FnMut`
bound.

Unannotated parameters take their modes and types from the shape bound of
the parameter the closure is passed to, after the arguments before it: in
`xs.fold(0, |acc, x| acc + x)`, `acc` is `own u256` and `x` is a view of the
item. A closure passed to a function without such a bound, like the free
`filter` and `map` adapters used with `for .. by`, annotates its parameters:
`for x in xs by filter(|x: u256| x % 2 == 0)`. A closure literal may be
passed to a `mut` parameter directly.

## Captures

A closure captures what its body uses from the enclosing body by copy, for
`Copy` values, or by move. The captures are the closure's own, and read-only:
writing to one, taking `mut` access to it, or moving a non-`Copy` capture out
of the body is an error. A closure cannot capture an access (a view
parameter, a `ref` or `mut` binding) of a non-`Copy` value; pass the value as
a parameter instead. Thread state through parameters and results:
`fold(init, |acc, x| acc + x)`.

## Effects

The effects a closure's body uses, by naming an enclosing effect binding or
by calling a function that requires one, are not captured: they are the
closure's row `E`, which the caller of each call provides. A higher-order
function forwards it with `uses F::E`:

```fe
fn count<P: Fn(Self::Item) -> bool>(self, _ p: P) -> usize uses (E, P::E)

fn matching(_ xs: [u256; 4]) -> usize uses (store: Store) {
    xs.count(|x| x == store.n)
}
```

The provider is the one in scope where `count` is called, so a closure made
under `with (Store = a)` and passed to a higher-order function called under
`with (Store = b)` uses `b`. A `with` inside the closure's body provides the
effect itself. A row the body needs, such as `C::E` of a collection it
captures, is part of the closure's row too.

A closure cannot be used in a constant context.
