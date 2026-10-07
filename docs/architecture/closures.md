# Closures architecture

A closure is sugar for a struct of captures and a function of the callable
shape traits ([language](../language/closures.md)). The compiler keeps that
structure without generating items: a closure's type, its implementations of
the shape traits, and its body's instances are all derived from the closure
expression.

## Types

`ClosureTy` (`ty_def`) is interned from the closure's definition (body and
expression), the parent's generic arguments, the capture types (the
environment's fields), the parameters' modes and types, the result, and the
row's components. Closure types unify structurally. A closure environment is
a product like a tuple (`TyId::is_product`): its fields are its captures.

## Shape implementations

`ty::closure` is the single home for the relationship between a closure and
the shape traits. `CallableShape::of` recognizes a core shape trait
(`Fn_<modes>`, `FnMut_<modes>`, `Fn0`, `Fn<T, U>`) by name. The trait solver
adds a `Closure` clause when a goal's self type is a closure, building an
`ImplementorOrigin::Closure` implementor from the goal in the goal's own
table; the normalizer resolves `Out` to the closure's result; the row `E`
expands to the closure's components (`effects::rows`); method selection adds
both shapes' methods as candidates, `Fn`'s `call` and `FnMut`'s `call_mut`.

## Checking

A closure body is checked inline in its parent's body, so it sees the
enclosing bindings and its types are inferred together with the parent's.
Uses of a binding registered outside the closure are its captures. The
parent's effect environment is swapped out for the body: an effect the body
needs and its `with` blocks do not provide becomes a component of the row,
seeded as the body's own effect on first use, and the body is generic over
the component's provider through a closure-owned provider parameter, assumed
to implement a trait key's trait for the rest of the body
(`TyParam::row_effect_provider`). A function body names its rows' components
the same way where its view expands every row, so a `Bidirectional` method can
use the components of `Collection`'s row `E`.

An argument's closure expectation comes from the callee's shape bound on the
parameter (`Callable::closure_arg_expectation`).

## Lowering

The closure expression lowers to the aggregate of its captures. Its body is
the owner `BodyOwner::Closure`, one per receiver mode (`self` for `Fn`,
`mut self` for `FnMut`), whose typed body is the parent's viewed as the
closure's (`TypedBody::for_closure`: the environment and the closure's
parameters are its parameters). `closure_regions` tells which expressions
belong to which closure body, so each instance resolves only its own call
sites. Captures are read from the environment's fields.

A shape trait's `call` on a closure resolves to the closure body's instance
(`semantic_callee_key_with_assumptions`): its generic substitution is the
parent's arguments the closure type carries, and its effects are the
components of `call`'s row in order, with the providers the call binds;
`RowProviderSubst` replaces the body's provider parameters with them.
