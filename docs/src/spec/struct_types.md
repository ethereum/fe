# Struct types

A *struct type* is the type denoted by the name of an [`struct` item].
A `struct` *type* is a heterogeneous product of other types, called the
*fields* of the type.

New instances of a `struct` can be constructed with a [struct expression].

Struct types are either stored in storage or memory but are never stored directly on the stack.

Examples:

```Python
struct Rectangle:
  width: u256
  length: u256

contract Example:
  # A Rectangle in storage
  area: Rectangle

  fn do_something():
    # A rectangle in memory
    square: Rectangle = Rectangle(width=10, length=10)
```

All fields of struct types are always initialized.

The data layout of a `struct` is not part of its external API and may be changed in any release.

The fields of a `struct` may be qualified by [visibility modifiers], to allow
access to data in a struct outside a module.

[struct expression]: struct-expr.md
[visibility modifiers]: visibility_and_privacy.md
[`struct` item]: structs.md
