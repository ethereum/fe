# Struct types

A *struct type* is the type denoted by the name of an [`struct` item].
A `struct` *type* is a heterogeneous product of other types, called the
*fields* of the type.

New instances of a `struct` can be constructed with a [struct expression].

Struct types are either stored in storage or memory but are never stored directly on the stack.

Examples:

```fe
struct Rectangle {
  pub width: u256
  pub length: u256
}

contract Example {
  // A Rectangle in storage
  area: Rectangle

  fn do_something() {
    let length: u256 = 20
    // A rectangle in memory
    let square: Rectangle = Rectangle(width: 10, length)
  }
}
```

All fields of struct types are always initialized.

The data layout of a `struct` is not part of its external API and may be changed in any release.

The fields of a `struct` may be qualified by [visibility modifiers], to allow
access to data in a struct outside a module.

[struct expression]: ../../expressions/struct.md
[visibility modifiers]: ../../items/visibility_and_privacy.md
[`struct` item]: ../../items/structs.md
