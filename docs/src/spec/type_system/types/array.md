# Array types

> **<sup>Syntax</sup>**\
> _ArrayType_ :\
> &nbsp;&nbsp; Array<[_Type_], _INTEGER_LITERAL_>

An array is a fixed-size sequence of `N` elements of type `T`. The array type
is written as `Array<T, N>`. The size is an integer literal.

Arrays are either stored in storage or memory but are never stored directly on the stack.

Examples:

```fe
contract Foo {
  // An array in storage
  bar: Array<u8, 10>

  fn do_something() {
    // An array in memory
    let values: Array<u256, 3> = [10, 100, 100]
  }
}
```

All elements of arrays are always initialized, and access to an array is
always bounds-checked in safe methods and operators.

[_Type_]: ./index.md
