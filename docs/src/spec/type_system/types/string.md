# String Type

A value of type `String<N>` represents a sequence of unsigned bytes with the assumption that the data is valid UTF-8.

The `String<N>` type is generic over a constant value that has to be an integer literal. That value `N` constraints the maximum number of bytes that are available for storing the string's characters.

Note that the value of `N` does not restrict the type to hold exactly that number of bytes at all times which means that a type such as `String<10>` can hold a short word such as `"fox"` but it can not hold a full sentence such as `"The brown fox jumps over the white fence"`.

Example:

```fe
contract Foo {

  fn bar() {
    let single_byte_string: String<1> = "a"
    let longer_string: String<100> = "foo"
  }
}
```
