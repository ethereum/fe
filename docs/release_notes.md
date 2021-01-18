
# Release Notes

[🖥️ Download Binaries](https://github.com/ethereum/fe/releases)
[📄 Draft Spec](https://github.com/ethereum/fe/tree/master/spec)
[ℹ️ Getting Started](https://github.com/ethereum/fe#getting-started)


Fe is moving fast. Read up on all the latest improvements.



[//]: # (towncrier release notes start)
## 0.1.0-alpha "Amethyst" (2021-01-20)

**WARNING: This is an alpha version to share the development progress with developers and enthusiasts. It is NOT yet intended to be used for anything serious. At this point Fe is missing a lot of features and has a lot of bugs instead.**

This is the first **alpha** release and kicks off our release schedule which will be one release every month in the future. Since we have just started tracking progress on changes, the following list of changes is incomplete, but will appropriately document progress between releases from now on.

### Features


- Added support for `for loop`, allows iteration over static arrays. ([#134](https://github.com/ethereum/fe/issues/134))
- Enforce bounds on numeric literals in type constructors.

  For instance calling `u8(1000)` or `i8(-250)` will give an error because
  the literals `1000` and `-250` do not fit into `u8` or `i8`. ([#145](https://github.com/ethereum/fe/issues/145))
- Added builtin copying methods `clone()` and `to_mem()` to reference types. ([#155](https://github.com/ethereum/fe/issues/155))


  usage:

  ```
  # copy a segment of storage into memory and assign the new pointer
  my_mem_array = self.my_sto_array.to_mem()

  # copy a segment of memory into another segment of memory and assign the new pointer
  my_other_mem_array = my_mem_array.clone()
  ```
- Support emitting JSON ABI via `--emit abi`.
  The default value of `--emit` is now `abi,bytecode`. ([#160](https://github.com/ethereum/fe/issues/160))
- Ensure integer type constructor reject all expressions that aren't a numeric literal.
  For instance, previously the compiler would not reject the following code even though it could not be guaranteed that `val` would fit into an `u16`.

  ```
  pub def bar(val: u8) -> u16:
          return u16(val)
  ```

  Now such code is rejected and integer type constructor do only work with numeric literals such as `1` or `-3`. ([#163](https://github.com/ethereum/fe/issues/163))
- Support for ABI decoding of all array type. ([#172](https://github.com/ethereum/fe/issues/172))
- Support for value assignments in declaration.

  Previously, this code would fail:

  ```
  another_reference: u256[10] = my_array
  ```

  As a workaround declaration and assignment could be split apart.

  ```
  another_reference: u256[10]
  another_reference = my_array
  ```

  With this change, the shorter declaration with assignment syntax is supported. ([#173](https://github.com/ethereum/fe/issues/173))


### Improved Documentation


- Point to examples in the README ([#162](https://github.com/ethereum/fe/issues/162))
- Overhaul README page to better reflect the current state of the project. ([#177](https://github.com/ethereum/fe/issues/177))
- Added descriptions of the `to_mem` and `clone` functions to the spec. ([#195](https://github.com/ethereum/fe/issues/195))


### Internal Changes - for Fe Contributors


- Updated the Solidity backend to v0.8.0. ([#169](https://github.com/ethereum/fe/issues/169))
- Run CI tests on Mac and support creating Mac binaries for releases. ([#178](https://github.com/ethereum/fe/issues/178))


