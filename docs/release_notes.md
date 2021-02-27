
# Release Notes

[🖥️ Download Binaries](https://github.com/ethereum/fe/releases)
[📄 Draft Spec](https://github.com/ethereum/fe/tree/master/spec)
[ℹ️ Getting Started](https://github.com/ethereum/fe#getting-started)


Fe is moving fast. Read up on all the latest improvements.

**WARNING: All Fe releases are alpha releases and only meant to share the development progress with developers and enthusiasts. It is NOT yet ready for production usage.**

[//]: # (towncrier release notes start)
## 0.2.0-alpha "Borax" (2021-02-27)


### Features


- Add support for string literals.

  Example:

  ```
  def get_ticker_symbol() -> string3:
      return "ETH"
  ```

  String literals are stored in and loaded from the compiled bytecode. ([#186](https://github.com/ethereum/fe/issues/186))
- The CLI now compiles every contract in a module, not just the first one. ([#197](https://github.com/ethereum/fe/issues/197))

  Sample compiler output with all targets enabled:

  ```
  output
  |-- Bar
  |   |-- Bar.bin
  |   |-- Bar_abi.json
  |   `-- Bar_ir.yul
  |-- Foo
  |   |-- Foo.bin
  |   |-- Foo_abi.json
  |   `-- Foo_ir.yul
  |-- module.ast
  `-- module.tokens
  ```

- Add support for string type casts ([#201](https://github.com/ethereum/fe/issues/201))

  Example:

  ```
  val: string100 = string100("foo")
  ```
- Add basic support for structs. ([#203](https://github.com/ethereum/fe/issues/203))

  Example:

  ```
  struct House:
      price: u256
      size: u256
      vacant: bool

  contract City:

      pub def get_price() -> u256:
          building: House = House(300, 500, true)

          assert building.size == 500
          assert building.price == 300
          assert building.vacant

          return building.price
  ``` 
- Added support for external contract calls. Contract definitions now 
  add a type to the module scope, which may be used to create contract 
  values with the contract's public functions as callable attributes. ([#204](https://github.com/ethereum/fe/issues/204))

  Example:

  ```python
  contract Foo:
      pub def build_array(a: u256, b: u256) -> u256[3]:
          my_array: u256[3]
          my_array[0] = a
          my_array[1] = a * b
          my_array[2] = b
          return my_array

  contract FooProxy:
      pub def call_build_array(
          foo_address: address,
          a: u256,
          b: u256,
      ) -> u256[3]:
          foo: Foo = Foo(foo_address)
          return foo.build_array(a, b)
  ```
- Add support for `block`, `msg`, `chain`, and `tx` properties: ([#208](https://github.com/ethereum/fe/issues/208))
  ```
  block.coinbase: address
  block.difficulty: u256
  block.number: u256
  block.timestamp: u256
  chain.id: u256
  msg.value: u256
  tx.gas_price: u256
  tx.origin: address
  ```
  (Note that `msg.sender: address` was added previously.)

  Example:
  ```
  def post_fork() -> bool:
      return block.number > 2675000
  ```
- The CLI now panics if an error is encountered during Yul compilation. ([#218](https://github.com/ethereum/fe/issues/218))
- Support for contract creations.

  Example of `create2`, which takes a `value` and address `salt` as parameters.

  ```
  contract Foo:
      pub def get_my_num() -> u256:
          return 42

  contract FooFactory:
      pub def create2_foo() -> address:
          # value and salt
          foo: Foo = Foo.create2(0, 52)
          return address(foo)
  ```

  Example of `create`, which just takes a `value` parameter.

  ```
  contract Foo:
      pub def get_my_num() -> u256:
          return 42

  contract FooFactory:
      pub def create_foo() -> address:
          # value and salt
          foo: Foo = Foo.create(0)
          return address(foo)
  ```

  *Note: We do not yet support init parameters.* ([#239](https://github.com/ethereum/fe/issues/239))
- Support updating individual struct fields in storage. ([#246](https://github.com/ethereum/fe/issues/246))

  Example:

  ```
   pub def update_house_price(price: u256):
          self.my_house.price = price
  ``` 
- Implement global `keccak256` method. The method expects one parameter of `bytes[n]`
  and returns the hash as an `u256`. In a future version `keccak256` will most likely
  be moved behind an import so that it has to be imported (e.g. `from std.crypto import keccak256`). ([#255](https://github.com/ethereum/fe/issues/255))

  Example:

  ```
  pub def hash_single_byte(val: bytes[1]) -> u256:
      return keccak256(val)
  ```
- Require structs to be initialized using keyword arguments.

  Example:

  ```
  struct House:
      vacant: bool
      price: u256
  ```

  Previously, `House` could be instantiated as `House(true, 1000000)`.
  With this change it is required to be instantiated like `House(vacant=true, price=1000000)`

  This ensures property assignment is less prone to get mixed up. It also makes struct
  initialization visually stand out more from function calls. ([#260](https://github.com/ethereum/fe/issues/260))
- Implement support for boolean `not` operator. ([#264](https://github.com/ethereum/fe/issues/264))

  Example:

  ```
  if not covid_test.is_positive(person):
      allow_boarding(person)
  ```
- Do over/underflow checks for additions (SafeMath).

  With this change all additions (e.g `x + y`) for signed and unsigned
  integers check for over- and underflows and revert if necessary. ([#265](https://github.com/ethereum/fe/issues/265))
- Added a builtin function `abi_encode()` that can be used to encode stucts. The return type is a 
  fixed-size array of bytes that is equal in size to the encoding. The type system does not support 
  dynamically-sized arrays yet, which is why we used fixed. ([#266](https://github.com/ethereum/fe/issues/266))

  Example:

  ```
  struct House:
      price: u256
      size: u256
      rooms: u8
      vacant: bool
    
  contract Foo:
      pub def hashed_house() -> u256:
          house: House = House(
              price=300,
              size=500,
              rooms=u8(20),
              vacant=true
          )
          return keccak256(house.abi_encode())
  ```
- Perform over/underflow checks for subtractions (SafeMath). ([#267](https://github.com/ethereum/fe/issues/267))

  With this change all subtractions (e.g `x - y`) for signed and unsigned
  integers check for over- and underflows and revert if necessary. 
- Support for the boolean operations `and` and `or`. ([#270](https://github.com/ethereum/fe/issues/270))

  Examples:

  ```
  contract Foo:
      pub def bar(x: bool, y: bool) -> bool:
          return x and y
  ```

  ```
  contract Foo:
      pub def bar(x: bool, y: bool) -> bool:
          return x or y
  ```

  Support for `self.address`.

  This expression returns the address of the current contract.

  Example:

  ```
  contract Foo:
      pub def bar() -> address:
          return self.address
  ```


### Bugfixes


- Perform type checking when calling event constructors

  Previously, the following would not raise an error even though it should:

  ```
  contract Foo:
      event MyEvent:
          val_1: string100
          val_2: u8

      pub def foo():
          emit MyEvent("foo", 1000)

  ```

  Wit this change, the code fails with a type error as expected. ([#202](https://github.com/ethereum/fe/issues/202))
- Fix bug where compilation of contracts without public functions would result in illegal YUL. ([#219](https://github.com/ethereum/fe/issues/219))

  E.g without this change, the following doesn't compile to proper YUL

  ```
  contract Empty:
    lonely: u256
  ```
- Ensure numeric literals can't exceed 256 bit range. Previously, this would result in a
  non user friendly error at the YUL compilation stage. With this change it is caught
  at the analyzer stage and presented to the user as a regular error. ([#225](https://github.com/ethereum/fe/issues/225))
- Fix crash when return is used without value.

  These two methods should both be treated as returning `()`

  ```
    pub def explicit_return():
      return

    pub def implicit():
      pass
  ```

  Without this change, the `explicit_return` crashes the compiler. ([#261](https://github.com/ethereum/fe/issues/261))


### Internal Changes - for Fe Contributors


- Renamed the fe-semantics library to fe-analyzer. ([#207](https://github.com/ethereum/fe/issues/207))
- Runtime testing utilities. ([#243](https://github.com/ethereum/fe/issues/243))
- Values are stored more efficiently in storage. ([#251](https://github.com/ethereum/fe/issues/251))


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


