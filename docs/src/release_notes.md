
# Release Notes

[🖥️ Download Binaries](https://github.com/ethereum/fe/releases)
[📄 Draft Spec](https://github.com/ethereum/fe/tree/master/spec)
[ℹ️ Getting Started](https://github.com/ethereum/fe#getting-started)

Fe is moving fast. Read up on all the latest improvements.

**WARNING: All Fe releases are alpha releases and only meant to share the development progress with developers and enthusiasts. It is NOT yet ready for production usage.**

[//]: # (towncrier release notes start)
## 0.9.0-alpha (2021-09-29)## 0.9.0-alpha (2021-09-29)


### Features


- The `self` variable is no longer implicitly defined in code blocks. It must now be declared
  as the first parameter in a function signature.

  Example:

  ```
  contract Foo:
      my_stored_num: u256

      pub fn bar(self, my_num: u256):
          self.my_stored_num = my_num
        
      pub fn baz(self):
          self.bar(my_pure_func())
        
      pub fn my_pure_func() -> u256:
          return 42 + 26
  ``` ([#520](https://github.com/ethereum/fe/issues/520))
- The analyzer now disallows defining a type, variable, or function whose
  name conflicts with a built-in type, function, or object.

  Example:

  ```
  error: type name conflicts with built-in type
  ┌─ compile_errors/shadow_builtin_type.fe:1:6
  │
  1 │ type u256 = u8
  │      ^^^^ `u256` is a built-in type
  ``` ([#539](https://github.com/ethereum/fe/issues/539))


### Bugfixes


- Fixed cases where the analyzer would correctly reject code, but would panic instead of logging an error message. ([#534](https://github.com/ethereum/fe/issues/534))
- Non-fatal parser errors (eg missing parentheses when defining a function that takes no arguments: `fn foo:`)
  are no longer ignored if the semantic analysis stage succeeds. ([#535](https://github.com/ethereum/fe/issues/535))
- Fixed issue #531 by adding a `$` to the front of lowered tuple names. ([#546](https://github.com/ethereum/fe/issues/546))


### Internal Changes - for Fe Contributors


- Implemented pretty printing of Fe AST. ([#540](https://github.com/ethereum/fe/issues/540))


## 0.8.0-alpha "Haxonite" (2021-08-31)


### Features


- Support quotes, tabs and carriage returns in string literals and otherwise
  restrict string literals to the printable subset of the ASCII table. ([#329](https://github.com/ethereum/fe/issues/329))
- The analyzer now uses a query-based system, which fixes some shortcomings of the previous implementation.

  - Types can now refer to other types defined later in the file.
  Example:
  ```
  type Posts = Map<PostId, PostBody>
  type PostId = u256
  type PostBody = String<140>
  ```

  - Duplicate definition errors now show the location of the original definition.
  - The analysis of each function, type definition, etc happens independently, so an error in one
  doesn't stop the analysis pass. This means fe can report more user errors in a single run of the compiler. ([#468](https://github.com/ethereum/fe/issues/468))
- Function definitions are now denoted with the keyword fn instead of def. ([#496](https://github.com/ethereum/fe/issues/496))
- Variable declarations are now preceded by the `let` keyword. Example: `let x: u8 = 1`. ([#509](https://github.com/ethereum/fe/issues/509))
- Implemented support for numeric unary invert operator (`~`) ([#526](https://github.com/ethereum/fe/issues/526))


### Bugfixes


- Calling `self.__init__()` now results in a nice error instead of a panic in the yul compilation stage. ([#468](https://github.com/ethereum/fe/issues/468))
- Fixed an issue where certain expressions were not being moved to the correct location. ([#493](https://github.com/ethereum/fe/issues/493))
- Fixed an issue with a missing return statement not properly detected.

  Previous to this fix, the following code compiles but it should not:

  ```
  contract Foo:
      pub fn bar(val: u256) -> u256:
          if val > 1:
              return 5
  ```

  With this change, the compiler rightfully detects that the code is missing
  a `return` or `revert` statement after the `if` statement since it is not
  guaranteed that the path of execution always follows the arm of the `if` statement. ([#497](https://github.com/ethereum/fe/issues/497))
- Fixed a bug in the analyzer which allowed tuple item accessor names with a leading 0,
  resulting in an internal compiler error in a later pass. Example: `my_tuple.item001`.
  These are now rejected with an error message. ([#510](https://github.com/ethereum/fe/issues/510))
- Check call argument labels for function calls.

  Previously the compiler would not check any labels that were used
  when making function calls on `self` or external contracts.

  This can be especially problematic if gives developers the impression
  that they could apply function arguments in any order as long as they
  are named which is **not** the case. 

  ```
  contract Foo:

      pub fn baz():
          self.bar(val2=1, doesnt_even_exist=2)
    
      pub fn bar(val1: u256, val2: u256):
          pass
  ```

  Code as the one above is now rightfully rejected by the compiler. ([#517](https://github.com/ethereum/fe/issues/517))


### Improved Documentation


- Various improvements and bug fixes to both the content and layout of the specification. ([#489](https://github.com/ethereum/fe/issues/489))
- Document all remaining statements and expressions in the spec.

  Also added a CI check to ensure code examples in the documentation
  are validated against the latest compiler. ([#514](https://github.com/ethereum/fe/issues/514))


### Internal Changes - for Fe Contributors


- Separated Fe type traits between crates. ([#485](https://github.com/ethereum/fe/issues/485))


## 0.7.0-alpha "Galaxite" (2021-07-27)

### Features


- Enable the optimizer by default. The optimizer can still be disabled
  by supplying `--optimize=false` as an argument. ([#439](https://github.com/ethereum/fe/issues/439))
- The following checks are now performed while decoding data:

  - The size of the encoded data fits within the size range known at compile-time.
  - Values are correctly padded.
    - unsigned integers, addresses, and bools are checked to have correct left zero padding
    - the size of signed integers are checked
    - bytes and strings are checked to have correct right padding
  - Data section offsets are consistent with the size of preceding values in the data section.
  - The dynamic size of strings does not exceed their maximum size.
  - The dynamic size of byte arrays (`u8[n]`) is equal to the size of the array. ([#440](https://github.com/ethereum/fe/issues/440))
- Type aliases can now include tuples. Example:
  ```
  type InternetPoints = (address, u256)
  ```
  ([#459](https://github.com/ethereum/fe/issues/459))
- Revert with custom errors

  Example:

  ```
  struct PlatformError:
    code: u256

  pub fn do_something():
    revert PlatformError(code=4711)
  ```

  Error encoding [follows Solidity](https://docs.soliditylang.org/en/v0.8.4/abi-spec.html#errors) which is based on [EIP-838](https://github.com/ethereum/EIPs/issues/838). This means that custom errors returned from Fe are fully compatible with Solidity. ([#464](https://github.com/ethereum/fe/issues/464))
- - The builtin value `msg.sig` now has type `u256`.
  - Removed the `bytes[n]` type. The type `u8[n]` can be used in its placed and will be encoded as a dynamically-sized, but checked, bytes component. ([#472](https://github.com/ethereum/fe/issues/472))
- Encode certain reverts as panics.

  With this change, the following reverts are encoded as `Panic(uint256)` with
  the following panic codes:

  - `0x01`: An assertion that failed and did not specify an error message
  - `0x11`: An arithmetic expression resulted in an over- or underflow
  - `0x12`: An arithmetic expression divided or modulo by zero

  The panic codes are aligned with [the panic codes that Solidity uses](https://docs.soliditylang.org/en/v0.8.4/control-structures.html?highlight=Panic#panic-via-assert-and-error-via-require). ([#476](https://github.com/ethereum/fe/issues/476))


### Bugfixes


- Fixed a crash when trying to access an invalid attribute on a string.

  Example:

  ```
  contract Foo:

    pub fn foo():
      "".does_not_exist
  ```

  The above now yields a proper user error. ([#444](https://github.com/ethereum/fe/issues/444))
- Ensure `String<N>` type is capitalized in error messages ([#445](https://github.com/ethereum/fe/issues/445))
- Fixed ICE when using a static string that spans over multiple lines.

  Previous to this fix, the following code would lead to a compiler crash:

  ```
  contract Foo:
      pub fn return_with_newline() -> String<16>:
          return "foo
          balu"
  ```

  The above code now works as intended. ([#448](https://github.com/ethereum/fe/issues/448))
- Fixed ICE when using a tuple declaration and specifying a non-tuple type.
  Fixed a second ICE when using a tuple declaration where the number of
  target items doesn't match the number of items in the declared type. ([#469](https://github.com/ethereum/fe/issues/469))


### Internal Changes - for Fe Contributors


- - Cleaned up ABI encoding internals.
  - Improved yulc panic formatting. ([#472](https://github.com/ethereum/fe/issues/472))


## 0.6.0-alpha "Feldspar" (2021-06-10)


### Features


- Support for `pragma` statement

  Example: `pragma ^0.1.0` ([#361](https://github.com/ethereum/fe/issues/361))
- Add support for tuple destructuring

  Example:

  ```
  my_tuple: (u256, bool) = (42, true)
  (x, y): (u256, bool) = my_tuple
  ```
  ([#376](https://github.com/ethereum/fe/issues/376))
- 1. Call expression can now accept generic arguments
  2. Replace `stringN` to `String<N>`

  Example:

  ```
  s: String<10> = String<10>("HI")
  ```
  ([#379](https://github.com/ethereum/fe/issues/379))
- - Many analyzer errors now include helpful messages and underlined code.
  - Event and struct constructor arguments must now be labeled and in the order specified in the definition.
  - The analyzer now verifies that the left-hand side of an assignment is actually assignable. ([#398](https://github.com/ethereum/fe/issues/398))
- Types of integer literal are now inferred, rather than defaulting to `u256`.

  ```
  contract C:

    fn f(x: u8) -> u16:
      y: u8 = 100   # had to use u8(100) before
      z: i8 = -129  # "literal out of range" error

      return 1000   # had to use `return u16(1000)` before

    fn g():
      self.f(50)
  ```

  Similar inference is done for empty array literals. Previously, empty array
  literals caused a compiler crash, because the array element type couldn't
  be determined.
  ```
  contract C:
    fn f(xs: u8[10]):
      pass

    fn g():
      self.f([])
  ```
  (Note that array length mismatch is still a type error, so this code won't
  actually compile.) ([#429](https://github.com/ethereum/fe/issues/429))
- The Map type name is now capitalized. Example:

  ```
  contract GuestBook:
      guests: Map<address, String<100>>
  ```
  ([#431](https://github.com/ethereum/fe/issues/431))
- Convert all remaining errors to use the new advanced error reporting system ([#432](https://github.com/ethereum/fe/issues/432))
- Analyzer throws an error if `__init__` is not public. ([#435](https://github.com/ethereum/fe/issues/435))


### Internal Changes - for Fe Contributors


- Refactored front-end "not implemented" errors into analyzer errors and removed questionable variants. Any panic is now considered to be a bug. ([#437](https://github.com/ethereum/fe/issues/437))


## 0.5.0-alpha (2021-05-27)


### Features


- Add support for hexadecimal/octal/binary numeric literals.

  Example:

  ```
  value_hex: u256 = 0xff
  value_octal: u256 = 0o77
  value_binary: u256 = 0b11
  ```
  ([#333](https://github.com/ethereum/fe/issues/333))
- Added support for list expressions.

  Example:

  ```
  values: u256[3] = [10, 20, 30]

  # or anywhere else where expressions can be used such as in a call

  sum: u256 = self.sum([10, 20, 30])
  ```
  ([#388](https://github.com/ethereum/fe/issues/388))
- Contracts, events, and structs can now be empty.

  e.g.

  ```
  event MyEvent:
      pass

  ...
    
  contract MyContract:
      pass
   
  ...
    
  struct MyStruct:
      pass
  ```
  ([#406](https://github.com/ethereum/fe/issues/406))
- External calls can now handle dynamically-sized return types. ([#415](https://github.com/ethereum/fe/issues/415))


### Bugfixes


- The analyzer will return an error if a tuple attribute is not of the form `item<index>`. ([#401](https://github.com/ethereum/fe/issues/401))


### Improved Documentation


- Created a landing page for Fe at https://fe.ethereum.org ([#394](https://github.com/ethereum/fe/issues/394))
- Provide a Quickstart chapter in Fe Guide ([#403](https://github.com/ethereum/fe/issues/403))


### Internal Changes - for Fe Contributors


- Using insta to validate Analyzer outputs. ([#387](https://github.com/ethereum/fe/issues/387))
- Analyzer now disallows using `context.add_` methods to update attributes. ([#392](https://github.com/ethereum/fe/issues/392))
- `()` now represents a distinct type internally called the unit type, instead of an empty tuple.

  The lowering pass now does the following: Valueless return statements are given a `()` value and 
  functions without a return value are given explicit `()` returns. ([#406](https://github.com/ethereum/fe/issues/406))
- Add CI check to ensure fragment files always end with a new line ([#4711](https://github.com/ethereum/fe/issues/4711))


## 0.4.0-alpha (2021-04-28)


### Features


- Support for revert messages in assert statements

  E.g

  ```
  assert a == b, "my revert statement"
  ```

  The provided string is abi-encoded as if it were a call
  to a function `Error(string)`. For example, the revert string `"Not enough Ether provided."` returns the following hexadecimal as error return data:

  ```
  0x08c379a0                                                         // Function selector for Error(string)
  0x0000000000000000000000000000000000000000000000000000000000000020 // Data offset
  0x000000000000000000000000000000000000000000000000000000000000001a // String length
  0x4e6f7420656e6f7567682045746865722070726f76696465642e000000000000 // String data
  ```
  ([#288](https://github.com/ethereum/fe/issues/288))

- Added support for augmented assignments.

  e.g.

  ```
  contract Foo:
      pub fn add(a: u256, b: u256) -> u256:
          a += b
          return a

      pub fn sub(a: u256, b: u256) -> u256:
          a -= b
          return a

      pub fn mul(a: u256, b: u256) -> u256:
          a *= b
          return a

      pub fn div(a: u256, b: u256) -> u256:
          a /= b
          return a

      pub fn mod(a: u256, b: u256) -> u256:
          a %= b
          return a

      pub fn pow(a: u256, b: u256) -> u256:
          a **= b
          return a

      pub fn lshift(a: u8, b: u8) -> u8:
          a <<= b
          return a

      pub fn rshift(a: u8, b: u8) -> u8:
          a >>= b
          return a

      pub fn bit_or(a: u8, b: u8) -> u8:
          a |= b
          return a

      pub fn bit_xor(a: u8, b: u8) -> u8:
          a ^= b
          return a

      pub fn bit_and(a: u8, b: u8) -> u8:
          a &= b
          return a
  ```
  ([#338](https://github.com/ethereum/fe/issues/338))

- A new parser implementation, which provides more helpful error messages
  with fancy underlines and code context. ([#346](https://github.com/ethereum/fe/issues/346))
- Added support for tuples with base type items.

  e.g.

  ```
  contract Foo:
      my_num: u256

      pub fn bar(my_num: u256, my_bool: bool) -> (u256, bool):
          my_tuple: (u256, bool) = (my_num, my_bool)
          self.my_num = my_tuple.item0
          return my_tuple
  ```
  ([#352](https://github.com/ethereum/fe/issues/352))


### Bugfixes


- Properly reject invalid emit ([#211](https://github.com/ethereum/fe/issues/211))
- Properly tokenize numeric literals when they start with 0 ([#331](https://github.com/ethereum/fe/issues/331))
- Reject non-string assert reasons as type error ([#335](https://github.com/ethereum/fe/issues/335))
- Properly reject code that creates a circular dependency when using `create` or `create2`.

  Example, the follwing code is now rightfully rejected because it tries to create an
  instance of `Foo` from within the `Foo` contract itself.

  ```
  contract Foo:
    pub fn bar()->address:
      foo:Foo=Foo.create(0)

      return address(foo)
  ```
  ([#362](https://github.com/ethereum/fe/issues/362))


### Internal Changes - for Fe Contributors


- AST nodes use `String`s instead of `&str`s. This way we can perform incremental compilation on the AST. ([#332](https://github.com/ethereum/fe/issues/332))
- Added support for running tests against solidity fixtures.
  Also added tests that cover how solidity encodes revert reason strings. ([#342](https://github.com/ethereum/fe/issues/342))
- Refactoring of binary operation type checking. ([#347](https://github.com/ethereum/fe/issues/347))


## 0.3.0-alpha "Calamine" (2021-03-24)


### Features


- Add over/underflow checks for multiplications of all integers ([#271](https://github.com/ethereum/fe/issues/271))
- Add full support for empty Tuples. ([#276](https://github.com/ethereum/fe/issues/276))

  All functions in Fe implicitly return an empty Tuple if they have no other return value.
  However, before this change one was not able to use the empty Tuple syntax `()` explicitly.

  With this change, all of these are treated equally:

  ```
  contract Foo:

    pub fn explicit_return_a1():
      return

    pub fn explicit_return_a2():
      return ()

    pub fn explicit_return_b1() ->():
      return

    pub fn explicit_return_b2() ->():
      return ()

    pub fn implicit_a1():
      pass

    pub fn implicit_a2() ->():
      pass
  ```
  
- The JSON ABI builder now supports structs as both input and output. ([#296](https://github.com/ethereum/fe/issues/296))
- Make subsequently defined contracts visible.

  Before this change:

  ```
  # can't see Bar
  contract Foo:
     ...
  # can see Foo
  contract Bar:
     ...
  ```

  With this change the restriction is lifted and the following becomes possible. ([#298](https://github.com/ethereum/fe/issues/298))

  ```
  contract Foo:
      bar: Bar
      pub fn external_bar() -> u256:
          return self.bar.bar()
  contract Bar:
      foo: Foo
      pub fn external_foo() -> u256:
          return self.foo.foo()
  ```

- Perform checks for divison operations on integers ([#308](https://github.com/ethereum/fe/issues/308))
- Support for msg.sig to read the function identifier. ([#311](https://github.com/ethereum/fe/issues/311))
- Perform checks for modulo operations on integers ([#312](https://github.com/ethereum/fe/issues/312))
- Perform over/underflow checks for exponentiation operations on integers ([#313](https://github.com/ethereum/fe/issues/313))


### Bugfixes


- Properly reject `emit` not followed by an event invocation ([#212](https://github.com/ethereum/fe/issues/212))
- Properly reject octal number literals ([#222](https://github.com/ethereum/fe/issues/222))
- Properly reject code that tries to emit a non-existing event. ([#250](https://github.com/ethereum/fe/issues/250))

  Example that now produces a compile time error:

  ```
  emit DoesNotExist()
  ```
- Contracts that create other contracts can now include `__init__` functions.

  See https://github.com/ethereum/fe/issues/284 ([#304](https://github.com/ethereum/fe/issues/304))
- Prevent multiple types with same name in one module. ([#317](https://github.com/ethereum/fe/issues/317))

  Examples that now produce compile time errors:

  ```
  type bar = u8
  type bar = u16
  ```

  or

  ```
  struct SomeStruct:
      some_field: u8

  struct SomeStruct:
      other: u8
  ```

  or

  ```
  contract SomeContract:
      some_field: u8

  contract SomeContract:
      other: u8
  ```


  Prevent multiple fields with same name in one struct.

  Example that now produces a compile time error:

  ```
  struct SomeStruct:
      some_field: u8
      some_field: u8
  ```


  Prevent variable definition in child scope when name already taken in parent scope.

  Example that now produces a compile time error:

  ```
  pub fn bar():
      my_array: u256[3]
      sum: u256 = 0
      for i in my_array:
          sum: u256 = 0
  ```
- The CLI was using the overwrite flag to enable Yul optimization.

  i.e.

  ```
  # Would both overwite output files and run the Yul optimizer. 
  $ fe my_contract.fe --overwrite
  ```


  Using the overwrite flag now only overwrites and optimization is enabled with the optimize flag. ([#320](https://github.com/ethereum/fe/issues/320))
- Ensure analyzer rejects code that uses return values for `__init__` functions. ([#323](https://github.com/ethereum/fe/issues/323))

  An example that now produces a compile time error:

  ```
  contract C:
      pub fn __init__() -> i32:
          return 0
  ```
- Properly reject calling an undefined function on an external contract ([#324](https://github.com/ethereum/fe/issues/324))


### Internal Changes - for Fe Contributors


- Added the Uniswap demo contracts to our testing fixtures and validated their behaviour. ([#179](https://github.com/ethereum/fe/issues/179))
- IDs added to AST nodes. ([#315](https://github.com/ethereum/fe/issues/315))
- Failures in the Yul generation phase now panic; any failure is a bug. ([#327](https://github.com/ethereum/fe/issues/327))


## 0.2.0-alpha "Borax" (2021-02-27)


### Features


- Add support for string literals.

  Example:

  ```
  fn get_ticker_symbol() -> string3:
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

      pub fn get_price() -> u256:
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
      pub fn build_array(a: u256, b: u256) -> u256[3]:
          my_array: u256[3]
          my_array[0] = a
          my_array[1] = a * b
          my_array[2] = b
          return my_array

  contract FooProxy:
      pub fn call_build_array(
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
  fn post_fork() -> bool:
      return block.number > 2675000
  ```
- The CLI now panics if an error is encountered during Yul compilation. ([#218](https://github.com/ethereum/fe/issues/218))
- Support for contract creations.

  Example of `create2`, which takes a `value` and address `salt` as parameters.

  ```
  contract Foo:
      pub fn get_my_num() -> u256:
          return 42

  contract FooFactory:
      pub fn create2_foo() -> address:
          # value and salt
          foo: Foo = Foo.create2(0, 52)
          return address(foo)
  ```

  Example of `create`, which just takes a `value` parameter.

  ```
  contract Foo:
      pub fn get_my_num() -> u256:
          return 42

  contract FooFactory:
      pub fn create_foo() -> address:
          # value and salt
          foo: Foo = Foo.create(0)
          return address(foo)
  ```

  *Note: We do not yet support init parameters.* ([#239](https://github.com/ethereum/fe/issues/239))
- Support updating individual struct fields in storage. ([#246](https://github.com/ethereum/fe/issues/246))

  Example:

  ```
   pub fn update_house_price(price: u256):
          self.my_house.price = price
  ``` 
- Implement global `keccak256` method. The method expects one parameter of `bytes[n]`
  and returns the hash as an `u256`. In a future version `keccak256` will most likely
  be moved behind an import so that it has to be imported (e.g. `from std.crypto import keccak256`). ([#255](https://github.com/ethereum/fe/issues/255))

  Example:

  ```
  pub fn hash_single_byte(val: bytes[1]) -> u256:
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
      pub fn hashed_house() -> u256:
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
      pub fn bar(x: bool, y: bool) -> bool:
          return x and y
  ```

  ```
  contract Foo:
      pub fn bar(x: bool, y: bool) -> bool:
          return x or y
  ```

  Support for `self.address`.

  This expression returns the address of the current contract.

  Example:

  ```
  contract Foo:
      pub fn bar() -> address:
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

      pub fn foo():
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
    pub fn explicit_return():
      return

    pub fn implicit():
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
  pub fn bar(val: u8) -> u16:
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


