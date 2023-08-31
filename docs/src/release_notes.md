
# Release Notes

[🖥️ Download Binaries](https://github.com/ethereum/fe/releases)
[📄 Draft Spec](https://github.com/ethereum/fe/tree/master/spec)
[ℹ️ Getting Started](https://github.com/ethereum/fe#getting-started)

Fe is moving fast. Read up on all the latest improvements.

[//]: # (towncrier release notes start)
## 0.24.0 "Xenotime" (2023-08-10)


### Features


- Added support for project manifests and project dependencies.

  Example:

  ```
  my_project
  ├── fe.toml
  └── src
      └── main.fe
  ```

  ```
  # fe.toml
  name = "my_project"
  version = "1.0"

  [dependencies]
  my_lib = { path = "../path/to/my_lib", version = "1.0" }
  my_other_lib = "../path/to/my_other_lib"
  ```

  Note: The current implementation supports circular dependencies. ([#908](https://github.com/ethereum/fe/issues/908))


### Performance improvements


- `MemoryBuffer` now allocates an extra 31 bytes. This removes the need for runtime checks and bitshifting needed to ensure safe writing to a `MemoryBuffer`'s region. ([#898](https://github.com/ethereum/fe/issues/898))


### Improved Documentation


- Link to vs-code extension in Quickstart Guide ([#910](https://github.com/ethereum/fe/issues/910))


## 0.23.0 "Wiluite" (2023-06-01)


### Features


- Fixed an issue where generic parameters that were `mut` could not be satisfied at callsite.

  For instance, the following code would previously cause a compile error but now works as expected:

  ```rust
  struct Runner {
    pub fn run<T: Computable>(self, mut _ val: T) -> u256 {
      return val.compute(val: 1000)
    }
  }

  contract Example {
    pub fn run_test(self) {
      let runner: Runner = Runner();
      let mut mac: Mac = Mac();

      assert runner.run(mac) == 1001
    }
  }
  ```
  ([#865](https://github.com/ethereum/fe/issues/865))
- The `ctx` parameter can now be passed into test functions.

  example:

  ```
  #test
  fn my_test(ctx: Context) {
      assert ctx.block_number() == 0
  }
  ```
  ([#880](https://github.com/ethereum/fe/issues/880))
- The following has been added to the standard library:

  **Memory buffer abstraction**

  example:

  ```
  use std::buf::{MemoryBuffer, MemoryBufferReader, MemoryBufferWriter}
  use std::traits::Max

  #test
  fn test_buf_rw() {
      let mut buf: MemoryBuffer = MemoryBuffer::new(len: 161) 
      let mut writer: MemoryBufferWriter = buf.writer()
      let mut reader: MemoryBufferReader = buf.reader()

      writer.write(value: 42)
      writer.write(value: 42)
      writer.write(value: 26)
      writer.write(value: u8(26))
      writer.write(value: u256::max())
      writer.write(value: u128::max())
      writer.write(value: u64::max())
      writer.write(value: u32::max())
      writer.write(value: u16::max())
      writer.write(value: u8::max())
      writer.write(value: u8(0))

      assert reader.read_u256() == 42
      assert reader.read_u256() == 42
      assert reader.read_u256() == 26
      assert reader.read_u8() == 26
      assert reader.read_u256() == u256::max()
      assert reader.read_u128() == u128::max()
      assert reader.read_u64() == u64::max()
      assert reader.read_u32() == u32::max()
      assert reader.read_u16() == u16::max()
      assert reader.read_u8() == u8::max()
      assert reader.read_u8() == 0
  }
  ```

  **Precompiles**

  example:

  ```
  use std::precompiles
  use std::buf::{MemoryBuffer, MemoryBufferReader, MemoryBufferWriter}

  #test
  fn test_ec_recover() {
      let result: address = precompiles::ec_recover(
          hash: 0x456e9aea5e197a1f1af7a3e85a3212fa4049a3ba34c2289b4c860fc0b0c64ef3,
          v: 28,
          r: 0x9242685bf161793cc25603c231bc2f568eb630ea16aa137d2664ac8038825608,
          s: 0x4f8ae3bd7535248d0bd448298cc2e2071e56992d0774dc340c368ae950852ada
      )

      assert result == address(0x7156526fbd7a3c72969b54f64e42c10fbb768c8a)
  }
  ```

  **`ctx.raw_call()`**

  example:

  ```
  use std::buf::{
      RawCallBuffer,
      MemoryBufferReader, 
      MemoryBufferWriter
  }
  use std::evm

  contract Foo {
      pub unsafe fn __call__() {
          if evm::call_data_load(offset: 0) == 42 {
              evm::mstore(offset: 0, value: 26)
              evm::return_mem(offset: 0, len: 32)
          } else if evm::call_data_load(offset: 0) == 26 {
              revert
          }
      }
  }

  #test
  fn test_raw_call(mut ctx: Context) {
      let foo: Foo = Foo.create(ctx, 0)

      let mut buf: RawCallBuffer = RawCallBuffer::new(
          input_len: 32, 
          output_len: 32
      )
      let mut writer: MemoryBufferWriter = buf.writer()

      writer.write(value: 42)
      assert ctx.raw_call(addr: address(foo), value: 0, buf)
    
      let mut reader: MemoryBufferReader = buf.reader()
      assert reader.read_u256() == 26

      assert not ctx.raw_call(addr: address(foo), value: 0, buf)
  }
  ```
  ([#885](https://github.com/ethereum/fe/issues/885))


### Bugfixes


- Fixed an ICE when using aggregate types with aggregate type fields in public functions

  This code would previously cause an ICE:

  ```fe
  struct Tx {
    pub data: Array<u8, 320>
  }

  contract Foo {
    pub fn bar(mut tx: Tx) {}
  }
  ```
  ([#867](https://github.com/ethereum/fe/issues/867))
- Fixed a regression where the compiler would not reject a method call on a struct in storage.

  E.g. the follwing code should be rejected as it is missing a `to_mem()` call:

  ```
  struct Bar {
      pub x: u256

      pub fn get_x(self) -> u256{
          return self.x
      }
  }

  contract Foo {
      bar: Bar

      pub fn __init__(mut self) {
          self.bar = Bar( x: 2 )
      }
      fn yay(self) {
          self.bar.get_x()
      }
  }
  ```

  The compiler will now reject the code and suggest a `to_mem()` before calling`get_x()`. ([#881](https://github.com/ethereum/fe/issues/881))


## 0.22.0 "Vulcanite" (2023-04-05)

This is the first non-alpha release of Fe. Read our [announcement](https://blog.fe-lang.org/posts/beyond-alpha-preparing-fe-for-the-future/) for more details.

### Features


- Support for tests.

  example:

  ```
  #test
  fn my_test() {
      assert 26 + 16 == 42
  }
  ```

  Tests can be executed using the `test` subcommand.

  example:

  `$ fe test foo.fe` ([#807](https://github.com/ethereum/fe/issues/807))
- Fixed broken trait orphan rule

  Fe has an orphan rule for Traits similar to Rust's that requires
  that either the trait or the type that we are implementing the trait for
  are located in the same ingot as the `impl`. This rule was implemented
  incorrectly so that instead of requiring them to be in the same ingot,
  they were required to be in the same module. This change fixes this
  so that the orphan rule is enforced correctly.
  ([#863](https://github.com/ethereum/fe/issues/863))

- `address` values can now be specified with a number literal, with no explicit
  cast. So instead of `let t: address = address(0xfe)`, one can now write
  `let t: address = 0xfe`. This also means that it's possible to define `const`
  addresses: `const SOME_KNOWN_CONTRACT: address = 0xfefefefe`
  ([#864](https://github.com/ethereum/fe/issues/864))


### Bugfixes


- Fixed resolving of generic arguments to associated functions.

  For example, this code would previously crash the compiler:

  ```rust
  ...
    // This function doesn't take self
    pub fn run_static<T: Computable>(_ val: T) -> u256 {
      return val.compute(val: 1000)
    }
  ...

  // Invoking it would previously crash the compiler
  Runner::run_static(Mac())
  ...
  ```
  ([#861](https://github.com/ethereum/fe/issues/861))


### Improved Documentation


- Changed the Deployment tutorial to use foundry and the Sepolia network ([#853](https://github.com/ethereum/fe/issues/853))


## 0.21.0-alpha (2023-02-28)


### Features


- Support for `Self` type

  With this change `Self` (with capital `S`) can be used to refer
  to the enclosing type in contracts, structs, impls and traits.

  E.g.

  ```
  trait Min {
    fn min() -> Self;
  }

  impl Min for u8 {
    fn min() -> u8 { // Both `u8` or `Self` are valid here
      return 0
    }
  }
  ```

  Usage: `u8::min()` ([#803](https://github.com/ethereum/fe/issues/803))
- Added `Min` and `Max` traits to the std library.
  The std library implements the traits for all numeric types.

  Example

  ```
  use std::traits::{Min, Max}
  ...

  assert u8::min() < u8::max()
  ``` ([#836](https://github.com/ethereum/fe/issues/836))

- Upgraded underlying solc compiler to version `0.8.18`

### Bugfixes

- the release contains minor bugfixes

## 0.20.0-alpha (2022-12-05)


### Features


- Removed the `event` type as well as the `emit` keyword.
  Instead the `struct` type now automatically implements
  the `Emittable` trait and can be emitted via `ctx.emit(..)`.

  Indexed fields can be annotated via the `#indexed` attribute.

  E.g.

  ```
  struct Signed {
      book_msg: String<100>
  }

  contract GuestBook {
      messages: Map<address, String<100>>

      pub fn sign(mut self, mut ctx: Context, book_msg: String<100>) {
          self.messages[ctx.msg_sender()] = book_msg
          ctx.emit(Signed(book_msg))
      }
  }
  ```
  ([#717](https://github.com/ethereum/fe/issues/717))

- Allow to call trait methods on types when trait is in scope

  So far traits were only useful as bounds for generic functions.
  With this change traits can also be used as illustrated with
  the following example:

  ```
  trait Double {
    fn double(self) -> u256;
  }

  impl Double for (u256, u256) {
    fn double(self) -> u256 {
      return (self.item0 + self.item1) * 2
    }
  }

  contract Example {

    pub fn run_test(self) {
      assert (0, 1).double() == 2
    }
  }
  ```

  If a call turns out to be ambigious the compiler currently asks the
  user to disambiguate via renaming. In the future we will likely
  introduce a syntax to allow to disambiguate at the callsite. ([#757](https://github.com/ethereum/fe/issues/757))

- Allow contract associated functions to be called via `ContractName::function_name()` syntax. ([#767](https://github.com/ethereum/fe/issues/767))
- Add `enum` types and `match` statement.

  `enum` can now be defined, e.g.,

  ```fe
  pub enum MyEnum {
      Unit
      Tuple(u32, u256, bool)
    
      fn unit() -> MyEnum {
          return MyEnum::Unit
      }
  }
  ```

  Also, `match` statement is introduced, e.g.,

  ```fe,ignore
  pub fn eval_enum()  -> u256{
      match MyEnum {
          MyEnum::Unit => { 
              return 0
          }
        
          MyEnum::Tuple(a, _, false) => {
              return u256(a)
          }
        
          MyEnum::Tuple(.., true) => {
              return u256(1)
          }
      }
  }
  ```


  For now, available patterns are restricted to 
  * Wildcard(`_`), which matches all patterns: `_`
  * Named variable, which matches all patterns and binds the value to make the value usable in the arm. e.g., `a`, `b` and `c` in `MyEnum::Tuple(a, b, c)`
  * Boolean literal(`true` and `false`)
  * Enum variant. e.g., `MyEnum::Tuple(a, b, c)`
  * Tuple pattern. e.g., `(a, b, c)`
  * Struct pattern. e.g., `MyStruct {x: x1, y: y1, b: true}` 
  * Rest pattern(`..`), which matches the rest of the pattern. e.g., `MyEnum::Tuple(.., true)`
  * Or pattern(|). e.g., MyEnum::Unit | MyEnum::Tuple(.., true)

  Fe compiler performs the exhaustiveness and usefulness checks for `match` statement.  
  So the compiler will emit an error when all patterns are not covered or an unreachable arm are detected. ([#770](https://github.com/ethereum/fe/issues/770))
- Changed comments to use `//` instead of `#` ([#776](https://github.com/ethereum/fe/issues/776))
- Added the `mut` keyword, to mark things as mutable. Any variable or function parameter
  not marked `mut` is now immutable.

  ```fe
  contract Counter {
      count: u256

      pub fn increment(mut self) -> u256 {
          // `self` is mutable, so storage can be modified
          self.count += 1
          return self.count
      }
  }

  struct Point {
      pub x: u32
      pub y: u32

      pub fn add(mut self, _ other: Point) {
          self.x += other.x
          self.y += other.y

          // other.x = 1000 // ERROR: `other` is not mutable
      }
  }

  fn pointless() {
      let origin: Point = Point(x: 0, y: 0)
      // origin.x = 10 // ERROR: origin is not mutable

      let x: u32 = 10
      // x_coord = 100 // ERROR: `x_coord` is not mutable
      let mut y: u32 = 0
      y = 10 // OK

      let mut p: Point = origin // copies `origin`
      p.x = 10 // OK, doesn't modify `origin`

      let mut q: Point = p // copies `p`
      q.x = 100            // doesn't modify `p`

      p.add(q)
      assert p.x == 110
  }
  ```

  Note that, in this release, primitive type function parameters
  can't be `mut`. This restriction might be lifted in a future release.

  For example:
  ```fe,ignore
  fn increment(mut x: u256) { // ERROR: primitive type parameters can't be mut
      x += 1
  }
  ```
  ([#777](https://github.com/ethereum/fe/issues/777))
- The contents of the `std::prelude` module (currently just the `Context` struct)
  are now automatically `use`d by every module, so `use std::context::Context` is
  no longer required. ([#779](https://github.com/ethereum/fe/issues/779))
- When the Fe compiler generates a JSON ABI file for a contract, the
  "stateMutability" field for each function now reflects whether the function can
  read or modify chain or contract state, based on the presence or absence of the
  `self` and `ctx` parameters, and whether those parameters are `mut`able.

  If a function doesn't take `self` or `ctx`, it's "pure".
  If a function takes `self` or `ctx` immutably, it can read state but not mutate
  state, so it's a "view"
  If a function takes `mut self` or `mut ctx`, it can mutate state, and is thus
  marked "payable".

  Note that we're following the convention set by Solidity for this field, which
  isn't a perfect fit for Fe. The primary issue is that Fe doesn't currently
  distinguish between "payable" and "nonpayable" functions; if you want a function
  to revert when Eth is sent, you need to do it manually
  (eg `assert ctx.msg_value() == 0`). ([#783](https://github.com/ethereum/fe/issues/783))

- Trait associated functions

  This change allows trait functions that do not take a `self` parameter.
  The following demonstrates a possible trait associated function and its usage:

  ```
  trait Max {
    fn max(self) -> u8;
  }

  impl Max for u8 {
    fn max() -> u8 {
      return u8(255)
    }
  }

  contract Example {

    pub fn run_test(self) {
      assert u8::max() == 255
    }
  }
  ```
  ([#805](https://github.com/ethereum/fe/issues/805))


### Bugfixes


- Fix issue where calls to assiciated functions did not enforce visibility rules.

  E.g the following code should be rejected but previously wasn't:

  ```
  struct Foo {
      fn do_private_things() {
      }
  }

  contract Bar {
      fn test() {
          Foo::do_private_things()
      }
  }
  ```

  With this change, the above code is now rejected because `do_private_things` is not `pub`. ([#767](https://github.com/ethereum/fe/issues/767))
- Padding on `bytes` and `string` ABI types is zeroed out. ([#769](https://github.com/ethereum/fe/issues/769))
- Ensure traits from other modules or even ingots can be implemented ([#773](https://github.com/ethereum/fe/issues/773))
- Certain cases where the compiler would not reject pure functions
  being called on instances are now properly rejected. ([#775](https://github.com/ethereum/fe/issues/775))
- Reject calling `to_mem()` on primitive types in storage ([#801](https://github.com/ethereum/fe/issues/801))
- Disallow importing private type via `use`

  The following was previously allowed but will now error:

  `use foo::PrivateStruct` ([#815](https://github.com/ethereum/fe/issues/815))


## 0.19.1-alpha "Sunstone" (2022-07-06)


### Features


- Support returning nested struct.

  Example:
  ```
  pub struct InnerStruct {
      pub inner_s: String<10>
      pub inner_x: i256
  }

  pub struct NestedStruct {
      pub inner: InnerStruct
      pub outer_x: i256
  }

  contract Foo {
      pub fn return_nested_struct() -> NestedStruct {
          ...
      }
  }
  ```
  ([#635](https://github.com/ethereum/fe/issues/635))
- Made some small changes to how the `Context` object is used.

  - `ctx` is not required when casting an address to a contract type. Eg `let foo: Foo = Foo(address(0))`
  - `ctx` is required when calling an external contract function that requires ctx

  Example:

  ```fe,ignore
  use std::context::Context // see issue #679

  contract Foo {
    pub fn emit_stuff(ctx: Context) {
      emit Stuff(ctx)  # will be `ctx.emit(Stuff{})` someday
    }
  }
  contract Bar {
    pub fn call_foo_emit_stuff(ctx: Context) {
      Foo(address(0)).emit_stuff(ctx)
    }
  }
  event Stuff {}
  ```
  ([#703](https://github.com/ethereum/fe/issues/703))
- Braces! Fe has abandoned python-style significant whitespace in favor of the
  trusty curly brace.

  In addition, `elif` is now spelled `else if`, and the `pass`
  statement no longer exists.

  Example:
  ```fe
  pub struct SomeError {}

  contract Foo {
    x: u8
    y: u16

    pub fn f(a: u8) -> u8 {
      if a > 10 {
        let x: u8 = 5
        return a + x
      } else if a == 0 {
        revert SomeError()
      } else {
        return a * 10
      }
    }

    pub fn noop() {}
  }
  ```
  ([#707](https://github.com/ethereum/fe/issues/707))
- traits and generic function parameter

  Traits can now be defined, e.g:

  ```
  trait Computable {
    fn compute(self, val: u256) -> u256;
  }
  ```

  The mechanism to implement a trait is via an `impl` block e.g:

  ```
  struct Linux {
    pub counter: u256
    pub fn get_counter(self) -> u256 {
      return self.counter
    }
    pub fn something_static() -> u256 {
      return 5
    }
  }

  impl Computable for Linux {
    fn compute(self, val: u256) -> u256 {
      return val + Linux::something_static() + self.get_counter()
    }
  }
  ```

  Traits can only appear as bounds for generic functions e.g.:

  ```
  struct Runner {

    pub fn run<T: Computable>(self, _ val: T) -> u256 {
      return val.compute(val: 1000)
    }
  }
  ```

  Only `struct` functions (not `contract` functions) can have generic parameters.
  The `run` method of `Runner` can be called with any type that implements `Computable` e.g.

  ```
  contract Example {

    pub fn generic_compute(self) {
      let runner: Runner = Runner();
      assert runner.run(Mac()) == 1001
      assert runner.run(Linux(counter: 10)) == 1015
    }
  }
  ```
  ([#710](https://github.com/ethereum/fe/issues/710))
- Generate artifacts for all contracts of an ingot, not just for contracts that are defined in `main.fe` ([#726](https://github.com/ethereum/fe/issues/726))
- Allow using complex type as array element type.

  Example:
  ```
  contract Foo {
      pub fn bar() -> i256 {
          let my_array: Array<Pair, 3> = [Pair::new(1, 0), Pair::new(2, 0), Pair::new(3, 0)]

          let sum: i256 = 0
          for pair in my_array {
              sum += pair.x
          }

          return sum
      }
  }

  struct Pair {
      pub x: i256
      pub y: i256

      pub fn new(_ x: i256, _ y: i256) -> Pair {
          return Pair(x, y)
      }
  }
  ```
  ([#730](https://github.com/ethereum/fe/issues/730))
- The `fe` CLI now has subcommands:

  `fe new myproject` - creates a new project structure
  `fe check .`       - analyzes fe source code and prints errors
  `fe build .`       - builds a fe project ([#732](https://github.com/ethereum/fe/issues/732))
- Support passing nested struct types to public functions.

  Example:
  ```
  pub struct InnerStruct {
      pub inner_s: String<10>
      pub inner_x: i256
  }

  pub struct NestedStruct {
      pub inner: InnerStruct
      pub outer_x: i256
  }

  contract Foo {
      pub fn f(arg: NestedStruct) {
          ...
      }
  }
  ```
  ([#733](https://github.com/ethereum/fe/issues/733))
- Added support for repeat expressions (`[VALUE; LENGTH]`).

  e.g.

  ```
  let my_array: Array<bool, 42> = [bool; 42]
  ```

  Also added checks to ensure array and struct types are initialized. These checks are currently performed at the declaration site, but will be loosened in the future. ([#747](https://github.com/ethereum/fe/issues/747))


### Bugfixes

- Fix a bug that incorrect instruction is selected when the operands of a comp instruction are a signed type. ([#734](https://github.com/ethereum/fe/issues/734))
- Fix issue where a negative constant leads to an ICE

  E.g. the following code would previously crash the compiler but shouldn't:

  ```
  const INIT_VAL: i8 = -1
  contract Foo {
    pub fn init_bar() {
      let x: i8 = INIT_VAL
    }
  }
  ```
  ([#745](https://github.com/ethereum/fe/issues/745))
- Fix a bug that causes ICE when nested if-statement has multiple exit point.

  E.g. the following code would previously crash the compiler but shouldn't:
  ```fe,ignore
   pub fn foo(self) {
      if true {
          if self.something {
              return
          }
      }
      if true {
          if self.something {
              return
          }
      }
  }
  ```
  ([#749](https://github.com/ethereum/fe/issues/749))


## 0.18.0-alpha "Ruby" (2022-05-27)

### Features


- Added support for parsing of attribute calls with generic arguments (e.g. `foo.bar<Baz>()`). ([#719](https://github.com/ethereum/fe/issues/719))


### Bugfixes

- Fix a regression where the `stateMutability` field would not be included in the generated ABI ([#722](https://github.com/ethereum/fe/issues/722))
- Fix two regressions introduced in `0.17.0`
  * Properly lower right shift operation to yul's `sar` if operand is signed type
  * Properly lower negate operation to call `safe_sub` ([#723](https://github.com/ethereum/fe/issues/723))


## 0.17.0-alpha "Quartz" (2022-05-26)

### Features


- Support for underscores in numbers to improve readability e.g. `100_000`.

  Example

  ```
      let num: u256 = 1000_000_000_000
  ```
  ([#149](https://github.com/ethereum/fe/issues/149))
- Optimized access of struct fields in storage ([#249](https://github.com/ethereum/fe/issues/249))
- Unit type `()` is now ABI encodable ([#442](https://github.com/ethereum/fe/issues/442))
- Temporary default `stateMutability` to `payable` in ABI

  The ABI metadata that the compiler previously generated did not include the `stateMutability` field. This piece of information is important for tooling such as hardhat because it determines whether a function needs to be called with or without sending a transaction.

  As soon as we have support for `mut self` and `mut ctx` we will be able to derive that information from the function signature. In the meantime we now default to `payable`. ([#705](https://github.com/ethereum/fe/issues/705))


### Bugfixes


- Fixed a crash caused by certain memory to memory assignments.

  E.g. the following code would previously lead to a compiler crash:

  ```
  my_struct.x = my_struct.y
  ```
  ([#590](https://github.com/ethereum/fe/issues/590))
- Reject unary minus operation if the target type is an unsigned integer number.

  Code below should be reject by `fe` compiler:

  ```python
  contract Foo:
      pub fn bar(self) -> u32:
          let unsigned: u32 = 1
          return -unsigned

      pub fn foo():
          let a: i32 = 1
          let b: u32 = -a
  ```
  ([#651](https://github.com/ethereum/fe/issues/651))
- Fixed crash when passing a struct that contains an array

  E.g. the following would previously result in a compiler crash:

  ```
  struct MyArray:
      pub x: Array<i32, 2>


  contract Foo:
      pub fn bar(my_arr: MyArray):
          pass
  ```
  ([#681](https://github.com/ethereum/fe/issues/681))
- reject infinite size struct definitions.

  Fe `structs` having infinite size due to recursive definitions were not rejected earlier and would cause ICE in the analyzer since they were not properly handled. Now `structs` having infinite size are properly identified by detecting cycles in the dependency graph of the struct field definitions and an error is thrown by the analyzer. ([#682](https://github.com/ethereum/fe/issues/682))
- Return instead of revert when contract is called without data.

  If a contract is called without data so that no function is invoked,
  we would previously `revert` but that would leave us without a
  way to send ETH to a contract so instead it will cause a `return` now. ([#694](https://github.com/ethereum/fe/issues/694))
- Resolve compiler crash when using certain reserved YUL words as struct field names.

  E.g. the following would previously lead to a compiler crash because `numer` is
  a reserved keyword in YUL.

  ```
  struct Foo:
    pub number: u256

  contract Meh:

    pub fn yay() -> Foo:
      return Foo(number:2)
  ```
  ([#709](https://github.com/ethereum/fe/issues/709))


## 0.16.0-alpha (2022-05-05)

### Features


- Change static function call syntax from `Bar.foo()` to `Bar::foo()` ([#241](https://github.com/ethereum/fe/issues/241))
- Added support for retrieving the base fee via `ctx.base_fee()` ([#503](https://github.com/ethereum/fe/issues/503))


### Bugfixes


- Resolve functions on structs via path (e.g. `bi::ba::bums()`) ([#241](https://github.com/ethereum/fe/issues/241))


## 0.15.0-alpha (2022-04-04)


### Features


- Labels are now required on function arguments. Labels can be omitted if the
  argument is a variable with a name that matches the label, or if the function
  definition specifies that an argument should have no label. Functions often take
  several arguments of the same type; compiler-checked labels can help prevent
  accidentally providing arguments in the wrong order.

  Example:
  ```
  contract CoolCoin:
    balance: Map<address, i256>
    loans: Map<(address, address), i256>

    pub fn demo(self, ann: address, bob: address):
      let is_loan: bool = false
      self.give(from: ann, to: bob, 100, is_loan)

    fn transfer(self, from sender: address, to recipient: address, _ val: u256, is_loan: bool):
      self.cred[sender] -= val
      self.cred[recipient] += val
      if is_loan:
        self.loans[(sender, recipient)] += val
  ```

  Note that arguments must be provided in the order specified in the function
  definition.

  A parameter's label defaults to the parameter name, but can be changed by
  specifying a different label to the left of the parameter name. Labels should be
  clear and convenient for the caller, while parameter names are only used in the
  function body, and can thus be longer and more descriptive.
  In the example above, we choose to use `sender` and `recipient` as identifiers
  in the body of `fn transfer`, but use labels `from:` and `to:`.

  In cases where it's ideal to not have labels, e.g. if a function takes a single
  argument, or if types are sufficient to differentiate between arguments, use `_`
  to specify that a given parameter has no label. It's also fine to require labels
  for some arguments, but not others.

  Example:
  ```
  fn add(_ x: u256, _ y: u256) -> u256:
    return x + y

  contract Foo:
    fn transfer(self, _ to: address, wei: u256):
      pass

    pub fn demo(self):
      transfer(address(0), wei: add(1000, 42))
  ```
  ([#397](https://github.com/ethereum/fe/issues/397))


### Bugfixes


- The region of memory used to compute the slot of a storage map value was not being allocated. ([#684](https://github.com/ethereum/fe/issues/684))


## 0.14.0-alpha (2022-03-02)


### Features


- Events can now be defined outside of contracts.

  Example:
  ```
  event Transfer:
      idx sender: address
      idx receiver: address
      value: u256

  contract Foo:
      fn transferFoo(to: address, value: u256):
          emit Transfer(sender: msg.sender, receiver: to, value)

  contract Bar:
      fn transferBar(to: address, value: u256):
          emit Transfer(sender: msg.sender, receiver: to, value)
  ```
  ([#80](https://github.com/ethereum/fe/issues/80))

- The Fe standard library now includes a `std::evm` module, which provides functions that perform low-level evm operations.
  Many of these are marked `unsafe`, and thus can only be used inside of an `unsafe` function or an `unsafe` block.

  Example:
  ```
  use std::evm::{mstore, mload}

  fn memory_shenanigans():
    unsafe:
      mstore(0x20, 42)
      let x: u256 = mload(0x20)
      assert x == 42
  ```

  The global functions `balance` and `balance_of` have been removed; these can now be called as `std::evm::balance()`, etc.
  The global function `send_value` has been ported to Fe, and is now available
  as `std::send_value`.
  ([#629](https://github.com/ethereum/fe/issues/629))

- Support structs that have non-base type fields in storage.

  Example:

  ```
  struct Point:
      pub x: u256
      pub y: u256

  struct Bar:
      pub name: String<3>
      pub numbers: Array<u256, 2>
      pub point: Point
      pub something: (u256, bool)


  contract Foo:
      my_bar: Bar

      pub fn complex_struct_in_storage(self) -> String<3>:
          self.my_bar = Bar(
              name: "foo",
              numbers: [1, 2],
              point: Point(x: 100, y: 200),
              something: (1, true),
          )

          # Asserting the values as they were set initially
          assert self.my_bar.numbers[0] == 1
          assert self.my_bar.numbers[1] == 2
          assert self.my_bar.point.x == 100
          assert self.my_bar.point.y == 200
          assert self.my_bar.something.item0 == 1
          assert self.my_bar.something.item1

          # We can change the values of the array
          self.my_bar.numbers[0] = 10
          self.my_bar.numbers[1] = 20
          assert self.my_bar.numbers[0] == 10
          assert self.my_bar.numbers[1] == 20
          # We can set the array itself
          self.my_bar.numbers = [1, 2]
          assert self.my_bar.numbers[0] == 1
          assert self.my_bar.numbers[1] == 2

          # We can change the values of the Point
          self.my_bar.point.x = 1000
          self.my_bar.point.y = 2000
          assert self.my_bar.point.x == 1000
          assert self.my_bar.point.y == 2000
          # We can set the point itself
          self.my_bar.point = Point(x=100, y=200)
          assert self.my_bar.point.x == 100
          assert self.my_bar.point.y == 200

          # We can change the value of the tuple
          self.my_bar.something.item0 = 10
          self.my_bar.something.item1 = false
          assert self.my_bar.something.item0 == 10
          assert not self.my_bar.something.item1
          # We can set the tuple itself
          self.my_bar.something = (1, true)
          assert self.my_bar.something.item0 == 1
          assert self.my_bar.something.item1

          return self.my_bar.name.to_mem()
  ```
  ([#636](https://github.com/ethereum/fe/issues/636))

- Features that read and modify state outside of contracts are now implemented on a struct
  named "Context". `Context` is included in the standard library and can be imported with
  `use std::context::Context`. Instances of `Context` are created by calls to public functions
  that declare it in the signature or by unsafe code.

  Basic example:

  ```
  use std::context::Context

  contract Foo:
      my_num: u256

      pub fn baz(ctx: Context) -> u256:
          return ctx.block_number()

      pub fn bing(self, new_num: u256) -> u256:
          self.my_num = new_num
          return self.my_num


  contract Bar:

      pub fn call_baz(ctx: Context, foo_addr: address) -> u256:
          # future syntax: `let foo = ctx.load<Foo>(foo_addr)`
          let foo: Foo = Foo(ctx, foo_addr)
          return foo.baz()

      pub fn call_bing(ctx: Context) -> u256:
          # future syntax: `let foo = ctx.create<Foo>(0)`
          let foo: Foo = Foo.create(ctx, 0)
          return foo.bing(42)
  ```


  Example with `__call__` and unsafe block:

  ```
  use std::context::Context
  use std::evm

  contract Foo:

      pub fn __call__():
          unsafe:
              # creating an instance of `Context` is unsafe
              let ctx: Context = Context()
              let value: u256 = u256(bar(ctx))

              # return `value`
              evm::mstore(0, value)
              evm::return_mem(0, 32)

      fn bar(ctx: Context) -> address:
          return ctx.self_address()
  ```
  ([#638](https://github.com/ethereum/fe/issues/638))

- # Features

  ## Support local constant

  Example:
  ```python
  contract Foo:
      pub fn bar():
          const LOCAL_CONST: i32 = 1
  ```

  ## Support constant expression

  Example:
  ```python
  const GLOBAL: i32 = 8

  contract Foo:
      pub fn bar():
          const LOCAL: i32 = GLOBAL * 8
  ```

  ## Support constant generics expression

  Example:
  ```python
  const GLOBAL: u256= 8
  const USE_GLOBAL: bool = false
  type MY_ARRAY = Array<i32, { GLOBAL / 4 }>

  contract Foo:
      pub fn bar():
          let my_array: Array<i32, { GLOBAL if USE_GLOBAL else 4 }>
  ```

  # Bug fixes

  ## Fix ICE when constant type is mismatch

  Example:
  ```python
  const GLOBAL: i32 = "FOO"

  contract Foo:
      pub fn bar():
          let FOO: i32 = GLOBAL
  ```

  ## Fix ICE when assigning value to constant twice

  Example:
  ```python
  const BAR: i32 = 1

  contract FOO:
      pub fn bar():
          BAR = 10
  ```
  ([#649](https://github.com/ethereum/fe/issues/649))

- Argument label syntax now uses `:` instead of `=`. Example:

  ```
  struct Foo:
    x: u256
    y: u256

  let x: MyStruct = MyStruct(x: 10, y: 11)
  # previously:     MyStruct(x = 10, y = 11)
  ```
  ([#665](https://github.com/ethereum/fe/issues/665))

- Support module-level `pub` modifier, now default visibility of items in a module is private.

  Example:

  ```python
  # This constant can be used outside of the module.
  pub const PUBLIC:i32 = 1

  # This constant can NOT be used outside of the module.
  const PRIVATE: i32 = 1
  ```
  ([#677](https://github.com/ethereum/fe/issues/677))


### Internal Changes - for Fe Contributors


- - Source files are now managed by a (salsa) `SourceDb`. A `SourceFileId` now corresponds to a salsa-interned `File` with a path. File content is a salsa input function. This is mostly so that the future (LSP) language server can update file content when the user types or saves, which will trigger a re-analysis of anything that changed.
  - An ingot's set of modules and dependencies are also salsa inputs, so that when the user adds/removes a file or dependency, analysis is rerun.
  - Standalone modules (eg a module compiled with `fe fee.fe`) now have a fake ingot parent. Each Ingot has an IngotMode (Lib, Main, StandaloneModule), which is used to disallow `ingot::whatever` paths in standalone modules, and to determine the correct root module file.
  - `parse_module` now always returns an `ast::Module`, and thus a `ModuleId` will always exist for a source file, even if it contains fatal parse errors. If the parsing fails, the body will end with a `ModuleStmt::ParseError` node. The parsing will stop at all but the simplest of syntax errors, but this at least allows partial analysis of source file with bad syntax.
  - `ModuleId::ast(db)` is now a query that parses the module's file on demand, rather than the AST being interned into salsa. This makes handling parse diagnostics cleaner, and removes the up-front parsing of every module at ingot creation time. ([#628](https://github.com/ethereum/fe/issues/628))


## 0.13.0-alpha (2022-01-31)## 0.13.0-alpha (2022-01-31)


### Features


- Support private fields on structs

  Public fields now need to be declared with the `pub` modifier, otherwise they default to private fields.
  If a struct contains private fields it can not be constructed directly except from within the
  struct itself. The recommended way is to implement a method `new(...)` as demonstrated in the
  following example.

  ```
  struct House:
      pub price: u256
      pub size: u256
      vacant: bool

      pub fn new(price: u256, size: u256) -> House
        return House(price=price, size=size, vacant=true)

  contract Manager:

    house: House

    pub fn create_house(price: u256, size: u256):
      self.house = House::new(price, size)
      let can_access_price: u256 = self.house.price
      # can not access `self.house.vacant` because the field is private
  ```
  ([#214](https://github.com/ethereum/fe/issues/214))
- Support non-base type fields in structs

  Support is currently limited in two ways:
    - Structs with complex fields can not be returned from public functions
    - Structs with complex fields can not be stored in storage ([#343](https://github.com/ethereum/fe/issues/343))
- Addresses can now be explicitly cast to u256. For example:

  ```
  fn f(addr: address) -> u256:
    return u256(addr)
  ```
  ([#621](https://github.com/ethereum/fe/issues/621))
- A special function named `__call__` can now be defined in contracts.

  The body of this function will execute in place of the standard dispatcher when the contract is called.

  example (with intrinsics):

  ```
  contract Foo:
      pub fn __call__(self):
          unsafe:
              if __calldataload(0) == 1:
                  __revert(0, 0)
              else:
                  __return(0, 0)
  ```
  ([#622](https://github.com/ethereum/fe/issues/622))


### Bugfixes


- Fixed a crash that happend when using a certain unprintable ASCII char ([#551](https://github.com/ethereum/fe/issues/551))
- The argument to `revert` wasn't being lowered by the compiler,
  meaning that some `revert` calls would cause a compiler panic
  in later stages. For example:

  ```
  const BAD_MOJO: u256 = 0xdeaddead

  struct Error:
    code: u256

  fn fail():
    revert Error(code = BAD_MOJO)
  ```
  ([#619](https://github.com/ethereum/fe/issues/619))
- Fixed a regression where an empty list expression (`[]`) would lead to a compiler crash. ([#623](https://github.com/ethereum/fe/issues/623))
- Fixed a bug where int array elements were not sign extended in their ABI encodings. ([#633](https://github.com/ethereum/fe/issues/633))


## 0.12.0-alpha (2021-12-31)## 0.12.0-alpha (2021-12-31)


### Features


- Added unsafe low-level "intrinsic" functions, that perform raw evm operations.
  For example:

  ```
  fn foo():
    unsafe:
      __mtore(0, 5000)
      assert __mload(0) == 5000
  ```

  The functions available are exactly those defined in yul's "evm dialect":
  https://docs.soliditylang.org/en/v0.8.11/yul.html#evm-dialect
  but with a double-underscore prefix. Eg `selfdestruct` -> `__selfdestruct`.

  These are intended to be used for implementing basic standard library functionality,
  and shouldn't typically be needed in normal contract code.

  Note: some intrinsic functions don't return a value (eg `__log0`); using these
  functions in a context that assumes a return value of unit type (eg `let x: () = __log0(a, b)`)
  will currently result in a compiler panic in the yul compilation phase. ([#603](https://github.com/ethereum/fe/issues/603))
- Added an out of bounds check for accessing array items.
  If an array index is retrieved at an index that is not within
  the bounds of the array it now reverts with `Panic(0x32)`. ([#606](https://github.com/ethereum/fe/issues/606))


### Bugfixes


- Ensure ternary expression short circuit.

  Example:

  ```
  contract Foo:

      pub fn bar(input: u256) -> u256:
          return 1 if input > 5 else revert_me()

      fn revert_me() -> u256:
          revert
          return 0
  ```

  Previous to this change, the code above would **always** revert no matter
  which branch of the ternary expressions it would resolve to. That is because
  both sides were evaluated and then one side was discarded. With this change,
  only the branch that doesn't get picked won't get evaluated at all.

  The same is true for the boolean operations `and` and `or`. ([#488](https://github.com/ethereum/fe/issues/488))


### Internal Changes - for Fe Contributors


- Added a globally available *dummy* std lib.

  This library contains a single `get_42` function, which can be called using `std::get_42()`. Once
  low-level intrinsics have been added to the language, we can delete `get_42` and start adding
  useful code. ([#601](https://github.com/ethereum/fe/issues/601))


## 0.11.0-alpha "Karlite" (2021-12-02)


### Features


- Added support for multi-file inputs.

  **Implementation details:**

  Mostly copied Rust's crate system, but use the the term *ingot* instead of crate.

  Below is an example of an ingot's file tree, as supported by the current implementation.

  ```
  `-- basic_ingot
      `-- src
          |-- bar
          |   `-- baz.fe
          |-- bing.fe
          |-- ding
          |   |-- dang.fe
          |   `-- dong.fe
          `-- main.fe
  ```

  There are still a few features that will be worked on over the coming months:

  - source files accompanying each directory module (e.g. `my_mod.fe`)
  - configuration files and the ability to create library ingots
  - test directories
  - module-level `pub` modifier (all items in a module are public)
  - `mod` statements (all fe files in the input tree are public modules)

  These things will be implemented in order of importance over the next few months. ([#562](https://github.com/ethereum/fe/issues/562))
- The syntax for array types has changed to match other generic types.
  For example, `u8[4]` is now written `Array<u8, 4>`. ([#571](https://github.com/ethereum/fe/issues/571))
- Functions can now be defined on struct types. Example:

  ```
  struct Point:
    x: u64
    y: u64

    # Doesn't take `self`. Callable as `Point.origin()`.
    # Note that the syntax for this will soon be changed to `Point::origin()`.
    pub fn origin() -> Point:
      return Point(x=0, y=0)

    # Takes `self`. Callable on a value of type `Point`.
    pub fn translate(self, x: u64, y: u64):
      self.x += x
      self.y += y

    pub fn add(self, other: Point) -> Point:
      let x: u64 = self.x + other.x
      let y: u64 = self.y + other.y
      return Point(x, y)

    pub fn hash(self) -> u256:
      return keccak256(self.abi_encode())

  pub fn do_pointy_things():
    let p1: Point = Point.origin()
    p1.translate(5, 10)

    let p2: Point = Point(x=1, y=2)
    let p3: Point = p1.add(p2)

    assert p3.x == 6 and p3.y == 12
  ```
  ([#577](https://github.com/ethereum/fe/issues/577))


### Bugfixes


- Fixed a rare compiler crash.

  Example:

  ```
  let my_array: i256[1] = [-1 << 1]
  ```

  Previous to this fix, the given example would lead to an ICE. ([#550](https://github.com/ethereum/fe/issues/550))
- Contracts can now `create` an instance of a contract defined later in a file.
  This issue was caused by a weakness in the way we generated yul. ([#596](https://github.com/ethereum/fe/issues/596))


### Internal Changes - for Fe Contributors


- File IDs are now attached to `Span`s. ([#587](https://github.com/ethereum/fe/issues/587))
- The fe analyzer now builds a dependency graph of source code "items" (functions, contracts, structs, etc).
  This is used in the yulgen phase to determine which items are needed in the yul (intermediate representation)
  output. Note that the yul output is still cluttered with utility functions that may or may not be needed by
  a given contract. These utility functions are defined in the yulgen phase and aren't tracked in the dependency
  graph, so it's not yet possible to filter out the unused functions. We plan to move the definition of many
  of these utility functions into fe; when this happens they'll become part of the dependency graph and will only
  be included in the yul output when needed.

  The dependency graph will also enable future analyzer warnings about unused code. ([#596](https://github.com/ethereum/fe/issues/596))


## 0.10.0-alpha (2021-10-31)


### Features


- Support for module level constants for base types

  Example:

  ```
  const TEN = 10

  contract

    pub fn do_moon_math(self) -> u256:
      return 4711 * TEN
  ```

  The values of base type constants are always inlined. ([#192](https://github.com/ethereum/fe/issues/192))
- Encode revert errors for ABI decoding as `Error(0x103)` not `Panic(0x99)` ([#492](https://github.com/ethereum/fe/issues/492))
- Replaced `import` statements with `use` statements.

  Example:

  ```
  use foo::{bar::*, baz as baz26}
  ```

  Note: this only adds support for parsing `use` statements. ([#547](https://github.com/ethereum/fe/issues/547))
- Functions can no be defined outside of contracts. Example:

  ```
  fn add_bonus(x: u256) -> u256:
      return x + 10

  contract PointTracker:
      points: Map<address, u256>

      pub fn add_points(self, user: address, val: u256):
          self.points[user] += add_bonus(val)
  ```
  ([#566](https://github.com/ethereum/fe/issues/566))
- Implemented a `send_value(to: address, value_in_wei: u256)` function.

  The function is similar to the [`sendValue` function by OpenZeppelin](https://github.com/OpenZeppelin/openzeppelin-contracts/blob/5b28259dacf47fc208e03611eb3ba8eeaed63cc0/contracts/utils/Address.sol#L54-L59) with the differences being that:

  1. It reverts with `Error(0x100)` instead of `Error("Address: insufficient balance")` to
  safe more gas.

  2. It uses `selfbalance()` instead of `balance(address())` to safe more gas

  3. It reverts with `Error(0x101)` instead of `Error("Address: unable to send value, recipient may have reverted")` also to safe more gas. ([#567](https://github.com/ethereum/fe/issues/567))
- Added support for `unsafe` functions and `unsafe` blocks within functions.
  Note that there's currently no functionality within Fe that requires the use
  of `unsafe`, but we plan to add built-in `unsafe` functions that perform raw
  evm operations which will only callable within an `unsafe` block or function. ([#569](https://github.com/ethereum/fe/issues/569))
- Added `balance()` and `balance_of(account: address)` methods. ([#572](https://github.com/ethereum/fe/issues/572))
- Added support for explicit casting between numeric types.

  Example:

  ```
  let a: i8 = i8(-1)
  let a1: i16 = i16(a)
  let a2: u16 = u16(a1)

  assert a2 == u16(65535)

  let b: i8 = i8(-1)
  let b1: u8 = u8(b)
  let b2: u16 = u16(b1)

  assert b2 == u16(255)
  ```

  Notice that Fe allows casting between any two numeric types but does not allow
  to change both the sign and the size of the type in one step as that would leave
  room for ambiguity as the example above demonstrates. ([#576](https://github.com/ethereum/fe/issues/576))


### Bugfixes


- Adjust numeric values loaded from memory or storage

  Previous to this fix numeric values that were loaded from either memory or storage
  were not properly loaded on the stack which could result in numeric values not
  treated as intended.

  Example:

  ```
  contract Foo:

      pub fn bar() -> i8:
          let in_memory: i8[1] = [-3]
          return in_memory[0]
  ```

  In the example above `bar()` would not return `-3` but `253` instead. ([#524](https://github.com/ethereum/fe/issues/524))
- Propagate reverts from external contract calls.

  Before this fix the following code to `should_revert()` or `should_revert2()`
  would succeed even though it clearly should not.

  ```
  contract A:
    contract_b: B
    pub fn __init__(contract_b: address):
      self.contract_b = B(contract_b)

    pub fn should_revert():
      self.contract_b.fail()

    pub fn should_revert2():
      self.contract_b.fail_with_custom_error()

  struct SomeError:
    pass

  contract B:

    pub fn fail():
      revert

    pub fn fail_with_custom_error():
      revert SomeError()
  ```

  With this fix the revert errors are properly passed upwards the call hierachy. ([#574](https://github.com/ethereum/fe/issues/574))
- Fixed bug in left shift operation.

  Example:

  Let's consider the value `1` as an `u8` which is represented as
  the following 256 bit item on the EVM stack `00..|00000001|`.
  A left shift of `8` bits (`val << 8`) turns that into `00..01|00000000|`.

  Previous to this fix this resulted in the compiler taking `256` as the
  value for the `u8` when clearly `256` is not even in the range of `u8`
  anymore. With this fix the left shift operations was fixed to properly
  "clean up" the result of the shift so that `00..01|00000000|` turns into
  `00..00|00000000|`. ([#575](https://github.com/ethereum/fe/issues/575))
- Ensure negation is checked and reverts with over/underflow if needed.

  Example:

  The minimum value for an `i8` is `-128` but the maximum value of an `i8`
  is `127` which means that negating `-128` should lead to an overflow since
  `128` does not fit into an `i8`. Before this fix, negation operations where
  not checked for over/underflow resulting in returning the oversized value. ([#578](https://github.com/ethereum/fe/issues/578))


### Internal Changes - for Fe Contributors


- In the analysis stage, all name resolution (of variable names, function names,
  type names, etc used in code) now happens via a single `resolve_name` pathway,
  so we can catch more cases of name collisions and log more helpful error messages. ([#555](https://github.com/ethereum/fe/issues/555))
- Added a new category of tests: differential contract testing.

  Each of these tests is pased on a pair of contracts where one implementation
  is written in Fe and the other one is written in Solidity. The implementations
  should have the same public APIs and are assumed to always return identical
  results given equal inputs. The inputs are randomly generated using `proptest`
  and hence are expected to discover unknown bugs. ([#578](https://github.com/ethereum/fe/issues/578))


## 0.9.0-alpha (2021-09-29)


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
  ```
  ([#520](https://github.com/ethereum/fe/issues/520))
- The analyzer now disallows defining a type, variable, or function whose
  name conflicts with a built-in type, function, or object.

  Example:

  ```
  error: type name conflicts with built-in type
  ┌─ compile_errors/shadow_builtin_type.fe:1:6
  │
  1 │ type u256 = u8
  │      ^^^^ `u256` is a built-in type
  ```
  ([#539](https://github.com/ethereum/fe/issues/539))


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


- Created a landing page for Fe at https://fe-lang.org ([#394](https://github.com/ethereum/fe/issues/394))
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

  Example, the following code is now rightfully rejected because it tries to create an
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
  # Would both overwrite output files and run the Yul optimizer.
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
- Added a builtin function `abi_encode()` that can be used to encode structs. The return type is a
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
