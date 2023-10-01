# Context

`Context` is used frequently in Fe smart contracts. It is used to gate access to EVM functions for reading and modifyng the blockchain.

## Rationale

Smart contracts execute on the Ethereum Virtual Machine (EVM). The EVM exposes features that allow smart contracts to query or change some of the blockchain data, for example emitting logs that are included in transaction receipts, creating contracts, obtaining the current block number and altering the data stored at certain addresses.

To make Fe maximally explicit and as easy as possible to audit, these functions are gated behind a `Context` object. This is passed as an argument to functions, making it clear whether a function interacts with EVM features from the function signature alone. 

For example, the following function looks pure from its signature (i.e. it is not expected to alter any blockchain data) but in reality it does modify the blockchain (by emitting a log).

```rust
struct SomeEvent{
}

pub fn looks_pure_but_isnt() {
  emit(SomeEvent())
}
```

Using `Context` to control access to EVM functions such as `emit` solves this problem by requiring an instance of `Context` to be passed explicitly to the function, making it clear from the function signature that the function executes some blockchain interaction. The function above, rewritten using `Context`, looks as follows:

```fe
struct SomeEvent {
}

pub fn uses_context(mut ctx: Context) {
    ctx.emit(SomeEvent())
}
```

## The Context object

The Context object gates access to features such as:

- emitting logs
- creating contracts
- transferring ether
- reading message info
- reading block info

The `Context` object needs to be passed as a parameter to the function. The `Context` object has a defined location in the parameter list. It is the *first* parameter unless the function also takes `self`. `Context` or `self` appearing at any other position in the parameter list causes a compile time error.

The Context object is automatically injected when a function is called externally but it has to be passed explicitly when the function is called from another Fe function e.g.

```fe
// The context object is automatically injected when this is called externally
pub fn multiply_block_number(ctx: Context) -> u256 {
  // but it has to be passed along in this function call
  return retrieves_blocknumber(ctx) * 1000
}

fn retrieves_blocknumber(ctx: Context) -> u256 {
  return ctx.block_number()
}
```

## Context mutability

All functionality that modifies the blockchain such as creating logs or contracts or transferring ether would require a mutable `Context` reference whereas read-only access such as `ctx.blocknumber()` does not need require a mutable reference to the context. To pass a mutable `Context` object, prepend the object name with `mut` in the function definition, e.g.:

```fe
struct SomeEvent{
}

pub fn mutable(mut ctx: Context) {
    ctx.emit(SomeEvent())
}
```


## ABI conformity

The use of `Context` enables tighter rules and extra clarity compared wth the existing function categories in the ABI, especially when paired with [`self`](self.md). The following table shows how combinations of `self`, `mut self`, `Context` and `mut Context` map to ABI function types.

| Category                          | Characteristics                                                                                                                                                                 | Fe Syntax                         | ABI                   |
| --------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | --------------------------------- | --------------------- |
| Pure                              | Can only operate on input arguments and not produce any information besides its return value. Can not take self and therefore has no access to things that would make it impure | `foo(val: u256)`                  | pure                  |
| Read Contract                     | Reading information from the contract instance (broad definition includes reading constants from contract code)                                                                 | `foo(self)`                       | view                  |
| Storage Writing                   | Writing to contract storage (own or that of other contracts)                                                                                                                    | `foo(mut self)`                   | payable or nonpayable |
| Context Reading                   | Reading contextual information from the blockchain (`msg`, `block` etc)                                                                                                         | `foo(ctx: Context)`               | view                  |
| Context Modifying                 | Emitting logs, transferring ether, creating contracts                                                                                                                           | `foo(ctx: mut Context)`           | payable or nonpayable |
| Read Contract & `Context`         | Reading information from the contract instance and `Context`                                                                                                                    | `foo(self, ctx:Context)`          | view                  |
| Read Contract & write `Context`   | Reading information from the contract instance and modify `Context`                                                                                                             | `foo(self, ctx: mut Context)`     | view                  |
| Storage Writing & read `Context`  | Writing to contract storage and read from `Context`                                                                                                                             | `foo(mut self, ctx: Context)`     | payable or nonpayable |
| Storage Writing & write `Context` | Writing to contract storage and `Context`                                                                                                                                       | `foo(mut self, ctx: mut Context)` | payable or nonpayable |

This means Fe has nine different categories of function that can be derived from the function signatures that map to four different ABI types.


## Examples

### msg_sender and msg_value

`Context` includes information about inbound transactions. For example, the following function receives ether and adds the sender's address and the
transaction value to a mapping. No blockchain data is being changed, so `Context` does not need to be mutable.


```rust
// assumes existence of state variable named 'ledger' with type Map<address, u256>
pub fn add_to_ledger(mut self, ctx: Context) {
    self.ledger[ctx.msg_sender()] = ctx.msg_value();
}
```


### Transferring ether

Transferring ether modifies the blockchain state, so it requires access to a mutable `Context` object.

```fe
pub fn send_ether(mut ctx: Context, _addr: address, amount: u256) {
    ctx.send_value(to: _addr, wei: amount)
}
```

### create/create2

Creating a contract via `create`/`create2` requires access to a mutable `Context` object because it modifies the blockchain state data:

```rust
pub fn creates_contract(ctx: mut Context):
  ctx.create2(...)
```

### block number 

Reading block chain information such as the current block number requires `Context` (but does not require it to be mutable). 

```fe
pub fn retrieves_blocknumber(ctx: Context) {
  ctx.block_number()
}
```
