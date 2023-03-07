
<img src="https://raw.githubusercontent.com/ethereum/fe/master/logo/fe_svg/fe_source.svg" width="150px">

Fe is a high-level smart contract language for the Ethereum blockchain.

[![Build Status](https://github.com/ethereum/fe/workflows/CI/badge.svg)](https://github.com/ethereum/fe/actions)
[![Coverage](https://codecov.io/gh/ethereum/fe/branch/master/graph/badge.svg)](https://codecov.io/gh/ethereum/fe)


NOTE: **The larger part of the `master` branch will be replaced with the brand-new implementation, which is currently under development in the [fe-v2](https://github.com/ethereum/fe/tree/fe-v2) branch. Please refer to the branch if you kindly contribute to Fe**

## Overview

Fe is statically typed and designed for writing smart contracts. It is implemented in Rust and generates EVM bytecode.

## Language Features & Goals

* Heavily static
* Safe by default 
* Ergonomic
* Powerful
* Designed for Ethereum 

## Compiler/Tooling Features & Goals

* Efficient and correct code generation
* Compiler interactiveness and IDE support
* Testing that enables thorough validation of contracts and libraries
* Wasm support

## Language Specification

We aim to provide a full language specification that should eventually be used to formally verify the correctness of the compiler. A work in progress draft of the specification can be found [here](http://fe-lang.org/docs/spec/index.html).

## Progress (as of March 2023)

**Supported language features**
- Enums and match statements
- Module system
- Traits and generics 

**Bountiful**

We have launched a non-traditional bug bounty platform to help validate the Fe compiler. You can read more [here](https://blog.fe-lang.org/posts/bountiful-break-things-and-get-paid/). 

## Getting started

- [Build the compiler](https://github.com/ethereum/fe/blob/master/docs/src/development/build.md)
- [Or download the binary release](https://github.com/ethereum/fe/releases)

To compile Fe code:

1. Run `fe path/to/fe_source.fe`
2. Fe creates a directory `output` in the current working directory that contains the compiled binary and abi.

Run `fe --help` to explore further options.

## Examples

The following is a simple contract implemented in Fe.

```rust
struct Signed {
    book_msg: String<100>
}

contract GuestBook {
    messages: Map<address, String<100>>

    pub fn sign(self, ctx: Context, book_msg: String<100>) {
        self.messages[ctx.msg_sender()] = book_msg
        ctx.emit(Signed(book_msg: book_msg))
    }

    pub fn get_msg(self, addr: address) -> String<100> {
        return self.messages[addr].to_mem()
    }
}
```

A lot more working examples can be found in our [test fixtures directory](https://github.com/ethereum/fe/tree/master/crates/test-files/fixtures/demos).

The most advanced example that we can provide at this point is an implementation of the [Uniswap-V2 core contracts](https://github.com/ethereum/fe/blob/master/crates/test-files/fixtures/demos/uniswap.fe).

## Community

- Twitter: [@official_fe](https://twitter.com/official_fe)
- Chat: [Discord](https://discord.gg/ywpkAXFjZH)


## License

The Fe implementation is split into several crates. Crates that depend on the
solidity compiler (directly or indirectly) are licensed GPL-3.0-or-later. This
includes the `fe` CLI tool, yulc, driver, tests, and test-utils.

The remaining crates are licensed Apache-2.0. This includes the parser,
analyzer, mir, abi, and common.
