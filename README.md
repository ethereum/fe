
<img src="https://raw.githubusercontent.com/ethereum/fe/master/logo/fe_svg/fe_source.svg" width="150px">

Fe is an emerging smart contract language for the Ethereum blockchain.

[![Build Status](https://github.com/ethereum/fe/workflows/CI/badge.svg)](https://github.com/ethereum/fe/actions)
[![Coverage](https://codecov.io/gh/ethereum/fe/branch/master/graph/badge.svg)](https://codecov.io/gh/ethereum/fe)


NOTE: **The larger part of the `master` branch will be replaced with the brand-new implementation, which is currently under development in the [fe-v2](https://github.com/ethereum/fe/tree/fe-v2) branch. Please refer to the branch if you kindly contribute to Fe**

## Overview

Fe is a statically typed language for the Ethereum Virtual Machine (EVM). It is inspired by Python and Rust which makes it easy to learn -- especially for new developers entering the Ethereum ecosystem.

## Features & Goals

* Bounds and overflow checking
* Decidability by limitation of dynamic program behavior
* More precise gas estimation (as a consequence of decidability)
* Static typing
* Pure function support
* Restrictions on reentrancy
* Static looping
* Module imports
* Standard library
* Usage of [YUL](https://docs.soliditylang.org/en/latest/yul.html) IR to target both EVM and eWASM
* WASM compiler binaries for enhanced portability and in-browser compilation of
  Fe contracts
* Implementation in a powerful, systems-oriented language (Rust) with strong safety guarantees to reduce risk of compiler bugs

Additional information about design goals and background can be found in the [official announcement](https://snakecharmers.ethereum.org/fe-a-new-language-for-the-ethereum-ecosystem/).

## Language Specification

We aim to provide a full language specification that should eventually be used to formally verify the correctness of the compiler. A work in progress draft of the specification can be found [here](http://fe-lang.org/docs/spec/index.html).

## Progress

Fe development is still in its early stages. We have a basic [Roadmap for 2021](https://notes.ethereum.org/LVhaTF30SJOpkbG1iVw1jg) that we want to follow. We generally try to drive the development by working through real world use cases. Our next goal is to provide a working Uniswap implementation in Fe which will help us to advance and form the language.

Fe had its first alpha release January 2021 and is now following a monthly release cycle.

## Getting started

- [Build the compiler](https://github.com/ethereum/fe/blob/master/docs/src/development/build.md)
- [Or download the binary release](https://github.com/ethereum/fe/releases)

To compile Fe code:

1. Run `fe path/to/fe_source.fe`
2. Fe creates a directory `output` in the current working directory that contains the compiled binary and abi.

Run `fe --help` to explore further options.

## Examples

The following is a simple contract implemented in Fe.

```fe
struct Signed {
    pub book_msg: String<100>
}

contract GuestBook {
    messages: Map<address, String<100>>

    pub fn sign(mut self, mut ctx: Context, book_msg: String<100>) {
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
