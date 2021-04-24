
<img src="https://raw.githubusercontent.com/ethereum/fe/master/logo/fe_svg/fe_source.svg" width="150px">

Fe is an emerging smart contract language for the Ethereum blockchain.

[![Build Status](https://github.com/ethereum/fe/workflows/CI/badge.svg)](https://github.com/ethereum/fe/actions)
[![Coverage](https://codecov.io/gh/ethereum/fe/branch/master/graph/badge.svg)](https://codecov.io/gh/ethereum/fe)


## Overview

Fe is a statically typed language for the Ethereum Virtual Machine (EVM). It is inspired by Python and Rust and is easy to learn -- even for developers who are new to the Ethereum ecosystem.

## Features & Goals

* Bounds and overflow checking
* Decidability by limitation of dynamic program behavior
* More precise gas estimation (as a consequence of decidability)
* Static typing
* Pure function support
* Binary fixed-point math
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

We aim to provide a full language specification that should eventually be used to formally verify the correctness of the compiler. A work in progress draft of the specification can be found [here](https://github.com/ethereum/fe/blob/master/spec/index.md).

## Progress

Fe development is still in its early stages. We have a basic [Roadmap for 2021](https://notes.ethereum.org/LVhaTF30SJOpkbG1iVw1jg) that we want to follow. We generally try to drive the development by working through real world use cases. Our next goal is to provide a working Uniswap implementation in Fe which will help us to advance and form the language.

Fe had its first alpha release January 2021 and is now following a monthly release cycle.

## Getting started

- [Build the compiler](https://github.com/ethereum/fe/blob/master/docs/build.md)
- [Or download the binary release](https://github.com/ethereum/fe/releases)

To compile Fe code:

1. Run `fe path/to/fe_source.fe`
2. Fe creates a directory `output` in the current working directory that contains the compiled binary and abi.

Run `fe --help` to explore further options.

## Examples

The following is a simple contract implemented in Fe.

```fe
type BookMsg = bytes[100]

contract GuestBook:
    pub guest_book: map<address, BookMsg>

    event Signed:
        book_msg: BookMsg

    pub def sign(book_msg: BookMsg):
        self.guest_book[msg.sender] = book_msg

        emit Signed(book_msg=book_msg)

    pub def get_msg(addr: address) -> BookMsg:
        return self.guest_book[addr].to_mem()
```

A lot more working examples can be found in our [test fixtures directory](https://github.com/ethereum/fe/tree/master/compiler/tests/fixtures).

The most advanced example that we can provide at this point is a fully working [ERC20 implementation](https://github.com/ethereum/fe/blob/master/compiler/tests/fixtures/demos/erc20_token.fe).

## Community

- Twitter: [@official_fe](https://twitter.com/official_fe)
- Chat: [Discord](https://discord.gg/ywpkAXFjZH)


License: Apache-2.0
