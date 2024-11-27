
<img src="https://raw.githubusercontent.com/ethereum/fe/master/logo/fe_svg/fe_source.svg" width="150px">

Fe is an emerging smart contract language for the Ethereum blockchain.

[![Build Status](https://github.com/ethereum/fe/workflows/CI/badge.svg)](https://github.com/ethereum/fe/actions)
[![Coverage](https://codecov.io/gh/ethereum/fe/branch/master/graph/badge.svg)](https://codecov.io/gh/ethereum/fe)

NOTE: **Most of the `master` branch will be replaced with a new implementation, which is currently under development in the [fe-v2](https://github.com/ethereum/fe/tree/fe-v2) branch. Please refer to the branch if you kindly contribute to Fe**

## Overview

Fe is a statically typed language for the Ethereum Virtual Machine (EVM). The type system is similar to rust's, with the addition of higher-kinded types (on the `fe-v2` branch). We're exploring additional type system, syntax, and semantic changes. Please note that the current docs reflect the status of the language on the `master` branch.

## Progress

Fe has had several early releases which can be used today, though these releases are lacking some features and don't reflect our complete vision for the language. The current focus is on finishing the `fe-v2` branch, and development of the [sonatina](https://github.com/fe-lang/sonatina) compiler backend which Fe will use.

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

More examples can be found in our [test fixtures directory](https://github.com/ethereum/fe/tree/master/crates/test-files/fixtures/demos).

The most advanced example that we can provide at this point is an implementation of the [Uniswap-V2 core contracts](https://github.com/ethereum/fe/blob/master/crates/test-files/fixtures/demos/uniswap.fe).

## Community

- Twitter: [@official_fe](https://twitter.com/official_fe)
- Chat:
  - We've recently moved to [Zulip](https://fe-lang.zulipchat.com/join/dqvssgylulrmjmp2dx7vcbrq/)
  - The [Discord](https://discord.gg/ywpkAXFjZH) server is still live, but our preference is zulip.

## License

The Fe implementation is split into several crates. Crates that depend on the
solidity compiler (directly or indirectly) are licensed GPL-3.0-or-later. This
includes the `fe` CLI tool, yulc, driver, tests, and test-utils.

The remaining crates are licensed Apache-2.0. This includes the parser,
analyzer, mir, abi, and common.
