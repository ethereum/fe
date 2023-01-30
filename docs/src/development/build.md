## Build and test

Please make sure Rust is [installed](https://www.rust-lang.org/learn/get-started).

**Basic**

The following commands only build the Fe -> Yul compiler components.

- build the CLI: `cargo build`
- test: `cargo test --workspace`

**Full**

The Fe compiler depends on the Solidity compiler for transforming Yul IR to EVM bytecode. We currently use [solc-rust](https://github.com/cburgdorf/solc-rust) to perform this. In order to compile solc-rust, the following must be installed on your system:

- cmake
- libboost@1.79 (This is temporary and will be updated when solidity v0.8.18 is released)
- libclang

For macOS, you can install boost@1.79 dependency using the below command:
```bash
    brew tap Y-Nak/boost
    brew install Y-nak/boost/boost
```

Once these have been installed, you may run the full build. This is enabled using the *solc-backend* feature.

- build the CLI: `cargo build --features solc-backend`
- test: `cargo test --workspace --features solc-backend`