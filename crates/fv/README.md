# Fe Fv

This crate contains utilities for building and running kevm specs.

## kevm setup

A system installation of [evm-semantics](https://github.com/fe-lang/evm-semantics) is required to run the specs. Please clone the repository and follow the installation directions.

While in the `evm-semantics` project, `kompile` the Fe verification module:

```commandline
$ export PATH=$PATH:/path/to/evm-semantics/.build/usr/bin
$ kevm kompile --backend haskell tests/specs/fe/verification.k                                  \
    --directory tests/specs/fe/verification/haskell                                             \
    --main-module VERIFICATION                                                                  \
    --syntax-module VERIFICATION                                                                \
    --concrete-rules-file tests/specs/fe/concrete-rules.txt                                     \
    -I /usr/lib/kevm/include/kframework -I /usr/lib/kevm/blockchain-k-plugin/include/kframework
```

## running the proofs

Once the evm-semantics project has been built, set the `KEVM_PATH` environment variable:

```commandline
$ export KEVM_PATH=/path/to/evm-semantics
```

and then run the tests: 

```commandline
$ cargo test --features "solc-backend, kevm-backend"
```

*Note: If you are working on a resource constrained device, you may not be able to run all tests simultaneously. If issues are encountered, run tests in smaller groups.*

e.g.

```commandline
$ cargo test sanity_returns_42 --features "solc-backend, kevm-backend"
```
