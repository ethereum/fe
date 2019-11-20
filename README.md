# Rust Vyper

## Overview

Rust Vyper is a next generation version of the Vyper smart contract language.
Rust Vyper aims to achieve the goals of the [existing Vyper
project](https://github.com/vyperlang/vyper) along with the following
additional goals:

* Emphasis on correct implementation via use of better tooling, application of
  formal methods, and stricter development practices
* A complete language specification to aid in verification of compiler
  correctness
* Support for both eWASM and EVM code generation via use of the [YUL IR
  format](https://solidity.readthedocs.io/en/v0.5.13/yul.html)
* Implementation in a powerful, systems-oriented language (Rust) with strong
  safety guarantees
* Syntactic improvements and additional code re-use features
* Statically built compiler binaries for smoother installation process
* WASM compiler binaries for enhanced portability and in-browser compilation of
  Vyper contracts

## Inherited goals

Rust Vyper aims to achieve most of the goals of the [existing Vyper
project](https://github.com/vyperlang/vyper).  Those goals are [described in
that project's
documentation](https://vyper.readthedocs.io/en/latest/index.html#principles-and-goals)
though we include a summary of them here:

* Bounds and overflow checking
* Decidability by limitation of dynamic program behavior
* More precise gas estimation (as a consequence of decidability)
* Static typing
* Pure function support
* Binary fixed-point math
* Restrictions on reentrancy
* Static looping

The following is a list of goals or aspects of the existing Vyper project that
we *do not necessarily* wish to duplicate:

* Lack of code re-use features such as:
  - Modifiers (re-usable assertions)
  - Polymorphism (classes or interfaces)
  - Module imports
* Strict enforcement of limits on dynamic behavior -- Though we aim to provide
  Vyper's decidability by default, we believe it may also be useful to allow
  disabling of decidability features if the expressivity trade-off doesn't make
  sense for certain projects.

## Additional goals

### Stricter development practices

We wish to promote the following development practices:

* Thorough unit testing of *all* compiler components, from lexing and parsing
  routines to type checking and compilation routines.
* Thorough integration testing of combined components to identify issues with
  abstraction boundaries.
* Property-based testing or fuzz testing of components where appropriate.  See
  [QuickCheck](https://github.com/nick8325/quickcheck) and
  [hypothesis](https://hypothesis.works/).
* The above testing goals should, at a minimum, give 100% code coverage but
  should also redundantly test the same code sections in different modes of
  use.
* Separation of concerns -- Components responsible for different stages of
  compilation (parsing, type checking, compilation, etc.) should be clearly
  separated into different libraries with distinct APIs.
* Maintainability -- Code should be written with future contributors in mind.
  Complex features should not only be automatically tested but also described
  in developer documentation.  Code should be DRY and should include
  abstractions that naturally encapsulate specific tasks.

### Support for eWASM and EVM and the use of YUL

We wish to leverage the YUL IR format as a means of targeting both the eWASM
and EVM platforms.  YUL is actively developed by the Solidity community and we
see a great opportunity for synergy with the Solidity project through shared
use of YUL.

### Use of Rust as an implementation language

The Rust programming language has been a rising star in the PL community for a
number of years.  We believe it provides a reasonable balance between the
following factors:

* Type and memory safety
* Ease of use
* Expressivity
* Size and quality of developer community
* Availability of modern language idioms such as iterators, functional
  programming features, etc.
* Support from established organizations such as Mozilla

The Rust project and Mozilla foundation are also some of the primary drivers
behind the WASM project, which will likely end up playing a central role in the
Ethereum community.  The Rust compiler is one of the premier compilers to
target the WASM platform.

The use of a compiled language in general, and Rust in particular, will
facilitate improvements in the compiler release process.  New compiler versions
could be offered as statically compiled binaries (removing dependence on
existing Python binaries) or as WASM binaries (opening up the possibility to
run Rust Vyper in-browser or, in the distant future, on-chain).

### Language specification and formal methods

We wish to provide a complete language specification with the following parts:

* Syntax specification in extended BNF format similar to the [Python grammar
  specification](https://docs.python.org/3/reference/grammar.html).
* An [operational, small-step
  semantics](https://en.wikipedia.org/wiki/Operational_semantics#Small-step_semantics)
  to describe correct program evaluation and behavior.
* Sections of informal prose to aid in understanding of the grammar, semantics,
  their relationship, and the context in which they operate.

A secondary goal to providing a specification document could be to also provide
an executable specification via tools such as the [K
framework](http://www.kframework.org/index.php/Main_Page).  Providing such an
executable specification may become a primary goal if it can be achieved
without too much effort and without hampering the achievement of other goals.

Through the application of rigorous development practices and the availability
of a complete language specification, we hope to apply formal verification
methods to the Rust Vyper compiler and also to programs generated by it.
