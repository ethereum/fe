//! A Rust port of [Python's std lib `tokenize`
//! module](https://github.com/python/cpython/blob/2a58b0636d1f620f8a85a2e4c030cc10551936a5/Lib/tokenize.py).
//!
//! ## Usage
//!
//! ```rust
//! use fe_parser::tokenizer::tokenize;
//!
//! let source_string = r#"
//! class Foo:
//!     bar = "baz"
//! "#;
//! let token_vector = tokenize(source_string).unwrap();
//! ```
//!
//! ## Differences/similarities with python's `tokenize` module
//!
//! * The [`self::tokenize::tokenize`] function generates all token types
//!   produced by python's `tokenize.tokenize` method *except* for an initial
//!   `ENCODING` token.  Source files are always assumed to be encoded in utf-8
//!   so this token is redundant.
//! * The [`self::types::Token`] struct, which we use to represent parsed
//!   tokens, includes a single `span` field containing the byte offsets
//!   representing the beginning and exclusive end of the token in a source
//!   string. This differs from Python's `TokenInfo` instances which use
//!   line/column tuples to represent the beginning and ending positions of a
//!   token.
//! * The [`self::tokenize::tokenize`] function has no streaming behavior and
//!   accepts a reference to an entire source string available in memory.
//!   Python's `tokenize.tokenize` function accepts a reference to a function
//!   that progressively yields lines of text from a source file.
//!
//! As Python's `tokenize` module's implementation is pretty ugly, so is the
//! implementation of [`self::tokenize::tokenize`].  It may be a candidate for
//! later cleanup.  However, it may also be fine as is assuming it doesn't need
//! to be modified often.

mod regex;
pub mod tokenize;
pub mod types;
pub mod wasm;

pub use self::tokenize::{
    tokenize,
    TokenizeError,
};
pub use self::types::{
    Token,
    TokenType,
};
