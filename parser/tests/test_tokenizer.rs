extern crate difference;
extern crate pyo3;
extern crate vyper_parser;

use std::fmt;

use pyo3::prelude::*;
use pyo3::types::PyBytes;

use vyper_parser::tokenizer::*;

/// Return the lines of text in the string `lines` prefixed with the prefix in
/// the string `prefix`.
fn prefix_lines(prefix: &str, lines: &str) -> String {
    lines
        .lines()
        .map(|i| [prefix, i].concat())
        .collect::<Vec<String>>()
        .join("\n")
}

/// Wrapper struct for formatting changesets from the `difference` package.
pub struct Diff(difference::Changeset);

impl Diff {
    pub fn new(left: &str, right: &str) -> Self {
        Self(difference::Changeset::new(left, right, "\n"))
    }
}

impl fmt::Display for Diff {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for d in &self.0.diffs {
            match *d {
                difference::Difference::Same(ref x) => {
                    write!(f, "{}{}", prefix_lines(" ", x), self.0.split)?;
                }
                difference::Difference::Add(ref x) => {
                    write!(f, "\x1b[92m{}\x1b[0m{}", prefix_lines("+", x), self.0.split)?;
                }
                difference::Difference::Rem(ref x) => {
                    write!(f, "\x1b[91m{}\x1b[0m{}", prefix_lines("-", x), self.0.split)?;
                }
            }
        }
        Ok(())
    }
}

/// Compare the given strings and panic when not equal with a colorized line
/// diff.
macro_rules! assert_strings_eq {
    ($left:expr , $right:expr,) => ({
        assert_strings_eq!($left, $right)
    });
    ($left:expr , $right:expr) => ({
        match (&($left), &($right)) {
            (left_val, right_val) => {
                if *left_val != *right_val {
                    panic!(
                        "assertion failed: `(left == right)`\ndiff:\n{}",
                        Diff::new(left_val, right_val),
                    )
                }
            }
        }
    });
    ($left:expr , $right:expr, $($arg:tt)*) => ({
        match (&($left), &($right)) {
            (left_val, right_val) => {
                if *left_val != *right_val {
                    panic!(
                        "assertion failed: `(left == right)`: {}\ndiff:\n{}",
                        format_args!($($arg)*),
                        Diff::new(left_val, right_val),
                    )
                }
            }
        }
    });
}

pub struct TokenHelpers<'a> {
    py: Python<'a>,
    module: &'a PyModule,
}

impl<'a> TokenHelpers<'a> {
    fn new(py: Python<'a>) -> Self {
        let result = PyModule::from_code(
            py,
            include_str!("token_helpers.py"),
            "token_helpers.py",
            "token_helpers",
        );

        match result {
            Err(e) => {
                e.print(py);
                panic!("Python exception when loading token_helpers.py");
            }
            Ok(module) => Self { py, module },
        }
    }

    fn get_token_json(&self, source: &str) -> String {
        let bytes = PyBytes::new(self.py, source.as_bytes());
        let result = self.module.call("get_token_json", (bytes,), None);

        match result {
            Err(e) => {
                e.print(self.py);
                panic!("Python exception when calling get_token_json");
            }
            Ok(any) => match any.extract() {
                Err(e) => {
                    e.print(self.py);
                    panic!("Python exception when converting result to string");
                }
                Ok(string) => string,
            },
        }
    }
}

fn get_rust_token_json(input: &str) -> String {
    let tokens = tokenize(input).unwrap();
    let serialized = serde_json::to_string_pretty(&tokens).unwrap();
    serialized
}

#[test]
fn test_tokenize() {
    let fixtures = &[
        ("basic.py", include_str!("fixtures/tokenizer/basic.py")),
        (
            "triple_quote_strings.py",
            include_str!("fixtures/tokenizer/triple_quote_strings.py"),
        ),
        (
            "single_quote_strings.py",
            include_str!("fixtures/tokenizer/single_quote_strings.py"),
        ),
        (
            "continued_statements.py",
            include_str!("fixtures/tokenizer/continued_statements.py"),
        ),
        (
            "validator_registration.v.py",
            include_str!("fixtures/tokenizer/validator_registration.v.py"),
        ),
        (
            "tokenize.py",
            include_str!("fixtures/tokenizer/tokenize.py"),
        ),
    ];

    // Load python token helpers
    let gil = Python::acquire_gil();
    let py = gil.python();
    let token_helpers = TokenHelpers::new(py);

    for (filename, content) in fixtures {
        let expected = token_helpers.get_token_json(content);
        let actual = get_rust_token_json(content);

        assert_strings_eq!(
            expected,
            actual,
            "Tokenizations didn't match for {}",
            filename
        );
    }
}
