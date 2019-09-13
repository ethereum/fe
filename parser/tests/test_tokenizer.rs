extern crate difference;
extern crate pyo3;
extern crate rust_vyper_parser;

use std::fmt;

use difference::{Changeset, Difference};
use pyo3::prelude::*;
use pyo3::types::PyBytes;
use rust_vyper_parser::tokenizer::*;

pub struct Diff(Changeset);

impl Diff {
    pub fn new(left: &str, right: &str) -> Self {
        Self(Changeset::new(left, right, "\n"))
    }
}

impl fmt::Display for Diff {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for d in &self.0.diffs {
            match *d {
                Difference::Same(ref x) => {
                    let lines = x
                        .lines()
                        .map(|i| [" ", i].concat())
                        .collect::<Vec<String>>()
                        .join("\n");

                    write!(f, "{}{}", lines, self.0.split)?;
                }
                Difference::Add(ref x) => {
                    let lines = x
                        .lines()
                        .map(|i| ["+", i].concat())
                        .collect::<Vec<String>>()
                        .join("\n");

                    write!(f, "\x1b[92m{}\x1b[0m{}", lines, self.0.split)?;
                }
                Difference::Rem(ref x) => {
                    let lines = x
                        .lines()
                        .map(|i| ["-", i].concat())
                        .collect::<Vec<String>>()
                        .join("\n");

                    write!(f, "\x1b[91m{}\x1b[0m{}", lines, self.0.split)?;
                }
            }
        }
        Ok(())
    }
}

macro_rules! assert_strings_eq {
    ($left:expr , $right:expr,) => ({
        assert_eq!($left, $right)
    });
    ($left:expr , $right:expr) => ({
        match (&($left), &($right)) {
            (left_val, right_val) => {
                if !(*left_val == *right_val) {
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
                if !(*left_val == *right_val) {
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

fn get_token_helpers<'p>(py: Python<'p>) -> PyResult<&'p PyModule> {
    PyModule::from_code(
        py,
        include_str!("token_helpers.py"),
        "token_helpers.py",
        "token_helpers",
    )
}

#[test]
fn test_tokenize() {
    let test_py = include_str!("fixtures/tokenizer/test.py");
    let test_py_tokens = tokenize(test_py).unwrap();
    let actual_token_json = serde_json::to_string_pretty(&test_py_tokens).unwrap();

    let gil = Python::acquire_gil();
    let py = gil.python();
    let token_helpers = get_token_helpers(py).unwrap();

    let source = PyBytes::new(py, test_py.as_bytes());
    let result = token_helpers.call("get_token_json", (source,), None);

    match result {
        Ok(py_string) => {
            let expected_token_json: String = py_string.extract().unwrap();
            assert_strings_eq!(actual_token_json, expected_token_json);
        }
        Err(py_err) => py_err.print(py),
    }
}
