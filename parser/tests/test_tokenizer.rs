use pyo3::prelude::*;
use pyo3::types::PyBytes;
use serde::{
    Deserialize,
    Serialize,
};

use vyper_parser::span::Position;
use vyper_parser::tokenizer::*;

#[macro_use]
mod utils;

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

/// This struct and its associated `From` implementation are used to cast vyper
/// tokens (which may include specialized information only used by the vyper
/// parser, such as global byte offsets into a source file) into objects with
/// only as much information as would be found in a python token.
///
/// At the time this struct was added, its specific use was in filtering out
/// global byte offsets found in vyper tokens for which no corresponding values
/// exist by default in python tokens.
#[derive(Serialize, Deserialize)]
pub struct ConciseTokenInfo<'a> {
    pub typ: TokenType,
    pub string: &'a str,
    pub start: Position,
    pub end: Position,
    pub line: &'a str,
}

impl<'a> From<&'a TokenInfo<'a>> for ConciseTokenInfo<'a> {
    fn from(token_info: &'a TokenInfo<'a>) -> Self {
        Self {
            typ: token_info.typ,
            string: token_info.string,
            start: token_info.start_pos,
            end: token_info.end_pos,
            line: token_info.line,
        }
    }
}

fn get_rust_token_json(input: &str) -> String {
    let tokens = tokenize(input).unwrap();

    // Convert vyper tokens into "concise" tokens that only include as much
    // information as would be found in a python token object
    let concise_tokens = tokens
        .iter()
        .map(|i| i.into())
        .collect::<Vec<ConciseTokenInfo>>();

    serde_json::to_string_pretty(&concise_tokens).unwrap()
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
