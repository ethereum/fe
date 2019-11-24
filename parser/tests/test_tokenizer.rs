use pyo3::prelude::*;
use pyo3::types::PyBytes;
use serde::Serialize;

use vyper_parser::string_utils::{
    FilePositions,
    Position,
};
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
#[derive(Serialize)]
struct PythonTokenInfo<'a> {
    pub typ: TokenType,
    pub string: &'a str,
    pub start: Position,
    pub end: Position,
    pub line: &'a str,
}

impl<'a> From<(&'a Token<'a>, &mut FilePositions<'_>)> for PythonTokenInfo<'a> {
    fn from(info: (&'a Token<'a>, &mut FilePositions<'_>)) -> Self {
        let (tok, file_positions) = info;

        let start_pos = match file_positions.get_pos(tok.span.start) {
            Some(pos) => pos,
            None => file_positions.get_eof(),
        };
        let end_pos = match file_positions.get_pos(tok.span.end) {
            Some(pos) => pos,
            None => file_positions.get_eof(),
        };

        Self {
            typ: tok.typ,
            string: tok.string,
            start: start_pos,
            end: end_pos,
            line: tok.line,
        }
    }
}

fn get_rust_token_json(input: &str) -> String {
    let tokens = tokenize(input).unwrap();
    let mut file_positions = FilePositions::new(input);

    // Convert vyper tokens into python tokens
    let python_tokens = tokens
        .iter()
        .map(|tok| (tok, &mut file_positions).into())
        .collect::<Vec<PythonTokenInfo>>();

    serde_json::to_string_pretty(&python_tokens).unwrap()
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
