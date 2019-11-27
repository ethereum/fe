use serde::Serialize;

use vyper_parser::string_utils::StringPositions;
use vyper_parser::tokenizer::{
    tokenize,
    Token,
    TokenType,
};

#[macro_use]
mod utils;
use utils::parse_test_example;

/// A python token object similar to those defined in python's stdlib `tokenize`
/// module.
#[derive(Serialize)]
struct PythonTokenInfo<'a> {
    pub typ: TokenType,
    pub string: &'a str,
    pub start: (usize, usize),
    pub end: (usize, usize),
    pub line: &'a str,
}

impl<'a> PythonTokenInfo<'a> {
    pub fn from_token_and_positions(
        tok: &'a Token<'a>,
        string_pos: &mut StringPositions<'_>,
    ) -> Self {
        let start_pos = match string_pos.get_pos(tok.span.start) {
            Some(pos) => pos,
            None => string_pos.get_eof(),
        };
        let end_pos = match string_pos.get_pos(tok.span.end) {
            Some(pos) => pos,
            None => string_pos.get_eof(),
        };

        Self {
            typ: tok.typ,
            string: tok.string,
            start: (start_pos.line, start_pos.col),
            end: (end_pos.line, end_pos.col),
            line: tok.line,
        }
    }
}

fn get_rust_token_json(input: &str) -> String {
    let tokens = tokenize(input).unwrap();
    let mut string_pos = StringPositions::new(input);

    // Convert vyper tokens into python tokens
    let python_tokens = tokens
        .iter()
        .map(|tok| PythonTokenInfo::from_token_and_positions(tok, &mut string_pos))
        .collect::<Vec<PythonTokenInfo>>();

    serde_json::to_string_pretty(&python_tokens).unwrap()
}

/// Assert that all the given fixture files are tokenized correctly.  Expected
/// results are defined as serializations.  Print a debug trace if parsing
/// fails.
macro_rules! assert_fixtures_tokenized {
    ($($path:expr),+,) => {{
        assert_fixtures_tokenized!($($path),+)
    }};
    ($($path:expr),+) => {{
        let test_files = vec![
            $(($path, include_test_example!($path))),+
        ];

        for (filename, (inp, expected_ser)) in test_files {
            let actual_ser = get_rust_token_json(inp);

            assert_strings_eq!(
                actual_ser,
                expected_ser,
                "\nTokenizations did not match for {}",
                filename,
            );
        }
    }};
}

#[test]
fn test_tokenize() {
    assert_fixtures_tokenized!(
        "fixtures/tokenizer/basic.py.json",
        "fixtures/tokenizer/triple_quote_strings.py.json",
        "fixtures/tokenizer/single_quote_strings.py.json",
        "fixtures/tokenizer/continued_statements.py.json",
        "fixtures/tokenizer/validator_registration.v.py.json",
        "fixtures/tokenizer/tokenize.py.json",
    );
}
