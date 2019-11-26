use std::io::Write;
use std::process::{
    Command,
    Stdio,
};

use serde::Serialize;

use vyper_parser::string_utils::StringPositions;
use vyper_parser::tokenizer::{
    tokenize,
    Token,
    TokenType,
};

#[macro_use]
mod utils;

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

fn get_python_token_json(input: &str) -> String {
    let mut py3 = Command::new("python3")
        .arg("-c")
        .arg(include_str!("token_helpers.py"))
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("could not start python 3");

    {
        let py3_stdin = py3.stdin.as_mut().unwrap();
        py3_stdin
            .write_all(input.as_bytes())
            .expect("failed to write to stdin for python 3");
    }

    let output = py3.wait_with_output().expect("failed to wait on python 3");

    if !output.status.success() {
        panic!(
            "failed to get output from python 3: {}",
            String::from_utf8(output.stderr).unwrap()
        );
    }

    String::from_utf8(output.stdout).unwrap()
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

    for (filename, content) in fixtures {
        let expected = get_python_token_json(content);
        let actual = get_rust_token_json(content);

        assert_strings_eq!(
            expected,
            actual,
            "Tokenizations didn't match for {}",
            filename
        );
    }
}
