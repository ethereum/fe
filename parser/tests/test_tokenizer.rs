extern crate rust_vyper_parser;

use rust_vyper_parser::tokenizer::*;

#[test]
fn test_tokenize() {
    let tokens = tokenize(
        r"
# Test comment
class TestClassDefinition(WithParent1, WithParent2):
foo = 'bar'

def foo():
    return 'bar'

test = 'foo'
",
    )
    .unwrap();

    let serialized = serde_json::to_string_pretty(&tokens).unwrap();

    println!("{}", serialized);
}
