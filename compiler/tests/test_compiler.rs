use std::fs;
use vyper_compiler::yul as compiler;
use vyper_parser::parsers;

struct Fixture {
    vyp: String,
    yul: String,
}

fn read_fixture(path: &str) -> Fixture {
    let contents = fs::read_to_string(path).expect("Unable to read fixture file.");
    let split_contents: Vec<&str> = contents.split("---").collect();

    Fixture {
        vyp: String::from(split_contents[0]),
        yul: String::from(split_contents[1]),
    }
}

#[test]
fn test_fixtures() {
    let fixtures: Vec<Fixture> = fs::read_dir("tests/fixtures")
        .unwrap()
        .map(|entry| read_fixture(entry.unwrap().path().to_str().unwrap()))
        .collect();

    for fixture in fixtures {
        let toks = vyper_parser::get_parse_tokens(&*fixture.vyp).unwrap();
        let stmt = parsers::file_input(&toks[..]).unwrap().1.node;
        let expected_yul: Vec<&str> = fixture.yul.split_whitespace().collect();

        if let Ok(Some(compiled_yul)) = compiler::module(&stmt) {
            assert_eq!(compiled_yul.to_string(), expected_yul.join(" "));
        } else {
            assert!(false, "Something went wrong.")
        }
    }
}
