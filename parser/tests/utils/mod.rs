use std::path::PathBuf;

#[allow(dead_code)]
pub fn get_fixture_content(fixture_name: &str) -> (String, PathBuf) {
    let fe_fixtures_path = std::env::var("FE_FIXTURES_PATH")
        .expect("must set FE_FIXTURES_PATH env var to write fixtures");

    let mut path = PathBuf::from(fe_fixtures_path);
    path.push(fixture_name);

    let content = std::fs::read_to_string(&path).expect(&format!(
        "could not read content from fixture path {}",
        path.to_str().unwrap(),
    ));

    (content, path)
}

/// Parse file content containing a test example into a tuple of input text and
/// expected serialization.  Input text and expected serialization are separated
/// by a line that only contains the string "---".
pub fn parse_fixture(input: &str) -> Result<(&str, &str), String> {
    let parts: Vec<_> = input.split("\n---\n").collect();

    if parts.len() != 2 {
        Err(format!("expected 2 parts, got {}", parts.len()))
    } else {
        let input = parts[0];
        let parsed = parts[1];

        // If single trailing newline is present, clip off
        Ok(match parsed.chars().last() {
            Some(c) if c == '\n' => (input, &parsed[..parsed.len() - 1]),
            _ => (input, parsed),
        })
    }
}

/// Empty slice syntax is so ugly :/
#[allow(unused_macros)]
macro_rules! empty_slice {
    () => {
        &[][..]
    };
}

/// Include a test example file and parse it.
#[allow(unused_macros)]
macro_rules! include_test_example {
    ($path:expr) => {
        $crate::utils::parse_fixture(include_str!($path))
            .expect(&format!("Test example has wrong format {}", $path))
    };
}

/// Apply the function identified by `$func` to the fixture content in the files
/// given in `$($path),+`.
#[allow(unused_macros)]
macro_rules! do_with_fixtures {
    ($func:expr, $($path:expr),+,) => {{
        do_with_fixtures!($func, $($path),+)
    }};
    ($func:expr, $($path:expr),+) => {{
        let fixtures = vec![
            $(($path, include_test_example!($path))),+
        ];

        for (filename, (input, expected_ser)) in fixtures {
            $func(filename, input, expected_ser);
        }
    }};
}
