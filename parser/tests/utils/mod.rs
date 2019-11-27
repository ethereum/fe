use std::fmt;

use difference::{
    Changeset,
    Difference,
};
use serde::Serialize;

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
                    write!(f, "{}{}", prefix_lines(" ", x), self.0.split)?;
                }
                Difference::Add(ref x) => {
                    write!(f, "\x1b[92m{}\x1b[0m{}", prefix_lines("+", x), self.0.split)?;
                }
                Difference::Rem(ref x) => {
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
    ($left:expr, $right:expr,) => {{
        assert_strings_eq!($left, $right)
    }};
    ($left:expr, $right:expr) => {{
        match (&($left), &($right)) {
            (left_val, right_val) => {
                if *left_val != *right_val {
                    panic!(
                        "assertion failed: `(left == right)`\ndiff:\n{}",
                        utils::Diff::new(left_val, right_val),
                    )
                }
            }
        }
    }};
    ($left:expr, $right:expr, $($args:tt)*) => {{
        match (&($left), &($right)) {
            (left_val, right_val) => {
                if *left_val != *right_val {
                    panic!(
                        "assertion failed: `(left == right)`: {}\ndiff:\n{}",
                        format_args!($($args)*),
                        utils::Diff::new(left_val, right_val),
                    )
                }
            }
        }
    }};
}

/// Convenience function to serialize objects in RON format with custom pretty
/// printing config and struct names.
#[allow(dead_code)]
pub fn to_ron_string_pretty<T>(value: &T) -> ron::ser::Result<String>
where
    T: Serialize,
{
    let mut config = ron::ser::PrettyConfig::default();
    // Indent with 2 spaces
    config.indentor = "  ".to_string();

    let mut serializer = ron::ser::Serializer::new(Some(config), true);
    value.serialize(&mut serializer)?;

    Ok(serializer.into_output_string())
}

/// Parse test example file content into a tuple of input text and expected
/// serialization.
#[allow(dead_code)]
pub fn parse_test_example<'a>(input: &'a str) -> Result<(&'a str, &'a str), &'static str> {
    let parts: Vec<_> = input.split("\n---\n").collect();

    if parts.len() != 2 {
        Err("Test example has wrong format")
    } else {
        let input = parts[0];
        let parsed = parts[1];

        // If single trailing newline is present, clip off
        match parsed.chars().last() {
            Some(c) if c == '\n' => Ok((input, &parsed[..parsed.len() - 1])),
            _ => Ok((input, parsed)),
        }
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
    ($path:expr) => {{
        parse_test_example(include_str!($path)).unwrap()
    }};
}
