use difference::{Changeset, Difference};
use serde::Serialize;
use std::fmt;

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
#[macro_export]
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
                        Diff::new(left_val, right_val),
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
                        Diff::new(left_val, right_val),
                    )
                }
            }
        }
    }};
}

/// Convenience function to serialize objects in RON format with custom pretty
/// printing config and struct names.
pub fn to_ron_string_pretty<T>(value: &T) -> ron::ser::Result<String>
where
    T: Serialize,
{
    let config = ron::ser::PrettyConfig {
        indentor: "  ".to_string(),
        ..Default::default()
    };

    let mut serializer = ron::ser::Serializer::new(Some(config), true);
    value.serialize(&mut serializer)?;

    Ok(serializer.into_output_string())
}
