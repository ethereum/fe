use std::collections::HashSet;

/// Iterate over the lines in `input` and include line endings in the results.
pub fn lines_with_endings<'a>(input: &'a str) -> impl Iterator<Item = &'a str> {
    let mut rest = input;

    std::iter::from_fn(move || match rest.find('\n') {
        Some(i) => {
            let end = i + 1;
            let line = &rest[..end];
            rest = &rest[end..];
            Some(line)
        }
        None if !rest.is_empty() => {
            let line = rest;
            rest = &rest[rest.len()..];
            Some(line)
        }
        None => None,
    })
}

/// Strip the characters in the static string `strip` from the right side of the string slice
/// `input`.
pub fn rstrip_slice<'a>(input: &'a str, strip: &str) -> &'a str {
    let strip: HashSet<_> = strip.chars().collect();

    let mut end = input.len();

    if let Some(last_val) = input.chars().last() {
        let mut last = last_val;

        while strip.contains(&last) {
            end -= last.len_utf8();
            if let Some(last_val) = input[..end].chars().last() {
                last = last_val;
            } else {
                break;
            }
        }
    }

    &input[..end]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lines_with_endings() {
        // Empty string
        let input = r"";
        let actual: Vec<_> = lines_with_endings(input).collect();
        let expected: Vec<&'static str> = Vec::new();
        assert_eq!(actual, expected);

        // Single line
        let input = r"testing";
        let actual: Vec<_> = lines_with_endings(input).collect();
        let expected = vec!["testing"];
        assert_eq!(actual, expected);

        // No newline at start or end
        let input = r"testing
the
lines 
here";
        let actual: Vec<_> = lines_with_endings(input).collect();
        let expected = vec!["testing\n", "the\n", "lines \n", "here"];
        assert_eq!(actual, expected);

        // Newline at start only
        let input = r"
testing
the
lines 
here";
        let actual: Vec<_> = lines_with_endings(input).collect();
        let expected = vec!["\n", "testing\n", "the\n", "lines \n", "here"];
        assert_eq!(actual, expected);

        // Newline at end only
        let input = r"testing
the
lines 
here
";
        let actual: Vec<_> = lines_with_endings(input).collect();
        let expected = vec!["testing\n", "the\n", "lines \n", "here\n"];
        assert_eq!(actual, expected);

        // Newline at start and end
        let input = r"

testing

the
lines 
here


";
        let actual: Vec<_> = lines_with_endings(input).collect();
        let expected = vec![
            "\n",
            "\n",
            "testing\n",
            "\n",
            "the\n",
            "lines \n",
            "here\n",
            "\n",
            "\n",
        ];
        assert_eq!(actual, expected);
    }

    #[test]
    fn test_rstrip_slice() {
        let examples = vec![
            (("asdfasdf\r\n", "\r\n"), "asdfasdf"),
            (("asdfasdf\r\n", "\n"), "asdfasdf\r"),
            (("asdfasdf\r\n", ""), "asdfasdf\r\n"),
            (("asdfasdf", "\r\n"), "asdfasdf"),
            (("", "\r\n"), ""),
        ];
        for ((input, strip), expected) in examples {
            let actual = rstrip_slice(input, strip);
            assert_eq!(actual, expected);
        }
    }
}
