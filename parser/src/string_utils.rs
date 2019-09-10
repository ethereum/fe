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
}
