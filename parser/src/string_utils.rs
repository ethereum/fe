/// Iterate over the lines in `buf` and include line endings in the results.  Also, provide byte
/// offsets of line beginnings and endings.
pub fn lines_with_endings<'a>(buf: &'a str) -> impl Iterator<Item = (&'a str, usize, usize)> {
    let mut rest = buf;
    let mut rest_offset = 0;

    std::iter::from_fn(move || match rest.find('\n') {
        Some(i) => {
            let start = rest_offset;
            let end = i + 1;
            let line = &rest[..end];

            rest = &rest[end..];
            rest_offset += end;

            Some((line, start, rest_offset))
        }
        None if !rest.is_empty() => {
            let start = rest_offset;
            let end = rest.len();
            let line = rest;

            rest = &rest[end..];
            rest_offset += end;

            Some((line, start, rest_offset))
        }
        None => None,
    })
}

/// Strip the characters in the string `strip` from the right side of the string slice `input`.
pub fn rstrip_slice<'a>(input: &'a str, strip: &str) -> &'a str {
    let mut end = input.len();

    for (i, c) in input.char_indices().rev() {
        if strip.contains(c) {
            end = i;
        } else {
            break;
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
        let expected: Vec<(&'static str, usize, usize)> = Vec::new();
        assert_eq!(actual, expected);

        // Single line
        let input = r"testing";
        let actual: Vec<_> = lines_with_endings(input).collect();
        let expected = vec![("testing", 0, 7)];
        assert_eq!(actual, expected);

        // No newline at start or end
        let input = r"testing
the
lines 
here";
        let actual: Vec<_> = lines_with_endings(input).collect();
        let expected = vec![
            ("testing\n", 0, 8),
            ("the\n", 8, 12),
            ("lines \n", 12, 19),
            ("here", 19, 23),
        ];
        assert_eq!(actual, expected);

        // Newline at start only
        let input = r"
testing
the
lines 
here";
        let actual: Vec<_> = lines_with_endings(input).collect();
        let expected = vec![
            ("\n", 0, 1),
            ("testing\n", 1, 9),
            ("the\n", 9, 13),
            ("lines \n", 13, 20),
            ("here", 20, 24),
        ];
        assert_eq!(actual, expected);

        // Newline at end only
        let input = r"testing
the
lines 
here
";
        let actual: Vec<_> = lines_with_endings(input).collect();
        let expected = vec![
            ("testing\n", 0, 8),
            ("the\n", 8, 12),
            ("lines \n", 12, 19),
            ("here\n", 19, 24),
        ];
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
            ("\n", 0, 1),
            ("\n", 1, 2),
            ("testing\n", 2, 10),
            ("\n", 10, 11),
            ("the\n", 11, 15),
            ("lines \n", 15, 22),
            ("here\n", 22, 27),
            ("\n", 27, 28),
            ("\n", 28, 29),
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
