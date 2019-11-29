use serde::{
    Deserialize,
    Serialize,
};

/// Iterate over the lines in `buf` and include line endings in the results.
/// Also, provide byte offsets of line beginnings and endings.
pub fn lines_with_endings(buf: &str) -> impl Iterator<Item = (&str, usize, usize)> {
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

/// Strip the characters in the string `strip` from the left side of the string
/// slice `input`.
pub fn lstrip_slice<'a>(input: &'a str, strip: &str) -> &'a str {
    let mut start = 0;

    for c in input.chars() {
        if strip.contains(c) {
            start += c.len_utf8();
        } else {
            break;
        }
    }

    &input[start..]
}

/// Strip the characters in the string `strip` from the right side of the string
/// slice `input`.
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

/// A position in a source file specified by a 1-indexed line number and a
/// 0-indexed byte offset into the line specified by that number.
#[derive(Serialize, Deserialize, Debug, PartialEq, Clone, Copy)]
pub struct Position {
    /// A 1-indexed line number
    pub line: usize,
    /// A 0-indexed byte offset into a line
    pub col: usize,
}

impl Position {
    fn new(line: usize, col: usize) -> Self {
        Position { line, col }
    }
}

/// Efficiently find the text positions (line, column tuples) of a monotonically
/// increasing sequence of byte offsets in a string.  Non-monotonic sequences
/// are also supported but are less efficient.
pub struct StringPositions<'a> {
    /// A string in which text positions should be calculated
    input: &'a str,
    /// The 1-indexed line number at the current byte offset
    line: usize,
    /// The 0-indexed column number at the current byte offset
    col: usize,
    /// The current byte offset
    offset: usize,
}

impl<'a> StringPositions<'a> {
    /// Create a new position counter over the string in `input`.
    pub fn new(input: &'a str) -> Self {
        Self {
            input,
            line: 1,
            col: 0,
            offset: 0,
        }
    }

    /// Reset the position counter's internal state.
    fn reset(&mut self) {
        self.line = 1;
        self.col = 0;
        self.offset = 0;
    }

    /// Get the position at byte offset `pos_offset` in a string.
    pub fn get_pos(&mut self, pos_offset: usize) -> Option<Position> {
        if pos_offset >= self.input.len() {
            // Position does not exist
            return None;
        }
        if pos_offset < self.offset {
            // The desired position is behind the current cursor.  Start from the beginning.
            self.reset()
        }

        let rel_offset = pos_offset - self.offset;
        let rest = &self.input[self.offset..];

        for (chr_offset, chr) in rest.char_indices() {
            if chr_offset >= rel_offset {
                break;
            }

            let chr_len = chr.len_utf8();

            if chr == '\n' {
                self.line += 1;
                self.col = 0;
            } else {
                self.col += chr_len;
            }

            self.offset += chr_len;
        }

        Some(Position::new(self.line, self.col))
    }

    /// Get the last valid position in a string.
    pub fn get_last(&mut self) -> Option<Position> {
        let len = self.input.len();

        if len == 0 {
            None
        } else {
            self.get_pos(len - 1)
        }
    }

    /// Get the pseudo-position representing the end of the file (string).
    pub fn get_eof(&mut self) -> Position {
        match self.get_last() {
            None => Position::new(1, 0),
            Some(last_pos) => {
                let last_chr = self.input[self.offset..].chars().next().unwrap();

                if last_chr == '\n' {
                    Position::new(last_pos.line + 1, 0)
                } else {
                    Position::new(last_pos.line, last_pos.col + 1)
                }
            }
        }
    }
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
    fn test_lstrip_slice() {
        let examples = vec![
            (("\r\nasdfasdf", "\r\n"), "asdfasdf"),
            (("\n\rasdfasdf", "\n"), "\rasdfasdf"),
            (("\r\nasdfasdf", ""), "\r\nasdfasdf"),
            (("asdfasdf", "\r\n"), "asdfasdf"),
            (("", "\r\n"), ""),
        ];
        for ((input, strip), expected) in examples {
            let actual = lstrip_slice(input, strip);
            assert_eq!(actual, expected);
        }
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

    #[test]
    fn test_file_positions() {
        // Empty string has expected behavior
        let mut string_pos = StringPositions::new(&r#""#);
        assert_eq!(string_pos.get_pos(0), None);
        assert_eq!(string_pos.get_pos(1), None);
        assert_eq!(string_pos.get_last(), None);
        assert_eq!(string_pos.get_eof(), Position::new(1, 0));

        // Can get same position twice
        let mut string_pos = StringPositions::new(&r#"asdf"#);
        assert_eq!(string_pos.get_pos(0), Some(Position::new(1, 0)));
        assert_eq!(string_pos.get_pos(0), Some(Position::new(1, 0)));
        assert_eq!(string_pos.get_pos(1), Some(Position::new(1, 1)));
        assert_eq!(string_pos.get_pos(1), Some(Position::new(1, 1)));
        assert_eq!(string_pos.get_last(), Some(Position::new(1, 3)));
        assert_eq!(string_pos.get_eof(), Position::new(1, 4));

        // Can get sequential positions
        let mut string_pos = StringPositions::new(&r#"asdf"#);
        assert_eq!(string_pos.get_pos(0), Some(Position::new(1, 0)));
        assert_eq!(string_pos.get_pos(1), Some(Position::new(1, 1)));
        assert_eq!(string_pos.get_pos(2), Some(Position::new(1, 2)));
        assert_eq!(string_pos.get_pos(3), Some(Position::new(1, 3)));

        // Can get non-sequential positions
        let mut string_pos = StringPositions::new(&r#"asdf"#);
        assert_eq!(string_pos.get_pos(0), Some(Position::new(1, 0)));
        assert_eq!(string_pos.get_pos(1), Some(Position::new(1, 1)));
        assert_eq!(string_pos.get_pos(0), Some(Position::new(1, 0)));

        // Can get sequential then invalid
        let mut string_pos = StringPositions::new(&r#"asdf"#);
        assert_eq!(string_pos.get_pos(0), Some(Position::new(1, 0)));
        assert_eq!(string_pos.get_pos(1), Some(Position::new(1, 1)));
        assert_eq!(string_pos.get_pos(2), Some(Position::new(1, 2)));
        assert_eq!(string_pos.get_pos(3), Some(Position::new(1, 3)));
        assert_eq!(string_pos.get_pos(4), None);
        assert_eq!(string_pos.get_pos(5), None);

        // Can get invalid
        let mut string_pos = StringPositions::new(&r#"asdf"#);
        assert_eq!(string_pos.get_pos(4), None);

        // Can get multiple line positions
        let mut string_pos = StringPositions::new(
            &r#"i wrote this
thing that finds
positions on lines"#,
        );
        assert_eq!(string_pos.get_pos(4), Some(Position::new(1, 4)));
        assert_eq!(string_pos.get_pos(11), Some(Position::new(1, 11)));
        assert_eq!(string_pos.get_pos(12), Some(Position::new(1, 12)));
        assert_eq!(string_pos.get_pos(13), Some(Position::new(2, 0)));
        assert_eq!(string_pos.get_pos(18), Some(Position::new(2, 5)));
        assert_eq!(string_pos.get_pos(28), Some(Position::new(2, 15)));
        assert_eq!(string_pos.get_pos(29), Some(Position::new(2, 16)));
        assert_eq!(string_pos.get_pos(30), Some(Position::new(3, 0)));
        assert_eq!(string_pos.get_pos(42), Some(Position::new(3, 12)));
        assert_eq!(string_pos.get_pos(47), Some(Position::new(3, 17)));
        assert_eq!(string_pos.get_pos(48), None);
        assert_eq!(string_pos.get_last(), Some(Position::new(3, 17)));
        assert_eq!(string_pos.get_eof(), Position::new(3, 18));

        // EOF is after newline
        let mut string_pos = StringPositions::new(
            &r#"i wrote this
thing that finds
positions on lines
"#,
        );
        assert_eq!(string_pos.get_last(), Some(Position::new(3, 18)));
        assert_eq!(string_pos.get_eof(), Position::new(4, 0));
    }
}
