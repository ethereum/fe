use regex::Regex;

use crate::node::Span;
use crate::string_utils::{
    lines_with_endings,
    lstrip_slice,
    rstrip_slice,
};
use crate::tokenizer::regex::{
    compile_anchored,
    get_pseudotoken_pattern,
    get_single_quote_set,
    get_triple_quote_set,
    DOUBLE,
    DOUBLE3,
    SINGLE,
    SINGLE3,
};
use crate::tokenizer::types::{
    Token,
    TokenType::*,
};

const TABSIZE: usize = 8;

#[inline]
fn is_identifier_char(c: char) -> bool {
    c == '_' || c.is_ascii_alphabetic() || c.is_digit(10)
}

#[derive(Debug, PartialEq)]
pub struct TokenizeError {
    pub msg: &'static str,
    pub offset: usize,
}

/// Parse a source string into a vector of tokens.
///
/// Arguments:
///
/// * `input` - The source string to be parsed.
///
/// Returns:
///
/// A vector of tokens.
#[allow(clippy::cognitive_complexity)]
#[allow(clippy::trivial_regex)]
pub fn tokenize<'a>(input: &'a str) -> Result<Vec<Token<'a>>, TokenizeError> {
    // Static values/helpers
    let pseudo_token_re = compile_anchored(&get_pseudotoken_pattern());

    let triple_quoted = get_triple_quote_set();
    let single_quoted = get_single_quote_set();

    let double3_re = Regex::new(DOUBLE3).unwrap();
    let single3_re = Regex::new(SINGLE3).unwrap();
    let double_re = Regex::new(DOUBLE).unwrap();
    let single_re = Regex::new(SINGLE).unwrap();

    // The ordering of checks matters here.  We need to eliminate the possibility of
    // triple quote delimiters before looking for single quote delimiters.
    let get_contstr_end_re = |token: &str| -> &Regex {
        let token_stripped = lstrip_slice(token, "bBrRuUfF");

        if token_stripped.starts_with("\"\"\"") {
            &double3_re
        } else if token_stripped.starts_with("'''") {
            &single3_re
        } else if token_stripped.starts_with('"') {
            &double_re
        } else {
            // This arm of the if statement is equivalent to the following check:
            // `else if token_stripped.starts_with('\'')`
            //
            // This is because any string in `token` has already been matched against a
            // regex that ensures it begins with """, ''', ", or ' after
            // stripping of any leading prefix codes
            &single_re
        }
    };

    // Token list result
    let mut result: Vec<Token<'a>> = Vec::new();

    // State vars
    let mut parenlev: usize = 0;
    let mut continued: bool = false;
    let mut indents: Vec<usize> = vec![0];

    let mut contstr_start: Option<usize> = None;
    let mut contline_start: Option<usize> = None;
    let mut contstr_end_re: Option<&Regex> = None;
    let mut needcont: bool = false;

    for (line, line_start, line_end) in lines_with_endings(input) {
        // Set parsing position relative to this line
        let mut line_pos: usize = 0;
        let line_len: usize = line.len();

        if let Some(contstr_start_val) = contstr_start {
            // Continued string
            if let Some(endmatch) = contstr_end_re.unwrap().find(line) {
                let tok_end = endmatch.end();
                line_pos = tok_end;

                result.push(Token {
                    typ: STRING,
                    string: &input[contstr_start_val..line_start + tok_end],
                    span: Span::new(contstr_start_val, line_start + tok_end),
                    line: &input[contline_start.unwrap()..line_end],
                });

                contstr_start = None;
                contline_start = None;

                needcont = false;
            } else if needcont && !line.ends_with("\\\n") && !line.ends_with("\\\r\n") {
                result.push(Token {
                    typ: ERRORTOKEN,
                    string: &input[contstr_start_val..line_end],
                    span: Span::new(contstr_start_val, line_end),
                    line: &input[contline_start.unwrap()..line_start],
                });

                contstr_start = None;
                contline_start = None;

                continue;
            } else {
                continue;
            }
        } else if parenlev == 0 && !continued {
            // New statement
            let mut column: usize = 0;

            // Measure leading whitespace
            for c in line.chars() {
                match c {
                    ' ' => {
                        column += 1;
                    }
                    '\t' => {
                        column = (column / TABSIZE + 1) * TABSIZE;
                    }
                    '\x0c' => {
                        // Form feed ("\f" in python)
                        column = 0;
                    }
                    _ => {
                        // Break if we encounter anything that's not part of indentation
                        break;
                    }
                }
                line_pos += c.len_utf8();
            }

            if line_pos == line_len {
                // If no more chars in line (not even newline, carriage return, etc.), we're at
                // EOF.  Break out of the token loop.
                break;
            }

            {
                let c = line[line_pos..].chars().next().unwrap();
                if c == '#' || c == '\r' || c == '\n' {
                    if c == '#' {
                        let comment_token = rstrip_slice(&line[line_pos..], "\r\n");
                        let comment_token_len = comment_token.len();

                        result.push(Token {
                            typ: COMMENT,
                            string: comment_token,
                            span: Span::new(
                                line_start + line_pos,
                                line_start + line_pos + comment_token_len,
                            ),
                            line,
                        });

                        line_pos += comment_token_len;
                    }

                    result.push(Token {
                        typ: NL,
                        string: &line[line_pos..],
                        span: Span::new(line_start + line_pos, line_end),
                        line,
                    });

                    continue;
                }
            }

            let rest_off = line_start + line_pos;

            if column > *indents.last().unwrap() {
                indents.push(column);
                result.push(Token {
                    typ: INDENT,
                    string: &line[..line_pos],
                    span: Span::new(line_start, rest_off),
                    line,
                });
            }

            if !indents.contains(&column) {
                return Err(TokenizeError {
                    msg: "unindent does not match any outer indentation level",
                    offset: rest_off,
                });
            }

            while column < *indents.last().unwrap() {
                indents.pop();
                result.push(Token {
                    typ: DEDENT,
                    string: &line[line_pos..line_pos],
                    span: Span::new(rest_off, rest_off),
                    line,
                });
            }
        } else {
            continued = false;
        }

        while line_pos < line_len {
            if let Some(pseudomatch) = pseudo_token_re.captures(&line[line_pos..]) {
                let capture = pseudomatch.get(1).unwrap();
                let tok_start = line_pos + capture.start();
                let tok_end = line_pos + capture.end();

                let soff = line_start + tok_start;
                let eoff = line_start + tok_end;
                line_pos = tok_end;

                if tok_start == tok_end {
                    continue;
                }

                let token = &line[tok_start..tok_end];
                let initial = line[tok_start..].chars().next().unwrap();

                if initial.is_ascii_digit() || (initial == '.' && token != "." && token != "...") {
                    result.push(Token {
                        typ: NUMBER,
                        string: token,
                        span: Span::new(soff, eoff),
                        line,
                    });
                } else if initial == '\r' || initial == '\n' {
                    result.push(Token {
                        typ: if parenlev > 0 { NL } else { NEWLINE },
                        string: token,
                        span: Span::new(soff, eoff),
                        line,
                    });
                } else if initial == '#' {
                    result.push(Token {
                        typ: COMMENT,
                        string: token,
                        span: Span::new(soff, eoff),
                        line,
                    });
                } else if triple_quoted.contains(token) {
                    contstr_end_re = Some(get_contstr_end_re(token));

                    if let Some(endmatch) = contstr_end_re.unwrap().find_at(line, line_pos) {
                        line_pos = endmatch.end();
                        let token = &line[tok_start..line_pos];

                        result.push(Token {
                            typ: STRING,
                            string: token,
                            span: Span::new(soff, line_start + line_pos),
                            line,
                        });
                    } else {
                        contstr_start = Some(line_start + tok_start);
                        contline_start = Some(line_start);
                        break;
                    }
                } else if single_quoted.contains(&initial.to_string())
                    || single_quoted.contains(&token.chars().take(2).collect::<String>())
                    || single_quoted.contains(&token.chars().take(3).collect::<String>())
                {
                    if token.ends_with('\n') {
                        contstr_end_re = Some(get_contstr_end_re(token));

                        contstr_start = Some(line_start + tok_start);
                        contline_start = Some(line_start);

                        needcont = true;
                    } else {
                        result.push(Token {
                            typ: STRING,
                            string: token,
                            span: Span::new(soff, eoff),
                            line,
                        });
                    }
                } else if is_identifier_char(initial) {
                    result.push(Token {
                        typ: NAME,
                        string: token,
                        span: Span::new(soff, eoff),
                        line,
                    });
                } else if initial == '\\' {
                    continued = true;
                } else {
                    if initial == '(' || initial == '[' || initial == '{' {
                        parenlev += 1;
                    } else if initial == ')' || initial == ']' || initial == '}' {
                        if parenlev == 0 {
                            return Err(TokenizeError {
                                msg: "Unbalanced brackets",
                                offset: line_pos,
                            });
                        }
                        parenlev -= 1;
                    }
                    result.push(Token {
                        typ: OP,
                        string: token,
                        span: Span::new(soff, eoff),
                        line,
                    });
                }
            } else {
                let char = line[line_pos..].chars().next().unwrap();
                let len = char.len_utf8();
                let string = &line[line_pos..line_pos + len];
                #[allow(clippy::range_plus_one)]
                result.push(Token {
                    typ: ERRORTOKEN,
                    string,
                    span: Span::new(line_start + line_pos, line_start + line_pos + len),
                    line,
                });
                line_pos += len;
            }
        }
    }

    // Ensure brackets are balanced
    if parenlev != 0 {
        return Err(TokenizeError {
            msg: "Unbalanced brackets",
            offset: input.len(),
        });
    }

    // We use this zero-length slice as the ending content for remaining tokens.
    // This is *just in case* anyone actually cares that the location of the
    // pointer makes any kind of sense.
    let input_len = input.len();
    let empty_end_slice = &input[input_len..];

    if contstr_start.is_some() {
        return Err(TokenizeError {
            msg: "EOF in multi-line string",
            offset: input_len,
        });
    }

    if continued {
        return Err(TokenizeError {
            msg: "EOF in multi-line statement",
            offset: input_len,
        });
    }

    // Ensure content tokens end with newline (this allows parsers to be defined
    // more consistently)
    match result.last() {
        Some(Token { typ: NEWLINE, .. }) => (),
        _ => result.push(Token {
            typ: NEWLINE,
            string: empty_end_slice,
            span: Span::new(input_len, input_len),
            line: empty_end_slice,
        }),
    }
    // Emit any necessary dedents
    for _ in indents.iter().skip(1) {
        result.push(Token {
            typ: DEDENT,
            string: empty_end_slice,
            span: Span::new(input_len, input_len),
            line: empty_end_slice,
        });
    }
    result.push(Token {
        typ: ENDMARKER,
        string: empty_end_slice,
        span: Span::new(input_len, input_len),
        line: empty_end_slice,
    });

    Ok(result)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_balanced_brackets() {
        let inputs = ["[]", "[[]]", "()", "(())", "{}", "{{}}"];
        for input in &inputs {
            assert!(tokenize(input).is_ok());
        }
    }

    #[test]
    fn test_unbalanced_brackets() {
        let inputs = ["[[]", "[]]", "(()", "())", "{{}", "{}}"];
        for input in &inputs {
            assert!(tokenize(input).is_err());
        }
    }

    #[test]
    fn test_unicode_token() {
        let uni = "\u{6dd}";
        let input = format!("[{}]", uni);
        let result = tokenize(&input);
        assert!(result.is_ok());
        let tokens = result.unwrap();
        let token = tokens
            .iter()
            .filter(|token| token.typ == ERRORTOKEN)
            .nth(0)
            .unwrap();
        assert_eq!(token.typ, ERRORTOKEN);
        assert_eq!(token.string, uni);
    }
}
