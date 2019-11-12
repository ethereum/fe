use regex::Regex;

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
    Position,
    TokenInfo,
    TokenType::*,
};

pub const TABSIZE: usize = 8;

/// Return true if the char `c` is a valid identifier character.
#[inline]
pub fn is_identifier_char(c: char) -> bool {
    c == '_' || c.is_ascii_alphabetic() || c.is_digit(10)
}

pub fn tokenize<'a>(input: &'a str) -> Result<Vec<TokenInfo<'a>>, String> {
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
    let get_contstr_end_re = |token: &str| {
        let token_stripped = lstrip_slice(token, "bBrRuUfF");

        if token_stripped.starts_with("\"\"\"") {
            &double3_re
        } else if token_stripped.starts_with("'''") {
            &single3_re
        } else if token_stripped.starts_with("\"") {
            &double_re
        } else if token_stripped.starts_with("'") {
            &single_re
        } else {
            panic!("Unrecognized quote style {:?}", token_stripped);
        }
    };

    // Token list result
    let mut result: Vec<TokenInfo<'a>> = Vec::new();

    // State vars
    let mut parenlev: usize = 0;
    let mut continued: bool = false;
    let mut indents: Vec<usize> = vec![0];

    let mut contstr_start_pos: Option<Position> = None;
    let mut contstr_start: Option<usize> = None;
    let mut contline_start: Option<usize> = None;
    let mut contstr_end_re: Option<&Regex> = None;
    let mut needcont: bool = false;

    // Token generation loop.  We use the `loop` keyword here (instead of `for
    // (line, line_num) in ...`) so we can hold onto the iterator vars after the
    // loop finishes.
    let mut line = &input[..0];
    let mut line_num = 0;
    let mut lines = lines_with_endings(input);

    loop {
        let next = lines.next();
        line_num += 1;
        // We use this guard style of exiting to avoid indenting the entire loop body
        if next.is_none() {
            break;
        }

        // Get current line and line offsets
        let next_unwrap = next.unwrap();
        line = next_unwrap.0;
        let line_start = next_unwrap.1;
        let line_end = next_unwrap.2;

        // Set parsing position relative to this line
        let mut line_pos: usize = 0;
        let line_len: usize = line.len();

        if let Some(contstr_start_val) = contstr_start {
            // Continued string
            if let Some(endmatch) = contstr_end_re.unwrap().find(line) {
                let tok_end = endmatch.end();
                line_pos = tok_end;

                result.push(TokenInfo {
                    typ: STRING,
                    string: &input[contstr_start_val..line_start + tok_end],
                    start: contstr_start_pos.unwrap(),
                    end: (line_num, tok_end),
                    line: &input[contline_start.unwrap()..line_end],
                });

                contstr_start = None;
                contline_start = None;

                needcont = false;
            } else if needcont && !line.ends_with("\\\n") && !line.ends_with("\\\r\n") {
                result.push(TokenInfo {
                    typ: ERRORTOKEN,
                    string: &input[contstr_start_val..line_end],
                    start: contstr_start_pos.unwrap(),
                    end: (line_num, line_len),
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
            for c in line[line_pos..].chars() {
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

                        result.push(TokenInfo {
                            typ: COMMENT,
                            string: comment_token,
                            start: (line_num, line_pos),
                            end: (line_num, line_pos + comment_token_len),
                            line: line,
                        });

                        line_pos += comment_token_len;
                    }

                    result.push(TokenInfo {
                        typ: NL,
                        string: &line[line_pos..],
                        start: (line_num, line_pos),
                        end: (line_num, line_len),
                        line: line,
                    });

                    continue;
                }
            }

            if column > *indents.last().unwrap() {
                indents.push(column);
                result.push(TokenInfo {
                    typ: INDENT,
                    string: &line[..line_pos],
                    start: (line_num, 0),
                    end: (line_num, line_pos),
                    line: line,
                });
            }

            while column < *indents.last().unwrap() {
                if !indents.contains(&column) {
                    return Err("Unindent does not match any outer indentation level".to_string());
                }
                indents.pop();
                result.push(TokenInfo {
                    typ: DEDENT,
                    string: &line[line_len..],
                    start: (line_num, line_pos),
                    end: (line_num, line_pos),
                    line: line,
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

                let spos = (line_num, tok_start);
                let epos = (line_num, tok_end);
                line_pos = tok_end;

                if tok_start == tok_end {
                    continue;
                }

                let token = &line[tok_start..tok_end];
                let initial = line[tok_start..].chars().next().unwrap();

                if initial.is_ascii_digit() || (initial == '.' && token != "." && token != "...") {
                    result.push(TokenInfo {
                        typ: NUMBER,
                        string: token,
                        start: spos,
                        end: epos,
                        line: line,
                    });
                } else if initial == '\r' || initial == '\n' {
                    if parenlev > 0 {
                        result.push(TokenInfo {
                            typ: NL,
                            string: token,
                            start: spos,
                            end: epos,
                            line: line,
                        });
                    } else {
                        result.push(TokenInfo {
                            typ: NEWLINE,
                            string: token,
                            start: spos,
                            end: epos,
                            line: line,
                        });
                    }
                } else if initial == '#' {
                    result.push(TokenInfo {
                        typ: COMMENT,
                        string: token,
                        start: spos,
                        end: epos,
                        line: line,
                    });
                } else if triple_quoted.contains(token) {
                    contstr_end_re = Some(get_contstr_end_re(token));

                    if let Some(endmatch) = contstr_end_re.unwrap().find_at(line, line_pos) {
                        line_pos = endmatch.end();
                        let token = &line[tok_start..line_pos];

                        result.push(TokenInfo {
                            typ: STRING,
                            string: token,
                            start: spos,
                            end: (line_num, line_pos),
                            line: line,
                        });
                    } else {
                        contstr_start_pos = Some((line_num, tok_start));
                        contstr_start = Some(line_start + tok_start);
                        contline_start = Some(line_start);
                        break;
                    }
                } else if single_quoted.contains(&initial.to_string())
                    || single_quoted.contains(&token.chars().take(2).collect::<String>())
                    || single_quoted.contains(&token.chars().take(3).collect::<String>())
                {
                    if token.chars().last().unwrap() == '\n' {
                        contstr_end_re = Some(get_contstr_end_re(token));

                        contstr_start_pos = Some((line_num, tok_start));
                        contstr_start = Some(line_start + tok_start);
                        contline_start = Some(line_start);

                        needcont = true;
                    } else {
                        result.push(TokenInfo {
                            typ: STRING,
                            string: token,
                            start: spos,
                            end: epos,
                            line: line,
                        });
                    }
                } else if is_identifier_char(initial) {
                    result.push(TokenInfo {
                        typ: NAME,
                        string: token,
                        start: spos,
                        end: epos,
                        line: line,
                    });
                } else if initial == '\\' {
                    continued = true;
                } else {
                    if initial == '(' || initial == '[' || initial == '{' {
                        parenlev += 1;
                    } else if initial == ')' || initial == ']' || initial == '}' {
                        parenlev -= 1;
                    }
                    result.push(TokenInfo {
                        typ: OP,
                        string: token,
                        start: spos,
                        end: epos,
                        line: line,
                    });
                }
            } else {
                result.push(TokenInfo {
                    typ: ERRORTOKEN,
                    string: &line[line_pos..line_pos + 1],
                    start: (line_num, line_pos),
                    end: (line_num, line_pos + 1),
                    line: line,
                });
                line_pos += 1;
            }
        }
    }

    if let Some(_) = contstr_start {
        return Err("EOF in multi-line string".to_string());
    }

    if continued {
        return Err("EOF in multi-line statement".to_string());
    }

    // We use this zero-length slice as the ending content for remaining tokens.
    // This is *just in case* anyone actually cares that the location of the
    // pointer makes any kind of sense.
    let empty_end_slice = &input[input.len()..];

    if !line.is_empty() {
        let last_char = line.chars().last().unwrap();
        if last_char != '\r' && last_char != '\n' {
            result.push(TokenInfo {
                typ: NEWLINE,
                string: empty_end_slice,
                start: (line_num - 1, line.len()),
                end: (line_num - 1, line.len() + 1),
                line: empty_end_slice,
            });
        }
    }
    for _ in indents.iter().skip(1) {
        result.push(TokenInfo {
            typ: DEDENT,
            string: empty_end_slice,
            start: (line_num, 0),
            end: (line_num, 0),
            line: empty_end_slice,
        });
    }
    result.push(TokenInfo {
        typ: ENDMARKER,
        string: empty_end_slice,
        start: (line_num, 0),
        end: (line_num, 0),
        line: empty_end_slice,
    });

    Ok(result)
}
