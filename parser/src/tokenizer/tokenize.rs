use regex::Regex;
use str_concat::concat;

use crate::parsers::is_identifier_char;
use crate::string_utils::{lines_with_endings, rstrip_slice};
use crate::tokenizer::regex::{
    get_pseudotoken_pattern, get_single_quote_set, get_triple_quote_set, DOUBLE, DOUBLE3, SINGLE,
    SINGLE3,
};
use crate::tokenizer::types::{Position, TokenInfo, TokenType::*};

pub const TABSIZE: usize = 8;

pub fn tokenize<'a>(input: &'a str) -> Result<Vec<TokenInfo<'a>>, String> {
    // Static values/helpers
    let pseudo_token_re = Regex::new(&get_pseudotoken_pattern()).unwrap();

    let triple_quoted = get_triple_quote_set();
    let single_quoted = get_single_quote_set();

    let double3_re = Regex::new(DOUBLE3).unwrap();
    let single3_re = Regex::new(SINGLE3).unwrap();
    let double_re = Regex::new(DOUBLE).unwrap();
    let single_re = Regex::new(SINGLE).unwrap();

    // The ordering of checks matters here.  We need to eliminate the possibility of triple quote
    // endings before looking for single quote endings.
    let get_endprog = |token: &str| {
        if token.ends_with("\"\"\"") {
            &double3_re
        } else if token.ends_with("'''") {
            &single3_re
        } else if token.ends_with("\"") {
            &double_re
        } else if token.ends_with("'") {
            &single_re
        } else {
            panic!("Unrecognized quote style");
        }
    };

    // Token list result
    let mut result: Vec<TokenInfo<'a>> = Vec::new();

    // State vars
    let mut parenlev: usize = 0;
    let mut continued: bool = false;
    let mut needcont: bool = false;
    let mut indents: Vec<usize> = vec![0];

    let mut strstart: Option<Position> = None;
    let mut contstr: Option<&'a str> = None;
    let mut contline: Option<&'a str> = None;
    let mut endprog: Option<&Regex> = None;

    // Token generation loop.  We use the `loop` keyword here (instead of `for (line, lnum) in
    // ...`) so we can hold onto the iterator vars after the loop finishes.
    let mut line = &input[..1];
    let mut lnum = 0;
    let mut lines = lines_with_endings(input);
    loop {
        let next = lines.next();
        lnum += 1;
        // We use this guard style of exiting to avoid indenting the entire loop body
        if next.is_none() {
            break;
        }
        line = next.unwrap();

        let mut pos: usize = 0;
        let line_len: usize = line.len();

        if let Some(contstr_val) = contstr {
            // Continued string
            if let Some(engprog_val) = endprog {
                if let Some(endmatch) = engprog_val.find(line) {
                    let end = endmatch.end();
                    pos = end;
                    result.push(TokenInfo {
                        typ: STRING,
                        string: concat(contstr.unwrap(), &line[..end]).unwrap(),
                        start: strstart.unwrap(),
                        end: (lnum, end),
                        line: concat(contline.unwrap(), line).unwrap(),
                    });
                    contstr = None;
                    contline = None;
                    needcont = false;
                }
            } else if needcont && !line.ends_with("\\\n") && !line.ends_with("\\\r\n") {
                result.push(TokenInfo {
                    typ: ERRORTOKEN,
                    string: concat(contstr_val, line).unwrap(),
                    start: strstart.unwrap(),
                    end: (lnum, line_len),
                    line: contline.unwrap(),
                });
                contstr = None;
                contline = None;
                continue;
            } else {
                contstr = Some(concat(contstr_val, line).unwrap());
                contline = Some(concat(contline.unwrap(), line).unwrap());
                continue;
            }
        } else if parenlev == 0 && !continued {
            // New statement
            let mut column: usize = 0;

            // Measure leading whitespace
            for c in line[pos..].chars() {
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
                pos += c.len_utf8();
            }

            if pos == line_len {
                // If no more chars in line (not even newline, carriage return, etc.), we're at
                // EOF.  Break out of the token loop.
                break;
            }

            if let Some(c) = line[pos..].chars().next() {
                if c == '#' || c == '\r' || c == '\n' {
                    if c == '#' {
                        let comment_token = rstrip_slice(&line[pos..], "\r\n");
                        let comment_token_len = comment_token.len();

                        result.push(TokenInfo {
                            typ: COMMENT,
                            string: comment_token,
                            start: (lnum, pos),
                            end: (lnum, pos + comment_token_len),
                            line: line,
                        });

                        pos += comment_token_len;
                    }

                    result.push(TokenInfo {
                        typ: NL,
                        string: &line[pos..],
                        start: (lnum, pos),
                        end: (lnum, line_len),
                        line: line,
                    });

                    continue;
                }
            }

            if column > *indents.last().unwrap() {
                indents.push(column);
                result.push(TokenInfo {
                    typ: INDENT,
                    string: &line[..pos],
                    start: (lnum, 0),
                    end: (lnum, pos),
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
                    start: (lnum, pos),
                    end: (lnum, pos),
                    line: line,
                });
            }
        } else {
            continued = false;
        }

        while pos < line_len {
            if let Some(pseudomatch) = pseudo_token_re.find_at(line, pos) {
                let start = pseudomatch.start();
                let end = pseudomatch.end();

                let spos = (lnum, start);
                let epos = (lnum, end);
                pos = end;

                if start == end {
                    continue;
                }

                let token = &line[start..end];
                let initial = line[start..].chars().next().unwrap();

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
                    endprog = Some(get_endprog(token));

                    if let Some(endmatch) = endprog.unwrap().find_at(line, pos) {
                        pos = endmatch.end();
                        let token = &line[start..pos];

                        result.push(TokenInfo {
                            typ: STRING,
                            string: token,
                            start: spos,
                            end: (lnum, pos),
                            line: line,
                        });
                    } else {
                        strstart = Some((lnum, start));
                        contstr = Some(&line[start..]);
                        contline = Some(line);
                        break;
                    }
                } else if single_quoted.contains(&initial.to_string())
                    || single_quoted.contains(&token.chars().take(2).collect::<String>())
                    || single_quoted.contains(&token.chars().take(3).collect::<String>())
                {
                    if token.chars().last().unwrap() == '\n' {
                        endprog = Some(get_endprog(token));

                        strstart = Some((lnum, start));
                        contstr = Some(&line[start..]);
                        contline = Some(&line);
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
                    string: &line[pos..pos + 1],
                    start: (lnum, pos),
                    end: (lnum, pos + 1),
                    line: line,
                });
                pos += 1;
            }
        }
    }

    if let Some(_) = contstr {
        return Err("EOF in multi-line string".to_string());
    }

    // We use this zero-length slice as the ending content for remaining tokens.  This is *just in
    // case* anyone actually cares that the location of the pointer makes any kind of sense.
    let empty_end_slice = &input[input.len()..];

    if !line.is_empty() {
        let last_char = line.chars().last().unwrap();
        if last_char != '\r' && last_char != '\n' {
            result.push(TokenInfo {
                typ: NEWLINE,
                string: empty_end_slice,
                start: (lnum - 1, line.len()),
                end: (lnum - 1, line.len() + 1),
                line: empty_end_slice,
            });
        }
    }
    for _ in indents.iter().skip(1) {
        result.push(TokenInfo {
            typ: DEDENT,
            string: empty_end_slice,
            start: (lnum, 0),
            end: (lnum, 0),
            line: empty_end_slice,
        });
    }
    result.push(TokenInfo {
        typ: ENDMARKER,
        string: empty_end_slice,
        start: (lnum, 0),
        end: (lnum, 0),
        line: empty_end_slice,
    });

    Ok(result)
}
