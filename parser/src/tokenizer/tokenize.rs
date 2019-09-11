use regex::Regex;
use str_concat::concat;

use crate::string_utils::{lines_with_endings, rstrip_slice};
use crate::tokenizer::types::{Position, TokenInfo, TokenType};

const TABSIZE: usize = 8;

fn tokenize<'a>(input: &'a str) -> Result<Vec<TokenInfo<'a>>, String> {
    let mut result: Vec<TokenInfo<'a>> = Vec::new();

    // State vars
    let mut parenlev: usize = 0;
    let mut continued: bool = false;
    let mut needcont: bool = false;
    let mut indents: Vec<usize> = vec![0];

    let mut contstr: Option<&'a str> = None;
    let mut contline: Option<&'a str> = None;
    let mut endprog: Option<&Regex> = None;

    let mut strstart: Option<Position> = None;

    let mut last_line: Option<&'a str> = None;
    for (line, lnum) in lines_with_endings(input).zip(1..) {
        let mut pos: usize = 0;
        let line_len: usize = line.len();

        if let Some(contstr_val) = contstr {
            // Continued string
            if let Some(engprog_val) = endprog {
                if let Some(mtch) = engprog_val.find(line) {
                    let end = mtch.end();
                    pos = end;
                    result.push(TokenInfo {
                        typ: TokenType::String,
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
                    typ: TokenType::ErrorToken,
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
                        result.push(TokenInfo {
                            typ: TokenType::Comment,
                            string: comment_token,
                            start: (lnum, pos),
                            end: (lnum, pos + comment_token.len()),
                            line: line,
                        });
                        pos += comment_token.len();
                    }

                    result.push(TokenInfo {
                        typ: TokenType::NL,
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
                    typ: TokenType::Indent,
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
                    typ: TokenType::Dedent,
                    string: &line[line_len..],
                    start: (lnum, pos),
                    end: (lnum, pos),
                    line: line,
                });
            }
        } else {
            continued = false;
        }

        last_line = Some(line);
    }

    if let Some(_) = contstr {
        return Err("EOF in multi-line string".to_string());
    }

    Ok(result)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tokenize() {
        let tokens = tokenize(
            r"
# Test comment
class TestClassDefinition(WithParent1, WithParent2):
    foo = 'bar'

    def foo():
        return 'bar'

test = 'foo'
",
        );

        let serialized = serde_json::to_string_pretty(&tokens).unwrap();

        //assert_eq!(
        //    vec![TokenInfo {
        //        typ: TokenType::Comment,
        //        string: "Test",
        //        start: 0,
        //        end: 4,
        //        line: 0,
        //    }]
        //);
    }
}
