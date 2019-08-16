use nom::bytes::complete::{tag, take_while1};
use nom::character::complete::{char, multispace1, space0, space1};
use nom::combinator::{opt, verify};
use nom::error::{context, ParseError};
use nom::multi::many0;
use nom::sequence::{preceded, terminated};
use nom::{Err as NomErr, IResult, InputTake};

use crate::ast::ModuleStmt::*;
use crate::ast::*;

/// Return true if the char `c` is a valid identifier character.
pub fn is_identifier_char(c: char) -> bool {
    c == '_' || c.is_ascii_alphabetic() || c.is_digit(10)
}

/// Parse an identifier i.e. `_foo`, `Foo`, `foo_Bar_1`, etc.  Identifiers may not begin with
/// numbers.
pub fn identifier<'a, E>(inp: &'a str) -> IResult<&'a str, &'a str, E>
where
    E: ParseError<&'a str>,
{
    let ident = context(
        r"expected identifier /[a-zA-Z_][a-zA-Z0-9_]*/",
        verify(take_while1(is_identifier_char), |out: &str| {
            !out.chars().next().unwrap().is_digit(10)
        }),
    );
    ident(inp)
}

/// Parse a sequence of whitespace characters that must contain at least one newline.  Remaining
/// parser input begins just after the last parsed newline.  Parser output includes all whitespace
/// chars parsed up to and including the last newline.
pub fn ws_nl<'a, E>(inp: &'a str) -> IResult<&'a str, &'a str, E>
where
    E: ParseError<&'a str>,
{
    let (_, out) = multispace1(inp)?;
    let last_newline_i = out.char_indices().rev().find(|i| i.1 == '\n');

    if let Some((i, _)) = last_newline_i {
        Ok(inp.take_split(i + 1))
    } else {
        let mut err = E::from_char(inp, '\n');
        err = E::add_context(inp, "expected at least one newline", err);

        Err(NomErr::Error(err))
    }
}

/// Parse a vyper source file into a `Module` AST object.
pub fn parse_file<'a, E>(inp: &'a str) -> IResult<&'a str, Module, E>
where
    E: ParseError<&'a str>,
{
    let mut i = inp;

    // Eat leading whitespace except whitespace on the same line as and preceding the first module
    // statement.  This edge case must be handled we or could end up parsing invalid syntax like
    // this:
    //
    //   event Greeter:  # first line of event decl is indented (bad)
    //     name: bytes32
    let (i_, _) = opt(ws_nl)(i)?;
    i = i_;

    let mut body = vec![];

    while i.len() != 0 {
        // Exit if no more content or if only remaining content is whitespace
        if i.len() == 0 {
            break;
        }
        if let Ok((i_, _)) = multispace1::<&'a str, E>(i) {
            if i_.len() == 0 {
                i = i_;
                break;
            }
        }

        // Parse next module statement.  This will and should fail if any whitespace is present on
        // the same line as the next statement.
        let (i_, next_stmt) = parse_module_stmt(i)?;
        i = i_;
        body.push(next_stmt);

        // Eat whitespace before next statement
        let (i_, _) = opt(ws_nl)(i)?;
        i = i_;
    }

    Ok((i, Module { body }))
}

/// Parse a module statement, such as an event or contract definition, into a `ModuleStmt` object.
pub fn parse_module_stmt<'a, E>(inp: &'a str) -> IResult<&'a str, ModuleStmt, E>
where
    E: ParseError<&'a str>,
{
    let (i, module_stmt) = context("expected event definition", parse_event_def)(inp)?;

    Ok((i, module_stmt))
}

/// Parse an event definition statement into a `ModuleStmt::EventDef` object.
pub fn parse_event_def<'a, E>(inp: &'a str) -> IResult<&'a str, ModuleStmt, E>
where
    E: ParseError<&'a str>,
{
    // "event" identifier ":" ws_nl
    let (i, _) = terminated(tag("event"), space1)(inp)?;
    let (i, name) = terminated(identifier, space0)(i)?;
    let (i, _) = terminated(char(':'), ws_nl)(i)?;

    // Determine indentation level
    let (_, indent) = space1(i)?;

    // <indent> event_field (ws_nl <indent> event_field)*
    let indented_field = parse_event_field(indent);
    let (i, first_field) = indented_field(i)?;
    let (i, mut other_fields) = many0(preceded(ws_nl, indented_field))(i)?;

    let mut fields = vec![first_field];
    fields.append(&mut other_fields);

    Ok((
        i,
        EventDef {
            name: name.to_string(),
            fields: fields,
        },
    ))
}

/// Parse an event field definition into an `EventField` object.
pub fn parse_event_field<'a, E>(
    indent: &'a str,
) -> impl Fn(&'a str) -> IResult<&'a str, EventField, E>
where
    E: ParseError<&'a str>,
{
    move |inp: &'a str| {
        let (i, _) = context(r"expected indentation /[ \t]+/", tag(indent))(inp)?;
        let (i, name) = terminated(identifier, space0)(i)?;
        let (i, _) = terminated(char(':'), space0)(i)?;
        let (i, typ) = identifier(i)?;

        Ok((
            i,
            EventField {
                name: name.to_string(),
                typ: typ.to_string(),
            },
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use nom::error::{ErrorKind, ErrorKind::*};

    use crate::errors::make_error;

    type SimpleError<I> = (I, ErrorKind);

    #[test]
    fn test_identifier() {
        // Success
        let examples = vec![
            ("Foo", Ok(("", "Foo"))),
            ("FooBar_Baz", Ok(("", "FooBar_Baz"))),
            ("_foo", Ok(("", "_foo"))),
            ("foo_bar", Ok(("", "foo_bar"))),
            ("_foo123  ", Ok(("  ", "_foo123"))),
        ];
        for (inp, expected) in examples {
            let actual = identifier::<SimpleError<_>>(inp);
            assert_eq!(actual, expected);
        }

        // Error
        let examples = vec![
            ("  foo", make_error("  foo", TakeWhile1)),
            ("12foo", make_error("12foo", Verify)),
            ("", make_error("", TakeWhile1)),
        ];
        for (inp, expected) in examples {
            let actual = identifier::<SimpleError<_>>(inp);
            assert_eq!(actual, expected);
        }
    }

    #[test]
    fn test_ws_nl() {
        // Success
        let examples = vec![
            ("\n", Ok(("", "\n"))),
            ("\n \n", Ok(("", "\n \n"))),
            (" \n    ", Ok(("    ", " \n"))),
            (" \n \n ", Ok((" ", " \n \n"))),
            ("  \n   \n", Ok(("", "  \n   \n"))),
            ("  \n   \n     ", Ok(("     ", "  \n   \n"))),
        ];
        for (inp, expected) in examples {
            let actual = ws_nl::<SimpleError<_>>(inp);
            assert_eq!(actual, expected);
        }

        // Error
        let examples = vec![
            ("", make_error("", MultiSpace)),
            ("  ", make_error("  ", Char)),
            ("  foo", make_error("  foo", Char)),
        ];
        for (inp, expected) in examples {
            let actual = ws_nl::<SimpleError<_>>(inp);
            assert_eq!(actual, expected);
        }
    }

    #[test]
    fn test_parse_file() {
        // Empty file
        let examples = vec!["", "  \t ", " \n\n   \t \n \t "];
        let expected: IResult<_, _, SimpleError<_>> = Ok(("", Module { body: vec![] }));

        for inp in examples {
            let actual = parse_file::<SimpleError<_>>(inp);
            assert_eq!(actual, expected);
        }

        // Test one stmt
        let examples = vec![
            // No leading or trailing whitespace
            r"event Greet:
    name: bytes32
    age: uint8",
            // Leading whitespace
            r"
event Greet:
    name: bytes32
    age: uint8",
            // Leading and trailing whitespace
            r"
event Greet:
    name: bytes32
    age: uint8
",
        ];
        let expected: IResult<_, _, SimpleError<_>> = Ok((
            "",
            Module {
                body: vec![EventDef {
                    name: "Greet".to_string(),
                    fields: vec![
                        EventField {
                            name: "name".to_string(),
                            typ: "bytes32".to_string(),
                        },
                        EventField {
                            name: "age".to_string(),
                            typ: "uint8".to_string(),
                        },
                    ],
                }],
            },
        ));
        for inp in examples {
            let actual = parse_file::<SimpleError<_>>(inp);
            assert_eq!(actual, expected);
        }

        // More than one stmt
        let examples = vec![
            // No leading, mid, or trailing whitespace
            r"event Greet:
    name: bytes32
    age: uint8
event Other:
    info1: uint256
    info2: bool",
            // Leading whitespace
            r"
event Greet:
    name: bytes32
    age: uint8
event Other:
    info1: uint256
    info2: bool",
            // Leading and trailing whitespace
            r"
event Greet:
    name: bytes32
    age: uint8
event Other:
    info1: uint256
    info2: bool
",
            // Leading, mid, and trailing whitespace
            r"
event Greet:
    name: bytes32
    age: uint8

event Other:
    info1: uint256
    info2: bool
",
        ];
        let expected: IResult<_, _, SimpleError<_>> = Ok((
            "",
            Module {
                body: vec![
                    EventDef {
                        name: "Greet".to_string(),
                        fields: vec![
                            EventField {
                                name: "name".to_string(),
                                typ: "bytes32".to_string(),
                            },
                            EventField {
                                name: "age".to_string(),
                                typ: "uint8".to_string(),
                            },
                        ],
                    },
                    EventDef {
                        name: "Other".to_string(),
                        fields: vec![
                            EventField {
                                name: "info1".to_string(),
                                typ: "uint256".to_string(),
                            },
                            EventField {
                                name: "info2".to_string(),
                                typ: "bool".to_string(),
                            },
                        ],
                    },
                ],
            },
        ));
        for inp in examples {
            let actual = parse_file::<SimpleError<_>>(inp);
            assert_eq!(actual, expected);
        }
    }
}
