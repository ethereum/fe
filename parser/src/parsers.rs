use nom::bytes::complete::{tag, take_while1};
use nom::character::complete::{char, line_ending, multispace0, space0, space1};
use nom::multi::many0;
use nom::sequence::terminated;
use nom::IResult;

use crate::ast::ModuleStmt::*;
use crate::ast::*;

pub fn is_symbol_start(c: char) -> bool {
    c == '_' || c.is_ascii_alphabetic()
}

pub fn is_symbol_char(c: char) -> bool {
    c == '_' || c.is_ascii_alphabetic() || c.is_digit(10)
}

pub fn symbol(i: &str) -> IResult<&str, &str> {
    take_while1(is_symbol_start)(i)?;
    take_while1(is_symbol_char)(i)
}

pub fn symbol_token(i: &str) -> IResult<&str, &str> {
    terminated(symbol, space0)(i)
}

pub fn char_token(c: char) -> impl Fn(&str) -> IResult<&str, char> {
    move |i: &str| terminated(char(c), space0)(i)
}

pub fn tag_token(t: &'static str) -> impl Fn(&str) -> IResult<&str, &str> {
    move |i: &str| terminated(tag(t), space0)(i)
}

pub fn parse_module(i: &str) -> IResult<&str, Module> {
    let (i, _) = multispace0(i)?;
    let (i, body) = many0(parse_module_stmt)(i)?;

    Ok((i, Module { body }))
}

pub fn parse_module_stmt(i: &str) -> IResult<&str, ModuleStmt> {
    let (i, event_def) = parse_event_def(i)?;

    Ok((i, event_def))
}

pub fn parse_event_def(i: &str) -> IResult<&str, ModuleStmt> {
    let (i, _) = terminated(tag("event"), space1)(i)?;
    let (i, name) = terminated(symbol, space0)(i)?;
    let (i, _) = terminated(char(':'), space0)(i)?;
    let (i, _) = line_ending(i)?;

    let (i, _) = tag("    ")(i)?;
    let (i, field) = parse_event_field(i)?;

    Ok((
        i,
        EventDef {
            name: name.to_string(),
            fields: vec![field],
        },
    ))
}

pub fn parse_event_field(i: &str) -> IResult<&str, EventField> {
    let (i, name) = terminated(symbol, space0)(i)?;
    let (i, _) = terminated(char(':'), space0)(i)?;
    let (i, typ) = terminated(symbol, space0)(i)?;

    Ok((
        i,
        EventField {
            name: name.to_string(),
            typ: typ.to_string(),
        },
    ))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_module() {
        assert_eq!(
            parse_module(
                r###"
event Greet:
    name: bytes32"###
            ),
            Ok((
                "",
                Module {
                    body: vec![EventDef {
                        name: "Greet".to_string(),
                        fields: vec![EventField {
                            name: "name".to_string(),
                            typ: "bytes32".to_string(),
                        },],
                    }]
                }
            )),
        );
    }
}
