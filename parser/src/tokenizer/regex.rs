use regex::Regex;

fn group(choices: &[&str]) -> String {
    ["(", &choices.join("|"), ")"].concat()
}

fn any(choices: &[&str]) -> String {
    [&group(choices), "*"].concat()
}

fn maybe(choices: &[&str]) -> String {
    [&group(choices), "?"].concat()
}

pub const WHITESPACE: &str = r"[ \f\t]*";
pub const COMMENT: &str = r"#[^\r\n]*";

/// Whitespace + any(r"\\\r?\n" + Whitespace) + maybe(Comment)
pub fn get_ignore_pattern() -> String {
    let any_part = &[r"\\\r?\n", WHITESPACE].concat();

    [WHITESPACE, &any(&[any_part]), &maybe(&[COMMENT])].concat()
}

pub const NAME: &str = r"\w+";

pub const HEXNUMBER: &str = r"0[xX](?:_?[0-9a-fA-F])+";
pub const BINNUMBER: &str = r"0[bB](?:_?[01])+";
pub const OCTNUMBER: &str = r"0[oO](?:_?[0-7])+";
pub const DECNUMBER: &str = r"(?:0(?:_?0)*|[1-9](?:_?[0-9])*)";

/// INTNUMBER = group(Hexnumber, Binnumber, Octnumber, Decnumber)
pub fn get_intnumber_pattern() -> String {
    group(&[HEXNUMBER, BINNUMBER, OCTNUMBER, DECNUMBER])
}

pub const EXPONENT: &str = r"[eE][-+]?[0-9](?:_?[0-9])*";

/// POINTFLOAT = group(
///     r"[0-9](?:_?[0-9])*\.(?:[0-9](?:_?[0-9])*)?",
///     r"\.[0-9](?:_?[0-9])*",
/// ) + maybe(EXPONENT)
pub fn get_pointfloat_pattern() -> String {
    [
        group(&[
            r"[0-9](?:_?[0-9])*\.(?:[0-9](?:_?[0-9])*)?",
            r"\.[0-9](?:_?[0-9])*",
        ]),
        maybe(&[EXPONENT]),
    ]
    .concat()
}

/// EXPFLOAT = r"[0-9](?:_?[0-9])*" + EXPONENT
pub fn get_expfloat_pattern() -> String {
    [r"[0-9](?:_?[0-9])*", EXPONENT].concat()
}

/// FLOATNUMBER = group(POINTFLOAT, EXPFLOAT)
pub fn get_floatnumber_pattern() -> String {
    group(&[&get_pointfloat_pattern(), &get_expfloat_pattern()])
}

/// IMAGNUMBER = group(r"[0-9](?:_?[0-9])*[jJ]", FLOATNUMBER + r"[jJ]")
pub fn get_imagnumber_pattern() -> String {
    group(&[
        r"[0-9](?:_?[0-9])*[jJ]",
        &[&get_floatnumber_pattern(), r"[jJ]"].concat(),
    ])
}

/// NUMBER = group(IMAGNUMBER, FLOATNUMBER, INTNUMBER)
pub fn get_number_pattern() -> String {
    group(&[
        &get_imagnumber_pattern(),
        &get_floatnumber_pattern(),
        &get_intnumber_pattern(),
    ])
}

pub const VALID_STRING_PREFIXES: &[&str] = &[
    "b", "B", "r", "R", "u", "U", "f", "F", "br", "BR", "bR", "Br", "fr", "FR", "fR", "Fr", "rb",
    "RB", "Rb", "rB", "rf", "RF", "Rf", "rF",
];

/// STRINGPREFIX = group(VALID_STRING_PREFIXES)
pub fn get_stringprefix_pattern() -> String {
    group(VALID_STRING_PREFIXES)
}

// Tail end of ' string
pub const SINGLE: &str = r###"
[^'\\]*(?:\\.[^'\\]*)*'
"###;

// Tail end of " string
pub const DOUBLE: &str = r###"
[^"\\]*(?:\\.[^"\\]*)*"
"###;

// Tail end of ''' string
pub const SINGLE3: &str = r###"
[^'\\]*(?:(?:\\.|'(?!''))[^'\\]*)*'''
"###;

// Tail end of """ string
pub const DOUBLE3: &str = r###"
[^"\\]*(?:(?:\\.|"(?!""))[^"\\]*)*"""
"###;

/// TRIPLE = group(STRINGPREFIX + "'''", STRINGPREFIX + '"""')
pub fn get_triple_pattern() -> String {
    let stringprefix = &get_stringprefix_pattern();
    group(&[
        &[stringprefix, "'''"].concat(),
        &[stringprefix, "\"\"\""].concat(),
    ])
}
/// Single-line ' or " string.
/// STRING = group(STRINGPREFIX + r"'[^\n'\\]*(?:\\.[^\n'\\]*)*'",
///                STRINGPREFIX + r'"[^\n"\\]*(?:\\.[^\n"\\]*)*"')
pub fn get_string_pattern() -> String {
    let stringprefix = &get_stringprefix_pattern();
    group(&[
        &[stringprefix, r###"'[^\n'\\]*(?:\\.[^\n'\\]*)*'"###].concat(),
        &[stringprefix, r###""[^\n"\\]*(?:\\.[^\n"\\]*)*""###].concat(),
    ])
}

/// Because of leftmost-then-longest match semantics, be sure to put the longest operators first
/// (e.g., if = came before ==, == would get recognized as two instances of =).
/// OPERATOR = group(r"\*\*=?", r">>=?", r"<<=?", r"!=",
///                 r"//=?", r"->",
///                 r"[+\-*/%&@|^=<>]=?",
///                 r"~")
pub fn get_operator_pattern() -> String {
    group(&[
        r"\*\*=?",
        r">>=?",
        r"<<=?",
        r"!=",
        r"//=?",
        r"->",
        r"[+\-*/%&@|^=<>]=?",
        r"~",
    ])
}

pub const BRACKET: &str = r"[][(){}]";

/// SPECIAL = group(r'\r?\n', r'\.\.\.', r'[:;.,@]')
pub fn get_special_pattern() -> String {
    group(&[r"\r?\n", r"\.\.\.", r"[:;.,@]"])
}

/// FUNNY = group(OPERATOR, BRACKET, SPECIAL)
pub fn get_funny_pattern() -> String {
    group(&[&get_operator_pattern(), BRACKET, &get_special_pattern()])
}

/// PLAINTOKEN = group(NUMBER, FUNNY, STRING, NAME)
pub fn get_plaintoken_pattern() -> String {
    group(&[
        &get_number_pattern(),
        &get_funny_pattern(),
        &get_string_pattern(),
        NAME,
    ])
}

/// TOKEN = IGNORE + PLAINTOKEN
pub fn get_token_pattern() -> String {
    [get_ignore_pattern(), get_plaintoken_pattern()].concat()
}

/// First (or only) line of ' or " string.
/// ContStr = group(StringPrefix + r"'[^\n'\\]*(?:\\.[^\n'\\]*)*" +
///                 group("'", r'\\\r?\n'),
///                 StringPrefix + r'"[^\n"\\]*(?:\\.[^\n"\\]*)*' +
///                 group('"', r'\\\r?\n'))
pub fn get_contstr_pattern() -> String {
    let stringprefix_pattern = &get_stringprefix_pattern();
    group(&[
        &[
            stringprefix_pattern,
            r###"'[^\n'\\]*(?:\\.[^\n'\\]*)*"###,
            &group(&["'", r"\\\r?\n"]),
        ]
        .concat(),
        &[
            stringprefix_pattern,
            r###""[^\n"\\]*(?:\\.[^\n"\\]*)*"###,
            &group(&["\"", r"\\\r?\n"]),
        ]
        .concat(),
    ])
}
/// PSEUDOEXTRAS = group(r'\\\r?\n|\Z', COMMENT, TRIPLE)
pub fn get_pseudoextras_pattern() -> String {
    group(&[r"\\\r?\n|\Z", COMMENT, &get_triple_pattern()])
}

/// PSEUDOTOKEN = WHITESPACE + group(PSEUDOEXTRAS, NUMBER, FUNNY, CONTSTR, NAME)
pub fn get_pseudotoken_pattern() -> String {
    [
        WHITESPACE,
        &group(&[
            &get_pseudoextras_pattern(),
            &get_number_pattern(),
            &get_funny_pattern(),
            &get_contstr_pattern(),
            NAME,
        ]),
    ]
    .concat()
}

pub fn get_string_end_regex(token: &str) -> Regex {
    if token.ends_with("\"\"\"") {
        Regex::new(DOUBLE3).unwrap()
    } else if token.ends_with("'''") {
        Regex::new(SINGLE3).unwrap()
    } else if token.ends_with("\"") {
        Regex::new(DOUBLE).unwrap()
    } else if token.ends_with("'") {
        Regex::new(SINGLE).unwrap()
    } else {
        panic!("Unrecognized quote style");
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_regex_patterns() {
        assert_eq!(
            get_ignore_pattern(),
            "[ \\f\\t]*(\\\\\\r?\\n[ \\f\\t]*)*(#[^\\r\\n]*)?",
        );
        assert_eq!(
            get_intnumber_pattern(),
            "(0[xX](?:_?[0-9a-fA-F])+|0[bB](?:_?[01])+|0[oO](?:_?[0-7])+|(?:0(?:_?0)*|[1-9](?:_?[0-9])*))",
        );
        assert_eq!(
            get_pointfloat_pattern(),
            "([0-9](?:_?[0-9])*\\.(?:[0-9](?:_?[0-9])*)?|\\.[0-9](?:_?[0-9])*)([eE][-+]?[0-9](?:_?[0-9])*)?",
        );
        assert_eq!(
            get_expfloat_pattern(),
            "[0-9](?:_?[0-9])*[eE][-+]?[0-9](?:_?[0-9])*",
        );
        assert_eq!(
            get_floatnumber_pattern(),
            "(([0-9](?:_?[0-9])*\\.(?:[0-9](?:_?[0-9])*)?|\\.[0-9](?:_?[0-9])*)([eE][-+]?[0-9](?:_?[0-9])*)?|[0-9](?:_?[0-9])*[eE][-+]?[0-9](?:_?[0-9])*)",
        );
        assert_eq!(
            get_imagnumber_pattern(),
            "([0-9](?:_?[0-9])*[jJ]|(([0-9](?:_?[0-9])*\\.(?:[0-9](?:_?[0-9])*)?|\\.[0-9](?:_?[0-9])*)([eE][-+]?[0-9](?:_?[0-9])*)?|[0-9](?:_?[0-9])*[eE][-+]?[0-9](?:_?[0-9])*)[jJ])",
        );
        assert_eq!(
            get_number_pattern(),
            "(([0-9](?:_?[0-9])*[jJ]|(([0-9](?:_?[0-9])*\\.(?:[0-9](?:_?[0-9])*)?|\\.[0-9](?:_?[0-9])*)([eE][-+]?[0-9](?:_?[0-9])*)?|[0-9](?:_?[0-9])*[eE][-+]?[0-9](?:_?[0-9])*)[jJ])|(([0-9](?:_?[0-9])*\\.(?:[0-9](?:_?[0-9])*)?|\\.[0-9](?:_?[0-9])*)([eE][-+]?[0-9](?:_?[0-9])*)?|[0-9](?:_?[0-9])*[eE][-+]?[0-9](?:_?[0-9])*)|(0[xX](?:_?[0-9a-fA-F])+|0[bB](?:_?[01])+|0[oO](?:_?[0-7])+|(?:0(?:_?0)*|[1-9](?:_?[0-9])*)))",
        );
        assert_eq!(
            get_stringprefix_pattern(),
            "(b|B|r|R|u|U|f|F|br|BR|bR|Br|fr|FR|fR|Fr|rb|RB|Rb|rB|rf|RF|Rf|rF)",
        );
        assert_eq!(
            get_triple_pattern(),
            "((b|B|r|R|u|U|f|F|br|BR|bR|Br|fr|FR|fR|Fr|rb|RB|Rb|rB|rf|RF|Rf|rF)\'\'\'|(b|B|r|R|u|U|f|F|br|BR|bR|Br|fr|FR|fR|Fr|rb|RB|Rb|rB|rf|RF|Rf|rF)\"\"\")",
        );
        assert_eq!(
            get_string_pattern(),
            "((b|B|r|R|u|U|f|F|br|BR|bR|Br|fr|FR|fR|Fr|rb|RB|Rb|rB|rf|RF|Rf|rF)\'[^\\n\'\\\\]*(?:\\\\.[^\\n\'\\\\]*)*\'|(b|B|r|R|u|U|f|F|br|BR|bR|Br|fr|FR|fR|Fr|rb|RB|Rb|rB|rf|RF|Rf|rF)\"[^\\n\"\\\\]*(?:\\\\.[^\\n\"\\\\]*)*\")",
        );
        assert_eq!(
            get_operator_pattern(),
            "(\\*\\*=?|>>=?|<<=?|!=|//=?|->|[+\\-*/%&@|^=<>]=?|~)",
        );
        assert_eq!(get_special_pattern(), "(\\r?\\n|\\.\\.\\.|[:;.,@])",);
        assert_eq!(
            get_funny_pattern(),
            "((\\*\\*=?|>>=?|<<=?|!=|//=?|->|[+\\-*/%&@|^=<>]=?|~)|[][(){}]|(\\r?\\n|\\.\\.\\.|[:;.,@]))",
        );
        assert_eq!(
            get_plaintoken_pattern(),
            "((([0-9](?:_?[0-9])*[jJ]|(([0-9](?:_?[0-9])*\\.(?:[0-9](?:_?[0-9])*)?|\\.[0-9](?:_?[0-9])*)([eE][-+]?[0-9](?:_?[0-9])*)?|[0-9](?:_?[0-9])*[eE][-+]?[0-9](?:_?[0-9])*)[jJ])|(([0-9](?:_?[0-9])*\\.(?:[0-9](?:_?[0-9])*)?|\\.[0-9](?:_?[0-9])*)([eE][-+]?[0-9](?:_?[0-9])*)?|[0-9](?:_?[0-9])*[eE][-+]?[0-9](?:_?[0-9])*)|(0[xX](?:_?[0-9a-fA-F])+|0[bB](?:_?[01])+|0[oO](?:_?[0-7])+|(?:0(?:_?0)*|[1-9](?:_?[0-9])*)))|((\\*\\*=?|>>=?|<<=?|!=|//=?|->|[+\\-*/%&@|^=<>]=?|~)|[][(){}]|(\\r?\\n|\\.\\.\\.|[:;.,@]))|((b|B|r|R|u|U|f|F|br|BR|bR|Br|fr|FR|fR|Fr|rb|RB|Rb|rB|rf|RF|Rf|rF)\'[^\\n\'\\\\]*(?:\\\\.[^\\n\'\\\\]*)*\'|(b|B|r|R|u|U|f|F|br|BR|bR|Br|fr|FR|fR|Fr|rb|RB|Rb|rB|rf|RF|Rf|rF)\"[^\\n\"\\\\]*(?:\\\\.[^\\n\"\\\\]*)*\")|\\w+)",
        );
        assert_eq!(
            get_token_pattern(),
            "[ \\f\\t]*(\\\\\\r?\\n[ \\f\\t]*)*(#[^\\r\\n]*)?((([0-9](?:_?[0-9])*[jJ]|(([0-9](?:_?[0-9])*\\.(?:[0-9](?:_?[0-9])*)?|\\.[0-9](?:_?[0-9])*)([eE][-+]?[0-9](?:_?[0-9])*)?|[0-9](?:_?[0-9])*[eE][-+]?[0-9](?:_?[0-9])*)[jJ])|(([0-9](?:_?[0-9])*\\.(?:[0-9](?:_?[0-9])*)?|\\.[0-9](?:_?[0-9])*)([eE][-+]?[0-9](?:_?[0-9])*)?|[0-9](?:_?[0-9])*[eE][-+]?[0-9](?:_?[0-9])*)|(0[xX](?:_?[0-9a-fA-F])+|0[bB](?:_?[01])+|0[oO](?:_?[0-7])+|(?:0(?:_?0)*|[1-9](?:_?[0-9])*)))|((\\*\\*=?|>>=?|<<=?|!=|//=?|->|[+\\-*/%&@|^=<>]=?|~)|[][(){}]|(\\r?\\n|\\.\\.\\.|[:;.,@]))|((b|B|r|R|u|U|f|F|br|BR|bR|Br|fr|FR|fR|Fr|rb|RB|Rb|rB|rf|RF|Rf|rF)\'[^\\n\'\\\\]*(?:\\\\.[^\\n\'\\\\]*)*\'|(b|B|r|R|u|U|f|F|br|BR|bR|Br|fr|FR|fR|Fr|rb|RB|Rb|rB|rf|RF|Rf|rF)\"[^\\n\"\\\\]*(?:\\\\.[^\\n\"\\\\]*)*\")|\\w+)",
        );
        assert_eq!(
            get_contstr_pattern(),
            "((b|B|r|R|u|U|f|F|br|BR|bR|Br|fr|FR|fR|Fr|rb|RB|Rb|rB|rf|RF|Rf|rF)\'[^\\n\'\\\\]*(?:\\\\.[^\\n\'\\\\]*)*(\'|\\\\\\r?\\n)|(b|B|r|R|u|U|f|F|br|BR|bR|Br|fr|FR|fR|Fr|rb|RB|Rb|rB|rf|RF|Rf|rF)\"[^\\n\"\\\\]*(?:\\\\.[^\\n\"\\\\]*)*(\"|\\\\\\r?\\n))",
        );
        assert_eq!(
            get_pseudoextras_pattern(),
            "(\\\\\\r?\\n|\\Z|#[^\\r\\n]*|((b|B|r|R|u|U|f|F|br|BR|bR|Br|fr|FR|fR|Fr|rb|RB|Rb|rB|rf|RF|Rf|rF)\'\'\'|(b|B|r|R|u|U|f|F|br|BR|bR|Br|fr|FR|fR|Fr|rb|RB|Rb|rB|rf|RF|Rf|rF)\"\"\"))",
        );
        assert_eq!(
            get_pseudotoken_pattern(),
            "[ \\f\\t]*((\\\\\\r?\\n|\\Z|#[^\\r\\n]*|((b|B|r|R|u|U|f|F|br|BR|bR|Br|fr|FR|fR|Fr|rb|RB|Rb|rB|rf|RF|Rf|rF)\'\'\'|(b|B|r|R|u|U|f|F|br|BR|bR|Br|fr|FR|fR|Fr|rb|RB|Rb|rB|rf|RF|Rf|rF)\"\"\"))|(([0-9](?:_?[0-9])*[jJ]|(([0-9](?:_?[0-9])*\\.(?:[0-9](?:_?[0-9])*)?|\\.[0-9](?:_?[0-9])*)([eE][-+]?[0-9](?:_?[0-9])*)?|[0-9](?:_?[0-9])*[eE][-+]?[0-9](?:_?[0-9])*)[jJ])|(([0-9](?:_?[0-9])*\\.(?:[0-9](?:_?[0-9])*)?|\\.[0-9](?:_?[0-9])*)([eE][-+]?[0-9](?:_?[0-9])*)?|[0-9](?:_?[0-9])*[eE][-+]?[0-9](?:_?[0-9])*)|(0[xX](?:_?[0-9a-fA-F])+|0[bB](?:_?[01])+|0[oO](?:_?[0-7])+|(?:0(?:_?0)*|[1-9](?:_?[0-9])*)))|((\\*\\*=?|>>=?|<<=?|!=|//=?|->|[+\\-*/%&@|^=<>]=?|~)|[][(){}]|(\\r?\\n|\\.\\.\\.|[:;.,@]))|((b|B|r|R|u|U|f|F|br|BR|bR|Br|fr|FR|fR|Fr|rb|RB|Rb|rB|rf|RF|Rf|rF)\'[^\\n\'\\\\]*(?:\\\\.[^\\n\'\\\\]*)*(\'|\\\\\\r?\\n)|(b|B|r|R|u|U|f|F|br|BR|bR|Br|fr|FR|fR|Fr|rb|RB|Rb|rB|rf|RF|Rf|rF)\"[^\\n\"\\\\]*(?:\\\\.[^\\n\"\\\\]*)*(\"|\\\\\\r?\\n))|\\w+)",
        );
    }
}
