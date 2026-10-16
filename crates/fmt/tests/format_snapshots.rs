use dir_test::{Fixture, dir_test};
use fe_fmt::{Config, format_str};
use parser::{RecoveryMode, SyntaxKind, SyntaxNode, parse_source_file};
use test_utils::snap_test;

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/tests/fixtures",
    glob: "*.fe"
)]
fn format_snap(fixture: Fixture<&str>) {
    let config = Config::default();
    let output = match format_str(fixture.content(), &config) {
        Ok(formatted) => formatted,
        Err(err) => format!("FORMAT ERROR: {err:?}"),
    };

    snap_test!(output, fixture.path());
}

#[test]
fn long_bodiless_signatures_keep_uses_on_their_line() {
    // A line break ends a signature without a body, so `uses` can't move to
    // the next line as it does before a body.
    let source = "trait Call {\n    fn call(self, _ a0: own A0, _ a1: own A1, _ a2: own A2) -> Self::Out uses E\n}\n";
    assert_formats_to(source, source);
}

#[test]
fn deeply_nested_tuples_format_in_linear_time() {
    // Each tuple level used to format its elements twice, so 40 levels took
    // about 2^40 steps.
    let depth = 40;
    let nested = |inner: &str, rest: &str| {
        format!(
            "{}{inner}{}",
            "(".repeat(depth),
            format!(", {rest})").repeat(depth)
        )
    };
    let source = format!(
        "fn f() {{\n    let {}: {} = {}\n}}\n",
        nested("x", "_"),
        nested("u256", "u8"),
        nested("1", "0"),
    );
    let config = Config::default();
    let formatted = format_str(&source, &config).expect("format should succeed");
    assert_eq!(
        formatted,
        format_str(&formatted, &config).expect("reformat should succeed")
    );
}

#[test]
fn comments_between_methods_format_in_one_pass() {
    let config = Config::default();
    for modifier in ["pub ", "unsafe ", "pub unsafe "] {
        for newlines in [1, 2, 4] {
            for comment in ["", "    // Clear state.\n    // Avoid another callback.\n"] {
                let separator = "\n".repeat(newlines);
                let source = format!(
                    "impl Notifications {{\n    {modifier}fn unsubscribe(self) uses (log: mut Log) {{\n        log.emit(0)\n    }}{separator}{comment}    {modifier}fn detach(self) uses (log: mut Log) {{\n        log.emit(1)\n    }}\n}}\n"
                );
                let formatted = format_str(&source, &config).expect("format should succeed");
                assert_eq!(
                    formatted,
                    format_str(&formatted, &config).expect("reformat should succeed"),
                    "{source}",
                );
                let separator = "\n".repeat(newlines.min(2));
                assert!(
                    formatted.contains(&format!("}}{separator}{comment}    {modifier}fn detach")),
                    "{formatted}",
                );
                assert!(formatted.starts_with("impl Notifications {\n    "));
            }
        }
    }
}

#[test]
fn ambiguous_binary_operators_break_after_the_operator() {
    let source = r#"
fn calculate() {
    let product = very_long_left_side_expression_name * very_long_right_side_expression_name_that_is_even_longer
    let difference = very_long_left_side_expression_name - very_long_right_side_expression_name_that_is_even_longer
}
"#;
    let formatted = format_str(source, &Config::default()).expect("format should succeed");

    assert!(
        formatted.contains("very_long_left_side_expression_name *\n"),
        "{formatted}",
    );
    assert!(
        formatted.contains("very_long_left_side_expression_name -\n"),
        "{formatted}",
    );

    let (green, errors) = parse_source_file(&formatted, RecoveryMode::NoRecover);
    assert!(errors.is_empty(), "{errors:#?}\n{formatted}");

    let syntax = SyntaxNode::new_root(green);
    assert_eq!(
        syntax
            .descendants()
            .filter(|node| node.kind() == SyntaxKind::BinExpr)
            .count(),
        2,
        "{syntax:#?}",
    );
    assert!(
        syntax
            .descendants()
            .all(|node| node.kind() != SyntaxKind::UnExpr),
        "{syntax:#?}",
    );
}

#[test]
fn nested_dereferences_remain_lexically_separate() {
    let source = r#"
fn load(pointer: **u256) -> u256 {
    * *pointer
}
"#;
    let formatted = format_str(source, &Config::default()).expect("format should succeed");

    assert!(formatted.contains("* *pointer"), "{formatted}");
    assert_eq!(
        format_str(&formatted, &Config::default()).expect("reformat should succeed"),
        formatted,
    );

    let (green, errors) = parse_source_file(&formatted, RecoveryMode::NoRecover);
    assert!(errors.is_empty(), "{errors:#?}\n{formatted}");

    let syntax = SyntaxNode::new_root(green);
    assert_eq!(
        syntax
            .descendants()
            .filter(|node| node.kind() == SyntaxKind::UnExpr)
            .count(),
        2,
        "{syntax:#?}",
    );
}

#[test]
fn bare_qualified_first_generic_arguments_do_not_become_shifts() {
    let source = r#"
fn wrap(_ x: Wrapped< <T as Model>>, _ y: Wrapped< <T as Model>::Point>) {
    let w = make< <T as Model>>(x)
}
"#;
    let formatted = format_str(source, &Config::default()).expect("format should succeed");

    assert!(formatted.contains("Wrapped< <T as Model> >"), "{formatted}");
    assert!(formatted.contains("make< <T as Model> >(x)"), "{formatted}");
    assert!(
        formatted.contains("Wrapped<<T as Model>::Point>"),
        "{formatted}"
    );
    assert_eq!(
        format_str(&formatted, &Config::default()).expect("reformat should succeed"),
        formatted,
    );

    let (green, errors) = parse_source_file(&formatted, RecoveryMode::NoRecover);
    assert!(errors.is_empty(), "{errors:#?}\n{formatted}");

    let syntax = SyntaxNode::new_root(green);
    assert_eq!(
        syntax
            .descendants()
            .filter(|node| node.kind() == SyntaxKind::GenericArgList)
            .filter(|node| node
                .parent()
                .is_some_and(|parent| parent.kind() == SyntaxKind::PathSegment))
            .count(),
        3,
        "{syntax:#?}",
    );
    assert!(
        syntax
            .descendants()
            .all(|node| node.kind() != SyntaxKind::BinExpr),
        "{syntax:#?}",
    );
}

#[test]
fn const_where_predicates_survive_formatting_in_source_order() {
    fn predicates(source: &str) -> Vec<(SyntaxKind, String)> {
        let (green, errors) = parse_source_file(source, RecoveryMode::NoRecover);
        assert!(errors.is_empty(), "{errors:?}\n{source}");
        SyntaxNode::new_root(green)
            .descendants()
            .filter(|node| {
                matches!(
                    node.kind(),
                    SyntaxKind::WherePredicate | SyntaxKind::WhereConstPredicate
                )
            })
            .map(|node| {
                (
                    node.kind(),
                    node.descendants_with_tokens()
                        .filter_map(|element| element.into_token())
                        .filter(|token| {
                            !matches!(token.kind(), SyntaxKind::WhiteSpace | SyntaxKind::Newline)
                        })
                        .map(|token| token.text().to_string())
                        .collect::<Vec<_>>()
                        .join(" "),
                )
            })
            .collect()
    }
    let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/const_where_predicates.fe");
    let source = std::fs::read_to_string(path)
        .expect("fixture should be readable")
        .replace("\r\n", "\n");
    let formatted = format_str(&source, &Config::default()).unwrap();
    assert_eq!(predicates(&source), predicates(&formatted), "{formatted}");
    assert_eq!(
        formatted,
        format_str(&formatted, &Config::default()).unwrap()
    );
    assert!(formatted.contains("// keep this condition"), "{formatted}");
}

/// Inner attributes of a module and attributes on `for` loops are kept, and
/// a multiline string argument keeps its whitespace. This source is inline
/// rather than a fixture because the tree-sitter grammar, which parses every
/// formatter fixture, does not accept these attribute positions yet.
#[test]
fn inner_and_loop_attributes_survive_formatting() {
    let source = r#"mod inner {
    #![arithmetic(unchecked)]

    pub(ingot) fn sum() -> u256 {
        var total: u256 = 0
        #[unroll]
        for i in 0..4 {
            total += i
        }
        total
    }
}

fn commented_loop() {
    // comment before the loop
    #[unroll]
    for i in 0..2 { // trailing comment
        let _ = i
    }
}

mod strings {
    #![example(text = "first
  second

    third")]
    #![other]

    fn looped() {
        #[example(text = "one
   two")]
        for i in 0..2 {
            let _ = i
        }
    }
}
"#;
    let expected = r#"mod inner {
    #![arithmetic(unchecked)]

    pub(ingot) fn sum() -> u256 {
        var total: u256 = 0
        #[unroll]
        for i in 0 .. 4 {
            total += i
        }
        total
    }
}

fn commented_loop() {
    // comment before the loop
    #[unroll]
    for i in 0 .. 2 {
        // trailing comment
        let _ = i
    }
}

mod strings {
    #![example(text = "first
  second

    third")]
    #![other]

    fn looped() {
        #[example(text = "one
   two")]
        for i in 0 .. 2 {
            let _ = i
        }
    }
}
"#;
    let formatted = format_str(source, &Config::default()).expect("format should succeed");
    assert_eq!(formatted.trim_end(), expected.trim_end());
    let reformatted = format_str(&formatted, &Config::default()).expect("reformat should succeed");
    assert_eq!(reformatted, formatted, "formatting is not stable");
}

/// Formats `source`, checks the result against `expected`, and checks that it
/// parses and formats to itself. For syntax that the tree-sitter grammar does
/// not accept yet, which rules out a file in `tests/fixtures`.
fn assert_formats_to(source: &str, expected: &str) {
    let formatted = format_str(source, &Config::default()).expect("format should succeed");
    assert_eq!(formatted, expected);

    let (_, errors) = parse_source_file(&formatted, RecoveryMode::NoRecover);
    assert!(errors.is_empty(), "{errors:#?}\n{formatted}");
    assert_eq!(
        format_str(&formatted, &Config::default()).expect("reformat should succeed"),
        formatted,
    );
}

#[test]
fn for_loop_attributes_are_kept() {
    let source = r#"
fn sum(xs: [u256; 4]) -> u256 {
    var total: u256 = 0
    #[unroll(never)]
    for x in xs {
        total += x
    }
    #[unroll]
       #[unroll(never)]
    for   x   in   xs {}
    for x in xs {
        #[unroll(never)]
        for y in xs {
            // body comment
        }
    }
    total
}
"#;
    let expected = r#"fn sum(xs: [u256; 4]) -> u256 {
    var total: u256 = 0
    #[unroll(never)]
    for x in xs {
        total += x
    }
    #[unroll]
    #[unroll(never)]
    for x in xs {}
    for x in xs {
        #[unroll(never)]
        for y in xs {
            // body comment
        }
    }
    total
}
"#;
    assert_formats_to(source, expected);
}

#[test]
fn nested_module_inner_attributes_are_kept() {
    let source = r#"#![arithmetic(checked)]

mod math {
    #![arithmetic(unchecked)]

    pub fn wrap() -> u8 {
        let x: u8 = 255
        x + 1
    }
}

mod tight {   #![arithmetic(unchecked)]
    fn f() {}
}

mod outer {
    #![arithmetic(unchecked)]
    #![payable]


    mod inner {
        #![arithmetic(checked)]
        fn g() {}
    }
}

mod only_attrs {
    #![arithmetic(unchecked)]
}
"#;
    let expected = r#"#![arithmetic(checked)]

mod math {
    #![arithmetic(unchecked)]

    pub fn wrap() -> u8 {
        let x: u8 = 255
        x + 1
    }
}

mod tight {
    #![arithmetic(unchecked)]
    fn f() {}
}

mod outer {
    #![arithmetic(unchecked)]
    #![payable]

    mod inner {
        #![arithmetic(checked)]
        fn g() {}
    }
}

mod only_attrs {
    #![arithmetic(unchecked)]
}
"#;
    assert_formats_to(source, expected);
}

#[test]
fn comments_among_inner_and_loop_attributes_are_kept() {
    let source = r#"mod m {
    #![arithmetic(unchecked)]
    // Comment inside the inner attributes.
    #![payable]
    // Comment on the first item.
    fn f(xs: [u8; 2]) {
        #[unroll(never)]
        // Note on the loop.
        for x in xs {}
    }
}
"#;
    assert_formats_to(source, source);
}
