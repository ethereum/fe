use fe_parser::ast as fe;
use fe_parser::span::{
    Span,
    Spanned,
};
use fe_semantics;
use fe_semantics::namespace::types::{
    Array,
    Base,
    FixedSize,
    Map,
    Type,
};
use fe_semantics::{
    ExpressionAttributes,
    Location,
};

const GUEST_BOOK: &str = include_str!("fixtures/guest_book.fe");

static ADDR_VAL: ExpressionAttributes = ExpressionAttributes {
    typ: Type::Base(Base::Address),
    location: Location::Value,
};

static BYTES_MEM: ExpressionAttributes = ExpressionAttributes {
    typ: Type::Array(Array {
        dimension: 100,
        inner: Base::Byte,
    }),
    location: Location::Memory,
};

static ADDR_BYTES_MAP_STO: ExpressionAttributes = ExpressionAttributes {
    typ: Type::Map(Map {
        key: FixedSize::Base(Base::Address),
        value: FixedSize::Array(Array {
            dimension: 100,
            inner: Base::Byte,
        }),
    }),
    location: Location::Storage,
};

fn mock_spanned_expr(start: usize, end: usize) -> Spanned<fe::Expr<'static>> {
    Spanned {
        node: fe::Expr::Name("foo"),
        span: Span { start, end },
    }
}

fn mock_spanned_func_stmt(start: usize, end: usize) -> Spanned<fe::FuncStmt<'static>> {
    Spanned {
        node: fe::FuncStmt::Expr {
            value: fe::Expr::Name("foo"),
        },
        span: Span { start, end },
    }
}

#[test]
fn guest_book_analysis() {
    let tokens = fe_parser::get_parse_tokens(GUEST_BOOK).expect("Couldn't parse expression");
    let fe_module = fe_parser::parsers::file_input(&tokens[..])
        .expect("failed to parse guest book")
        .1
        .node;

    let context = fe_semantics::analysis(&fe_module).expect("failed to perform semantic analysis");

    for (start, end, expected) in &[
        (200, 210, &ADDR_VAL),
        (214, 222, &BYTES_MEM),
        (253, 261, &BYTES_MEM),
        (326, 341, &ADDR_BYTES_MAP_STO),
        (326, 347, &BYTES_MEM),
        (342, 346, &ADDR_VAL),
    ] {
        assert_eq!(
            context.get_expression(&mock_spanned_expr(*start, *end)),
            Some(*expected),
        );
    }

    let actual_event = context
        .get_emit(&mock_spanned_func_stmt(232, 262))
        .expect("couldn't find event for emit");
    assert_eq!(
        actual_event.topic,
        "0xf95318ba442251854c1277ed370e2adb2cd6dc2156bdffdd75dc5d798b31ab0f"
    );
}
