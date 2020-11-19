use fe_parser::ast as fe;
use fe_parser::span::{
    Span,
    Spanned,
};
use fe_semantics;
use fe_semantics::namespace::types::{
    Array,
    Base,
    Map,
    Type,
};
use fe_semantics::{
    ExpressionAttributes,
    Location,
};

const GUEST_BOOK: &str = include_str!("fixtures/guest_book.fe");

fn addr_val() -> ExpressionAttributes {
    ExpressionAttributes::new(Type::Base(Base::Address), Location::Value)
}

fn bytes_sto_moved() -> ExpressionAttributes {
    let mut expression = ExpressionAttributes::new(
        Type::Array(Array {
            dimension: 100,
            inner: Base::Byte,
        }),
        Location::Storage { nonce: None },
    );

    expression.move_location = Some(Location::Memory);
    expression
}

fn bytes_mem() -> ExpressionAttributes {
    ExpressionAttributes::new(
        Type::Array(Array {
            dimension: 100,
            inner: Base::Byte,
        }),
        Location::Memory,
    )
}

fn addr_bytes_map_sto() -> ExpressionAttributes {
    ExpressionAttributes::new(
        Type::Map(Map {
            key: Base::Address,
            value: Box::new(Type::Array(Array {
                dimension: 100,
                inner: Base::Byte,
            })),
        }),
        Location::Storage { nonce: Some(0) },
    )
}

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
        (200, 210, &addr_val()),
        (214, 222, &bytes_mem()),
        (253, 261, &bytes_mem()),
        (326, 341, &addr_bytes_map_sto()),
        (326, 347, &bytes_sto_moved()),
        (342, 346, &addr_val()),
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
