use yultsur::yul;

pub fn untyped_identifier_expr(i: &str) -> yul::Expression {
    yul::Expression::Identifier(untyped_identifier(i))
}

pub fn untyped_identifier(i: &str) -> yul::Identifier {
    yul::Identifier {
        identifier: String::from(i),
        yultype: None,
    }
}

pub fn untyped_literal_expr(l: &str) -> yul::Expression {
    yul::Expression::Literal(untyped_literal(l))
}

pub fn untyped_literal(l: &str) -> yul::Literal {
    yul::Literal {
        literal: String::from(l),
        yultype: None,
    }
}
