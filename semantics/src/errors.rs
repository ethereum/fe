//! Semantic errors.

/// Errors for things that may arise in a valid Fe AST.
#[derive(Debug, PartialEq)]
pub enum SemanticError {
    MissingReturn,
    NotAnExpression,
    UnassignableExpression,
    UndefinedValue,
    UnexpectedReturn,
    TypeError,
}
