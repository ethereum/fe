//! Semantic errors.

/// Errors for things that may arise in a valid Fe AST.
#[derive(Debug)]
pub enum SemanticError {
    UnassignableExpression,
    UnrecognizedValue,
    InvalidDeclaration,
    InvalidType,
    NotIdexable,
}
