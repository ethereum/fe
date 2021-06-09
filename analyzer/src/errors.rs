//! Semantic errors.

use fe_common::diagnostics::Diagnostic;

/// Error to be returned from APIs that should reject duplicate definitions
#[derive(Debug)]
pub struct AlreadyDefined;

/// Error to be returned when otherwise no meaningful information can be returned
#[derive(Debug)]
pub struct FatalError;

/// Error indicating that a value can not move between memory and storage
#[derive(Debug)]
pub struct CannotMove;

/// Error indicating that a value has the wrong type
#[derive(Debug)]
pub struct TypeError;

/// Errors that can result from indexing
#[derive(Debug, PartialEq)]
pub enum IndexingError {
    WrongIndexType,
    NotSubscriptable,
}

/// Errors that can result from a binary operation
#[derive(Debug, PartialEq)]
pub enum BinaryOperationError {
    TypesNotEqual,
    TypesNotNumeric,
    RightTooLarge,
    RightIsSigned,
    NotEqualAndUnsigned,
}

#[derive(Debug)]
pub struct AnalyzerError(pub Vec<Diagnostic>);
