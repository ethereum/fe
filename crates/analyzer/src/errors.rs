//! Semantic errors.

use fe_common::diagnostics::{Diagnostic, Label, Severity};
use fe_common::Span;
use std::fmt::Display;
use std::marker::PhantomData;

/// Error to be returned from APIs that should reject duplicate definitions
#[derive(Debug)]
pub struct AlreadyDefined(pub Span);

/// Error to be returned when otherwise no meaningful information can be returned
#[derive(Debug)]
pub struct FatalError(PhantomData<()>);

impl FatalError {
    /// Create a `FatalError` instance.
    pub fn new() -> FatalError {
        // This is primarly a hook for debugging. If we hit a FatalError for which we haven't
        // yet assigned a proper user error, we can place a panic here to see where the fatal
        // error originates.
        FatalError(PhantomData::default())
    }
}

impl Default for FatalError {
    fn default() -> Self {
        Self::new()
    }
}

/// Error indicating that a value can not move between memory and storage
#[derive(Debug)]
pub struct CannotMove;

/// Error indicating that a type is invalid. An error diagnostic should be emitted
/// prior to returning this error type.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct TypeError;

/// Error indicating that a [`Type`] can't be converted into a [`FixedSize`]
#[derive(Debug)]
pub struct NotFixedSize;

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

impl From<TypeError> for FatalError {
    fn from(_: TypeError) -> Self {
        Self::new()
    }
}

pub fn error(message: impl Into<String>, label_span: Span, label: impl Into<String>) -> Diagnostic {
    fancy_error(message, vec![Label::primary(label_span, label)], vec![])
}

pub fn fancy_error(
    message: impl Into<String>,
    labels: Vec<Label>,
    notes: Vec<String>,
) -> Diagnostic {
    Diagnostic {
        severity: Severity::Error,
        message: message.into(),
        labels,
        notes,
    }
}

pub fn type_error(
    message: impl Into<String>,
    span: Span,
    expected: impl Display,
    actual: impl Display,
) -> Diagnostic {
    error(
        message,
        span,
        format!("this has type `{}`; expected type `{}`", actual, expected),
    )
}

pub fn not_yet_implemented(feature: impl Display, span: Span) -> Diagnostic {
    error(
        format!("feature not yet implemented: {}", feature),
        span,
        "not yet implemented",
    )
}

pub fn duplicate_name_error(
    message: &str,
    name: &str,
    original: Span,
    duplicate: Span,
) -> Diagnostic {
    fancy_error(
        message,
        vec![
            Label::primary(original, format!("`{}` first defined here", name)),
            Label::secondary(duplicate, format!("`{}` redefined here", name)),
        ],
        vec![],
    )
}
