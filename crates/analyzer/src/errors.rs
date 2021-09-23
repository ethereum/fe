//! Semantic errors.

use crate::context::DiagnosticVoucher;
use fe_common::diagnostics::{Diagnostic, Label, Severity};
use fe_common::Span;
use std::fmt::Display;

/// Error indicating that a type is invalid.
///
/// Note that the "type" of a thing (eg the type of a `FunctionParam`)
/// in [`crate::namespace::types`] is sometimes represented as a
/// `Result<FixedSize, TypeError>`.
///
/// If, for example, a function parameter has an undefined type, we emit a [`Diagnostic`] message,
/// give that parameter a "type" of `Err(TypeError)`, and carry on. If/when that parameter is
/// used in the function body, we assume that a diagnostic message about the undefined type
/// has already been emitted, and halt the analysis of the function body.
///
/// To ensure that that assumption is sound, a diagnostic *must* be emitted before creating
/// a `TypeError`. So that the rust compiler can help us enforce this rule, a `TypeError`
/// cannot be constructed without providing a [`DiagnosticVoucher`]. A voucher can be obtained
/// by calling an error function on an [`AnalyzerContext`](crate::context::AnalyzerContext).
/// Please don't try to work around this restriction.
///
/// # Example
/// ```ignore
/// pub fn check_something(context: &mut dyn AnalyzerContext, span: Span) -> Result<(), TypeError> {
///     // check failed! emit a diagnostic and return an error
///     let voucher = context.error("something is wrong", span, "this");
///     Err(TypeError::new(voucher))
/// }
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeError(DiagnosticVoucher);
impl TypeError {
    // `Clone` is required because these are stored in a salsa db.
    // Please don't clone these manually.

    pub fn new(voucher: DiagnosticVoucher) -> Self {
        Self(voucher)
    }
}

/// Error to be returned when otherwise no meaningful information can be returned.
/// Can't be created unless a diagnostic has been emitted, and thus a [`DiagnosticVoucher`]
/// has been obtained. (See comment on [`TypeError`])
#[derive(Debug)]
pub struct FatalError(DiagnosticVoucher);

impl FatalError {
    /// Create a `FatalError` instance, given a "voucher"
    /// obtained by emitting an error via an [`AnalyzerContext`](crate::context::AnalyzerContext).
    pub fn new(voucher: DiagnosticVoucher) -> Self {
        Self(voucher)
    }
}

/// Error to be returned from APIs that should reject duplicate definitions
#[derive(Debug)]
pub struct AlreadyDefined;

/// Error indicating that a value can not move between memory and storage
#[derive(Debug)]
pub struct CannotMove;

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
    fn from(err: TypeError) -> Self {
        Self::new(err.0)
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
