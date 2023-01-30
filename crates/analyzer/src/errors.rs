//! Semantic errors.

use crate::context::{DiagnosticVoucher, NamedThing};
use fe_common::diagnostics::{Diagnostic, Label, Severity};
use fe_common::Span;
use std::fmt::Display;

/// Error indicating that a type is invalid.
///
/// Note that the "type" of a thing (eg the type of a `FunctionParam`)
/// in [`crate::namespace::types`] is sometimes represented as a
/// `Result<Type, TypeError>`.
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
/// Example: `TypeError::new(context.error("something is wrong", some_span, "this thing"))`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeError(DiagnosticVoucher);
impl TypeError {
    // `Clone` is required because these are stored in a salsa db.
    // Please don't clone these manually.
    pub fn new(voucher: DiagnosticVoucher) -> Self {
        Self(voucher)
    }
}

impl From<FatalError> for TypeError {
    fn from(err: FatalError) -> Self {
        Self(err.0)
    }
}
impl From<ConstEvalError> for TypeError {
    fn from(err: ConstEvalError) -> Self {
        Self(err.0)
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

impl From<ConstEvalError> for FatalError {
    fn from(err: ConstEvalError) -> Self {
        Self(err.0)
    }
}

impl From<AlreadyDefined> for FatalError {
    fn from(err: AlreadyDefined) -> Self {
        Self(err.0)
    }
}

/// Error indicating constant evaluation failed.
///
/// This error emitted when
/// 1. an expression can't be evaluated in compilation time
/// 2. arithmetic overflow occurred during evaluation
/// 3. zero division is detected during evaluation
///
/// Can't be created unless a diagnostic has been emitted, and thus a [`DiagnosticVoucher`]
/// has been obtained. (See comment on [`TypeError`])
///
/// NOTE: `Clone` is required because these are stored in a salsa db.
/// Please don't clone these manually.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ConstEvalError(DiagnosticVoucher);

impl ConstEvalError {
    pub fn new(voucher: DiagnosticVoucher) -> Self {
        Self(voucher)
    }
}

impl From<TypeError> for ConstEvalError {
    fn from(err: TypeError) -> Self {
        Self(err.0)
    }
}

impl From<FatalError> for ConstEvalError {
    fn from(err: FatalError) -> Self {
        Self(err.0)
    }
}

impl From<IncompleteItem> for ConstEvalError {
    fn from(err: IncompleteItem) -> Self {
        Self(err.0)
    }
}

/// Error returned by `ModuleId::resolve_name` if the name is not found, and parsing of the module
/// failed. In this case, emitting an error message about failure to resolve the name might be misleading,
/// because the file may in fact contain an item with the given name, somewhere after the syntax error that caused
/// parsing to fail.
#[derive(Debug)]
pub struct IncompleteItem(DiagnosticVoucher);
impl IncompleteItem {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self(DiagnosticVoucher::assume_the_parser_handled_it())
    }
}

/// Error to be returned from APIs that should reject duplicate definitions
#[derive(Debug)]
pub struct AlreadyDefined(DiagnosticVoucher);
impl AlreadyDefined {
    #[allow(clippy::new_without_default)]
    pub fn new(voucher: DiagnosticVoucher) -> Self {
        Self(voucher)
    }
}

/// Errors that can result from indexing
#[derive(Debug, PartialEq, Eq)]
pub enum IndexingError {
    WrongIndexType,
    NotSubscriptable,
}

/// Errors that can result from a binary operation
#[derive(Debug, PartialEq, Eq)]
pub enum BinaryOperationError {
    TypesNotCompatible,
    TypesNotNumeric,
    RightTooLarge,
    RightIsSigned,
    NotEqualAndUnsigned,
}

/// Errors that can result from an implicit type coercion
#[derive(Debug, PartialEq, Eq)]
pub enum TypeCoercionError {
    /// Value is in storage and must be explicitly moved with .to_mem()
    RequiresToMem,
    /// Value type cannot be coerced to the expected type
    Incompatible,
    /// `self` contract used where an external contract value is expected
    SelfContractType,
}

impl From<TypeError> for FatalError {
    fn from(err: TypeError) -> Self {
        Self::new(err.0)
    }
}

impl From<IncompleteItem> for FatalError {
    fn from(err: IncompleteItem) -> Self {
        Self::new(err.0)
    }
}

impl From<IncompleteItem> for TypeError {
    fn from(err: IncompleteItem) -> Self {
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
        format!("this has type `{actual}`; expected type `{expected}`"),
    )
}

pub fn not_yet_implemented(feature: impl Display, span: Span) -> Diagnostic {
    error(
        format!("feature not yet implemented: {feature}"),
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
            Label::primary(original, format!("`{name}` first defined here")),
            Label::secondary(duplicate, format!("`{name}` redefined here")),
        ],
        vec![],
    )
}

pub fn name_conflict_error(
    name_kind: &str, // Eg "function parameter" or "variable name"
    name: &str,
    original: &NamedThing,
    original_span: Option<Span>,
    duplicate_span: Span,
) -> Diagnostic {
    if let Some(original_span) = original_span {
        fancy_error(
            format!(
                "{} name `{}` conflicts with previously defined {}",
                name_kind,
                name,
                original.item_kind_display_name()
            ),
            vec![
                Label::primary(original_span, format!("`{name}` first defined here")),
                Label::secondary(duplicate_span, format!("`{name}` redefined here")),
            ],
            vec![],
        )
    } else {
        fancy_error(
            format!(
                "{} name `{}` conflicts with built-in {}",
                name_kind,
                name,
                original.item_kind_display_name()
            ),
            vec![Label::primary(
                duplicate_span,
                format!(
                    "`{}` is a built-in {}",
                    name,
                    original.item_kind_display_name()
                ),
            )],
            vec![],
        )
    }
}

pub fn to_mem_error(span: Span) -> Diagnostic {
    fancy_error(
        "value must be copied to memory",
        vec![Label::primary(span, "this value is in storage")],
        vec![
            "Hint: values located in storage can be copied to memory using the `to_mem` function."
                .into(),
            "Example: `self.my_array.to_mem()`".into(),
        ],
    )
}
pub fn self_contract_type_error(span: Span, typ: &dyn Display) -> Diagnostic {
    fancy_error(
        format!("`self` can't be used where a contract of type `{typ}` is expected",),
        vec![Label::primary(span, "cannot use `self` here")],
        vec![format!(
            "Hint: Values of type `{typ}` represent external contracts.\n\
             To treat `self` as an external contract, use `{typ}(ctx.self_address())`."
        )],
    )
}
