//! Semantic errors.

use ansi_term::Color::Red;
use fe_common::diagnostics::Diagnostic;
use fe_parser::node::Span;

/// Error to be returned from APIs that should reject duplicate definitions
#[derive(Debug)]
pub struct AlreadyDefined;

#[derive(Debug)]
pub struct AnalyzerError {
    pub diagnostics: Vec<Diagnostic>,
    pub classic: Option<SemanticError>,
}

/// Errors for things that may arise in a valid Fe AST.
#[derive(Debug, PartialEq)]
pub enum ErrorKind {
    CannotMove,
    NotCallable,
    NotSubscriptable,
    SignedExponentNotAllowed,
    TypeError,
    UndefinedValue,
    Fatal,
}

#[derive(Debug, PartialEq)]
pub struct SemanticError {
    pub kind: ErrorKind,
    /// A sequence of nested spans containing the error's origin in the source
    /// code.
    pub context: Vec<Span>,
}

impl SemanticError {
    pub fn fatal() -> Self {
        SemanticError {
            kind: ErrorKind::Fatal,
            context: vec![],
        }
    }

    /// Create a new error with kind `NotSubscriptable`
    pub fn not_subscriptable() -> Self {
        SemanticError {
            kind: ErrorKind::NotSubscriptable,
            context: vec![],
        }
    }

    /// Create a new error with kind `SignedExponentNotAllowed`
    pub fn signed_exponent_not_allowed() -> Self {
        SemanticError {
            kind: ErrorKind::SignedExponentNotAllowed,
            context: vec![],
        }
    }

    /// Create a new error with kind `UndefinedValue`
    pub fn undefined_value() -> Self {
        SemanticError {
            kind: ErrorKind::UndefinedValue,
            context: vec![],
        }
    }

    /// Create a new error with kind `TypeError`
    pub fn type_error() -> Self {
        SemanticError {
            kind: ErrorKind::TypeError,
            context: vec![],
        }
    }

    /// Create a new error with kind `CannotMove`
    pub fn cannot_move() -> Self {
        SemanticError {
            kind: ErrorKind::CannotMove,
            context: vec![],
        }
    }

    /// Create a new error with kind `NotCallable`
    pub fn not_callable() -> Self {
        SemanticError {
            kind: ErrorKind::NotCallable,
            context: vec![],
        }
    }

    /// Maps the error to a new error that contains the given span in its
    /// context.
    pub fn with_context(mut self, span: Span) -> Self {
        self.context.push(span);
        self
    }

    /// Formats the error using the source code.
    ///
    /// The string will contain the error kind, line number, and surrounding
    /// code.
    pub fn format_user(&self, src: &str) -> String {
        let line = if let Some(span) = self.context.first() {
            src[..span.start].lines().count()
        } else {
            0
        };

        let context = match (self.context.get(0), self.context.get(1)) {
            (Some(inner), Some(outer)) => {
                let first_part = src[outer.start..inner.start].to_string();
                let middle_part = Red.paint(&src[inner.start..inner.end]).to_string();
                let last_part = src[inner.end..outer.end].to_string();

                format!("{}{}{}", first_part, middle_part, last_part)
            }
            (Some(span), None) => src[span.start..span.end].to_string(),
            (_, _) => "no error context available".to_string(),
        };

        format!("{:?} on line {}\n{}", self.kind, line, context)
    }
}
