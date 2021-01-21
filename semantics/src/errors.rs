//! Semantic errors.

use ansi_term::Color::Red;
use fe_parser::span::Span;

/// Errors for things that may arise in a valid Fe AST.
#[derive(Debug, PartialEq)]
pub enum ErrorKind {
    BreakWithoutLoop,
    ContinueWithoutLoop,
    MissingReturn,
    NotSubscriptable,
    NumericCapacityMismatch,
    StringCapacityMismatch,
    UndefinedValue,
    UnexpectedReturn,
    TypeError,
    CannotMove,
    NotCallable,
    NumericLiteralExpected,
    MoreThanThreeIndexedParams,
    WrongNumberOfParams,
}

#[derive(Debug, PartialEq)]
pub struct SemanticError {
    pub kind: ErrorKind,
    /// A sequence of nested spans containing the error's origin in the source
    /// code.
    pub context: Vec<Span>,
}

impl SemanticError {
    /// Create a new error with kind `BreakWithoutLoop`
    pub fn break_without_loop() -> Self {
        SemanticError {
            kind: ErrorKind::BreakWithoutLoop,
            context: vec![],
        }
    }

    /// Create a new error with kind `ContinueWithoutLoop`
    pub fn continue_without_loop() -> Self {
        SemanticError {
            kind: ErrorKind::ContinueWithoutLoop,
            context: vec![],
        }
    }

    /// Create a new error with kind `MissingReturn`
    pub fn missing_return() -> Self {
        SemanticError {
            kind: ErrorKind::MissingReturn,
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

    /// Create a new error with kind `NumericCapacityMismatch`
    pub fn numeric_capacity_mismatch() -> Self {
        SemanticError {
            kind: ErrorKind::NumericCapacityMismatch,
            context: vec![],
        }
    }

    /// Create a new error with kind `NumericCapacityMismatch`
    pub fn string_capacity_mismatch() -> Self {
        SemanticError {
            kind: ErrorKind::StringCapacityMismatch,
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

    /// Create a new error with kind `UnexpectedReturn`
    pub fn unexpected_return() -> Self {
        SemanticError {
            kind: ErrorKind::UnexpectedReturn,
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

    /// Create a new error with kind `NumericLiteralExpected`
    pub fn numeric_literal_expected() -> Self {
        SemanticError {
            kind: ErrorKind::NumericLiteralExpected,
            context: vec![],
        }
    }

    /// Create a new error with kind `MoreThanThreeIndexedParams`
    pub fn more_than_three_indexed_params() -> Self {
        SemanticError {
            kind: ErrorKind::MoreThanThreeIndexedParams,
            context: vec![],
        }
    }

    /// Create a new error with kind `WrongNumberOfParams`
    pub fn wrong_number_of_params() -> Self {
        SemanticError {
            kind: ErrorKind::WrongNumberOfParams,
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
    pub fn format_with_src(&self, src: &str) -> String {
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
