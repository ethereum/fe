use fe_common::diagnostics::Label;
use fe_common::Span;

use crate::context::{AnalyzerContext, DiagnosticVoucher};
use crate::errors::{BinaryOperationError, NotFixedSize};
use crate::namespace::types::{FixedSize, Type};
use std::convert::TryInto;
use std::fmt::Display;

pub fn types_to_fixed_sizes(sizes: &[Type]) -> Result<Vec<FixedSize>, NotFixedSize> {
    sizes.iter().map(|param| param.clone().try_into()).collect()
}

pub fn add_bin_operations_errors(
    context: &mut dyn AnalyzerContext,
    op: &dyn Display,
    left_span: Span,
    left_type: &Type,
    right_span: Span,
    right_type: &Type,
    error: BinaryOperationError,
) -> DiagnosticVoucher {
    match error {
        BinaryOperationError::NotEqualAndUnsigned => context.fancy_error(
            &format!("`{}` operand types must be equal and unsigned", op),
            vec![
                Label::primary(left_span, &format!("this has type `{}`", left_type)),
                Label::primary(right_span, &format!("this has type `{}`", right_type)),
            ],
            vec![],
        ),
        BinaryOperationError::RightIsSigned => context.fancy_error(
            &format!(
                "The right hand side of the `{}` operation must be unsigned",
                op
            ),
            vec![Label::primary(
                right_span,
                &format!("this has signed type `{}`", right_type),
            )],
            vec![],
        ),
        BinaryOperationError::RightTooLarge => context.fancy_error(
            &format!("incompatible `{}` operand types", op),
            vec![
                Label::primary(left_span, &format!("this has type `{}`", left_type)),
                Label::primary(right_span, &format!("this has type `{}`", right_type)),
            ],
            vec![format!(
                "The type of the right hand side cannot be larger than the left (`{}`)",
                left_type
            )],
        ),
        BinaryOperationError::TypesNotEqual => context.fancy_error(
            &format!("`{}` operand types must be equal", op),
            vec![
                Label::primary(left_span, &format!("this has type `{}`", left_type)),
                Label::primary(right_span, &format!("this has type `{}`", right_type)),
            ],
            vec![],
        ),
        BinaryOperationError::TypesNotNumeric => context.fancy_error(
            &format!("`{}` operands must be numeric", op),
            vec![
                Label::primary(left_span, &format!("this has type `{}`", left_type)),
                Label::primary(right_span, &format!("this has type `{}`", right_type)),
            ],
            vec![],
        ),
    }
}
