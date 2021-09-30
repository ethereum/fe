use crate::context::{AnalyzerContext, DiagnosticVoucher};
use crate::errors::{FatalError, TypeError};
use crate::namespace::scopes::BlockScope;
use crate::namespace::types::{EventField, FixedSize, FunctionParam};
use crate::traversal::expressions::assignable_expr;
use fe_common::{diagnostics::Label, utils::humanize::pluralize_conditionally};
use fe_common::{Span, Spanned};
use fe_parser::ast as fe;
use fe_parser::node::Node;

pub trait LabeledParameter {
    fn label(&self) -> Option<&str>;
    fn typ(&self) -> Result<FixedSize, TypeError>;
}

impl LabeledParameter for FunctionParam {
    fn label(&self) -> Option<&str> {
        Some(&self.name)
    }
    fn typ(&self) -> Result<FixedSize, TypeError> {
        self.typ.clone()
    }
}

impl LabeledParameter for EventField {
    fn label(&self) -> Option<&str> {
        Some(&self.name)
    }
    fn typ(&self) -> Result<FixedSize, TypeError> {
        self.typ.clone()
    }
}

// impl LabeledParameter for StructField {
//     fn label(&self) -> Option<&str> {
//         Some(&self.name)
//     }
//     fn typ(&self) -> Result<FixedSize, TypeError> {
//         self.typ.clone()
//     }
// }

impl LabeledParameter for (String, Result<FixedSize, TypeError>) {
    fn label(&self) -> Option<&str> {
        Some(&self.0)
    }
    fn typ(&self) -> Result<FixedSize, TypeError> {
        self.1.clone()
    }
}

pub fn validate_named_args(
    scope: &mut BlockScope,
    name: &str,
    name_span: Span,
    args: &Node<Vec<Node<fe::CallArg>>>,
    params: &[impl LabeledParameter],
    label_policy: LabelPolicy,
) -> Result<(), FatalError> {
    validate_arg_count(scope, name, name_span, args, params.len(), "argument");
    validate_arg_labels(scope, args, params, label_policy);
    validate_arg_types(scope, name, args, params)?;
    Ok(())
}

pub fn validate_arg_count(
    context: &mut dyn AnalyzerContext,
    name: &str,
    name_span: Span,
    args: &Node<Vec<impl Spanned>>,
    param_count: usize,
    argument_word: &str,
) -> Option<DiagnosticVoucher> {
    if args.kind.len() != param_count {
        let mut labels = vec![Label::primary(
            name_span,
            format!(
                "expects {} {}",
                param_count,
                pluralize_conditionally(argument_word, param_count)
            ),
        )];
        if args.kind.is_empty() {
            labels.push(Label::secondary(args.span, "supplied 0 arguments"));
        } else {
            for arg in &args.kind {
                labels.push(Label::secondary(arg.span(), ""));
            }
            labels.last_mut().unwrap().message = format!(
                "supplied {} {}",
                args.kind.len(),
                pluralize_conditionally(argument_word, args.kind.len())
            );
        }

        Some(context.fancy_error(
            &format!(
                "`{}` expects {} {}, but {} {} provided",
                name,
                param_count,
                pluralize_conditionally(argument_word, param_count),
                args.kind.len(),
                pluralize_conditionally(("was", "were"), args.kind.len())
            ),
            labels,
            vec![],
        ))
        // TODO: add `defined here` label (need span for definition)
    } else {
        None
    }
}

pub enum LabelPolicy {
    AllowAnyUnlabeled,
    AllowUnlabledIfNameEqual,
}

pub fn validate_arg_labels(
    scope: &mut BlockScope,
    args: &Node<Vec<Node<fe::CallArg>>>,
    params: &[impl LabeledParameter],
    label_policy: LabelPolicy,
) {
    for (expected_label, arg) in params
        .iter()
        .map(|param| param.label())
        .zip(args.kind.iter())
    {
        let arg_val = &arg.kind.value;
        match (expected_label, &arg.kind.label) {
            (Some(expected_label), Some(actual_label)) => {
                if expected_label != actual_label.kind {
                    let notes = if params
                        .iter()
                        .any(|param| param.label() == Some(actual_label.kind.as_str()))
                    {
                        vec!["Note: arguments must be provided in order.".into()]
                    } else {
                        vec![]
                    };
                    scope.fancy_error(
                        "argument label mismatch",
                        vec![Label::primary(
                            actual_label.span,
                            format!("expected `{}`", expected_label),
                        )],
                        notes,
                    );
                }
            }
            (Some(expected_label), None) => match &arg_val.kind {
                fe::Expr::Name(var_name) if var_name == expected_label => {}
                _ => {
                    if let LabelPolicy::AllowUnlabledIfNameEqual = label_policy {
                        scope.fancy_error(
                            "missing argument label",
                            vec![Label::primary(
                                Span::new(arg_val.span.start, arg_val.span.start),
                                format!("add `{}=` here", expected_label),
                            )],
                            vec![format!(
                                "Note: this label is optional if the argument is a variable named `{}`.",
                                expected_label
                            )],
                        );
                    }
                }
            },
            (None, Some(actual_label)) => {
                scope.error(
                    "argument should not be labeled",
                    actual_label.span,
                    "remove this label",
                );
            }
            (None, None) => {}
        }
    }
}

pub fn validate_arg_types(
    scope: &mut BlockScope,
    name: &str,
    args: &Node<Vec<Node<fe::CallArg>>>,
    params: &[impl LabeledParameter],
) -> Result<(), FatalError> {
    for (index, (param, arg)) in params.iter().zip(args.kind.iter()).enumerate() {
        let param_type = param.typ()?;
        let val_attrs = assignable_expr(scope, &arg.kind.value, Some(&param_type.clone().into()))?;
        if param_type != val_attrs.typ {
            let msg = if let Some(label) = param.label() {
                format!("incorrect type for `{}` argument `{}`", name, label)
            } else {
                format!(
                    "incorrect type for `{}` argument at position {}",
                    name, index
                )
            };
            scope.type_error(&msg, arg.kind.value.span, &param_type, &val_attrs.typ);
        }
    }
    Ok(())
}
