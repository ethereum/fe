use crate::context::{AnalyzerContext, DiagnosticVoucher};
use crate::display::Displayable;
use crate::errors::{FatalError, TypeError};
use crate::namespace::types::{EventField, FunctionParam, Generic, Type, TypeId};
use crate::traversal::expressions::assignable_expr;
use fe_common::{diagnostics::Label, utils::humanize::pluralize_conditionally};
use fe_common::{Span, Spanned};
use fe_parser::ast as fe;
use fe_parser::node::Node;
use smol_str::SmolStr;

pub trait LabeledParameter {
    fn label(&self) -> Option<&str>;
    fn typ(&self) -> Result<TypeId, TypeError>;
}

impl LabeledParameter for FunctionParam {
    fn label(&self) -> Option<&str> {
        self.label()
    }
    fn typ(&self) -> Result<TypeId, TypeError> {
        self.typ.clone()
    }
}

impl LabeledParameter for EventField {
    fn label(&self) -> Option<&str> {
        Some(&self.name)
    }
    fn typ(&self) -> Result<TypeId, TypeError> {
        self.typ.clone()
    }
}

impl LabeledParameter for (SmolStr, Result<TypeId, TypeError>) {
    fn label(&self) -> Option<&str> {
        Some(&self.0)
    }
    fn typ(&self) -> Result<TypeId, TypeError> {
        self.1.clone()
    }
}

pub fn validate_arg_count(
    context: &mut dyn AnalyzerContext,
    name: &str,
    name_span: Span,
    args: &Node<Vec<impl Spanned>>,
    param_count: usize,
    argument_word: &str,
) -> Option<DiagnosticVoucher> {
    if args.kind.len() == param_count {
        None
    } else {
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
    }
}

// TODO: missing label error should suggest adding `_` to fn def
pub fn validate_named_args(
    context: &mut dyn AnalyzerContext,
    name: &str,
    name_span: Span,
    args: &Node<Vec<Node<fe::CallArg>>>,
    params: &[impl LabeledParameter],
) -> Result<(), FatalError> {
    validate_arg_count(context, name, name_span, args, params.len(), "argument");
    // TODO: if the first arg is missing, every other arg will get a label and type error

    for (index, (param, arg)) in params.iter().zip(args.kind.iter()).enumerate() {
        let expected_label = param.label();
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
                    context.fancy_error(
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
                    context.fancy_error(
                            "missing argument label",
                            vec![Label::primary(
                                Span::new(arg_val.span.file_id, arg_val.span.start, arg_val.span.start),
                                format!("add `{}:` here", expected_label),
                            )],
                            vec![format!(
                                "Note: this label is optional if the argument is a variable named `{}`.",
                                expected_label
                            )],
                        );
                }
            },
            (None, Some(actual_label)) => {
                context.error(
                    "argument should not be labeled",
                    actual_label.span,
                    "remove this label",
                );
            }
            (None, None) => {}
        }

        let param_type = param.typ()?;
        let val_attrs = assignable_expr(context, &arg.kind.value, Some(param_type))?;
        if !validate_arg_type(context, arg, val_attrs.typ, param_type) {
            let msg = if let Some(label) = param.label() {
                format!("incorrect type for `{}` argument `{}`", name, label)
            } else {
                format!(
                    "incorrect type for `{}` argument at position {}",
                    name, index
                )
            };
            context.type_error(&msg, arg.kind.value.span, param_type, val_attrs.typ);
        }
    }
    Ok(())
}

fn validate_arg_type(
    context: &mut dyn AnalyzerContext,
    arg: &Node<fe::CallArg>,
    arg_type: TypeId,
    param_type: TypeId,
) -> bool {
    if let Type::Generic(Generic { bounds, .. }) = param_type.typ(context.db()) {
        for bound in bounds.iter() {
            if !bound.is_implemented_for(context.db(), arg_type) {
                context.error(
                    &format!(
                        "the trait bound `{}: {}` is not satisfied",
                        arg_type.display(context.db()),
                        bound.name(context.db())
                    ),
                    arg.span,
                    &format!(
                        "the trait `{}` is not implemented for `{}`",
                        bound.name(context.db()),
                        arg_type.display(context.db()),
                    ),
                );

                return false;
            }
        }
        true
    } else {
        arg_type == param_type
    }
}
