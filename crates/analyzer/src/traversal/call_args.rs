use super::expressions::{expr, expr_type};
use super::types::try_coerce_type;
use crate::context::{AnalyzerContext, DiagnosticVoucher};
use crate::display::Displayable;
use crate::errors::{self, FatalError, TypeCoercionError, TypeError};
use crate::namespace::types::{FunctionParam, Generic, Type, TypeId};
use fe_common::{diagnostics::Label, utils::humanize::pluralize_conditionally};
use fe_common::{Span, Spanned};
use fe_parser::ast as fe;
use fe_parser::node::Node;
use smol_str::SmolStr;

pub trait LabeledParameter {
    fn label(&self) -> Option<&str>;
    fn typ(&self) -> Result<TypeId, TypeError>;
    fn is_sink(&self) -> bool;
}

impl LabeledParameter for FunctionParam {
    fn label(&self) -> Option<&str> {
        self.label()
    }
    fn typ(&self) -> Result<TypeId, TypeError> {
        self.typ.clone()
    }
    fn is_sink(&self) -> bool {
        false
    }
}

impl LabeledParameter for (SmolStr, Result<TypeId, TypeError>, bool) {
    fn label(&self) -> Option<&str> {
        Some(&self.0)
    }
    fn typ(&self) -> Result<TypeId, TypeError> {
        self.1.clone()
    }
    fn is_sink(&self) -> bool {
        self.2
    }
}

impl LabeledParameter for (Option<SmolStr>, Result<TypeId, TypeError>, bool) {
    fn label(&self) -> Option<&str> {
        self.0.as_ref().map(smol_str::SmolStr::as_str)
    }
    fn typ(&self) -> Result<TypeId, TypeError> {
        self.1.clone()
    }
    fn is_sink(&self) -> bool {
        self.2
    }
}

pub fn validate_arg_count(
    context: &dyn AnalyzerContext,
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
    // TODO: if the first arg is missing, every other arg will get a label and type
    // error

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
                            format!("expected `{expected_label}`"),
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
                                format!("add `{expected_label}:` here"),
                            )],
                            vec![format!(
                                "Note: this label is optional if the argument is a variable named `{expected_label}`."
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
        // Check arg type
        let arg_type =
            if let Type::Generic(Generic { bounds, .. }) = param_type.deref_typ(context.db()) {
                let arg_type = expr_type(context, &arg.kind.value)?;
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
                    }
                }
                arg_type
            } else {
                let arg_attr = expr(context, &arg.kind.value, Some(param_type))?;
                match try_coerce_type(
                    context,
                    Some(&arg.kind.value),
                    arg_attr.typ,
                    param_type,
                    param.is_sink(),
                ) {
                    Err(TypeCoercionError::Incompatible) => {
                        let msg = if let Some(label) = param.label() {
                            format!("incorrect type for `{name}` argument `{label}`")
                        } else {
                            format!("incorrect type for `{name}` argument at position {index}")
                        };
                        context.type_error(&msg, arg.kind.value.span, param_type, arg_attr.typ);
                    }
                    Err(TypeCoercionError::RequiresToMem) => {
                        context.add_diagnostic(errors::to_mem_error(arg.span));
                    }
                    Err(TypeCoercionError::SelfContractType) => {
                        context.add_diagnostic(errors::self_contract_type_error(
                            arg.span,
                            &param_type.display(context.db()),
                        ));
                    }
                    Ok(_) => {}
                }
                arg_attr.typ
            };

        if param_type.is_mut(context.db()) && !arg_type.is_mut(context.db()) {
            let msg = if let Some(label) = param.label() {
                format!("`{name}` argument `{label}` must be mutable")
            } else {
                format!("`{name}` argument at position {index} must be mutable")
            };
            context.error(&msg, arg.kind.value.span, "is not `mut`");
        }
    }
    Ok(())
}
