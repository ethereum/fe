use crate::context::{AnalyzerContext, BindingMutability, Location, NamedThing};
use crate::errors::FatalError;
use crate::namespace::scopes::BlockScope;
use crate::operations;
use crate::traversal::expressions;
use crate::traversal::utils::add_bin_operations_errors;
use fe_common::diagnostics::Label;
use fe_parser::ast as fe;
use fe_parser::node::{Node, Span};
use smol_str::SmolStr;

/// Gather context information for assignments and check for type errors.
///
/// e.g. `foo[42] = "bar"`, `self.foo[42] = "bar"`, `foo = 42`
pub fn assign(scope: &mut BlockScope, stmt: &Node<fe::FuncStmt>) -> Result<(), FatalError> {
    let (target, rhs) = match &stmt.kind {
        fe::FuncStmt::Assign { target, value } => (target, value),
        _ => unreachable!(),
    };

    let target_attr = expressions::expr(scope, target, None)?;
    let rhs_attr = expressions::expr(scope, rhs, Some(&target_attr.typ))?;

    check_assign_target(scope, target)?;

    if target_attr.typ != rhs_attr.typ {
        scope.fancy_error(
            "mismatched types",
            vec![
                Label::primary(target.span, format!("this has type `{}`", target_attr.typ)),
                Label::secondary(
                    rhs.span,
                    format!("this value has incompatible type `{}`", rhs_attr.typ),
                ),
            ],
            vec![],
        );
    }

    if target_attr.mutable
        && !rhs_attr.mutable
        && !matches!(target_attr.location, Location::Storage { .. })
        && !matches!(rhs_attr.location, Location::Storage { .. })
    {
        scope.fancy_error(
            "can't create a mutable alias of an immutable value",
            vec![
                Label::primary(target.span, "this is mutable"),
                Label::secondary(rhs.span, "this is immutable"),
            ],
            vec![],
        );
    }

    if matches!(
        (target_attr.location, rhs_attr.final_location(),),
        (Location::Memory, Location::Storage { .. })
    ) {
        scope.fancy_error(
            "location mismatch",
            vec![
                Label::primary(target.span, "this variable is located in memory"),
                Label::secondary(rhs.span, "this value is located in storage"),
            ],
            vec!["Hint: values located in storage can be copied to memory using the `to_mem` function.".into(),
                 "Example: `self.my_array.to_mem()`".into(),
            ],
        );
    }

    Ok(())
}

fn find_name_def(scope: &BlockScope, expr: &Node<fe::Expr>) -> Option<(SmolStr, Span)> {
    match &expr.kind {
        fe::Expr::Attribute { value, .. } | fe::Expr::Subscript { value, .. } => {
            find_name_def(scope, value)
        }
        fe::Expr::Name(name) => {
            let thing = scope.resolve_name(name, expr.span).ok()??;
            thing.name_span(scope.db()).map(|span| (name.clone(), span))
        }
        _ => None,
    }
}

fn check_assign_target(scope: &mut BlockScope, target: &Node<fe::Expr>) -> Result<(), FatalError> {
    use fe::Expr::*;

    match &target.kind {
        Attribute { value: container, .. } | Subscript { value: container, .. } => {
            if !scope.expr_is_mutable(container) {
                let mut labels = vec![Label::primary(container.span, "not mutable")];
                if let Some((name, span)) = find_name_def(scope, container) {
                    labels.push(Label::secondary(span, &format!("consider changing this to be mutable: `mut {}`", name)));
                }
                scope.fancy_error(&format!("cannot modify `{}`, as it is not mutable", &container.kind),
                                  labels,
                                  vec![]);
            }
            Ok(())
        }
        Tuple { elts } => {
            for elt in elts {
                check_assign_target(scope, elt)?;
            }
            Ok(())
        }

        Name(name) => match scope.resolve_name(name, target.span)? {
            Some(NamedThing::SelfValue { .. })
                | Some(NamedThing::Item(_))
                | None => Err(invalid_assign_target(scope, target)),
            Some(NamedThing::Variable { mutability, span: def_span, .. }) => match mutability {
                BindingMutability::Mutable => Ok(()),

                BindingMutability::Immutable => {
                    scope.fancy_error(&format!("`{}` cannot be reassigned", name),
                                      vec![Label::primary(target.span, ""),
                                           Label::secondary(def_span, &format!("`{}` is not declared to be mutable", name))],
                                      vec![format!("Hint: use `let mut {}` to allow reassignment", name)]);
                    Ok(())
                }
                BindingMutability::Const => {
                    Err(FatalError::new(scope.fancy_error("cannot assign to constant variable",
                                                          vec![Label::primary(target.span, "")],
                                                          vec!["The left side of an assignment can be a variable name, attribute, subscript, or tuple.".into()])))
                }
            }
        },

        _ => Err(invalid_assign_target(scope, target)),
    }
}

fn invalid_assign_target(scope: &mut BlockScope, target: &Node<fe::Expr>) -> FatalError {
    FatalError::new(scope.fancy_error("invalid assignment target",
                                      vec![Label::primary(target.span, "")],
                                      vec!["The left side of an assignment can be a variable name, attribute, subscript, or tuple.".into()]))
}

/// Gather context information for assignments and check for type errors.
pub fn aug_assign(scope: &mut BlockScope, stmt: &Node<fe::FuncStmt>) -> Result<(), FatalError> {
    let (target, op, rhs) = match &stmt.kind {
        fe::FuncStmt::AugAssign { target, op, value } => (target, op, value),
        _ => unreachable!(),
    };

    let target_attr = expressions::expr(scope, target, None)?;
    let rhs_attr = expressions::expr(scope, rhs, Some(&target_attr.typ))?;
    check_assign_target(scope, target)?;

    if let Err(err) = operations::bin(&target_attr.typ, &op.kind, &rhs_attr.typ) {
        add_bin_operations_errors(
            scope,
            &op.kind,
            target.span,
            &target_attr.typ,
            rhs.span,
            &rhs_attr.typ,
            err,
        );
    }
    Ok(())
}
