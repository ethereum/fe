use crate::context::{AnalyzerContext, Location, NamedThing};
use crate::display::Displayable;
use crate::errors::FatalError;
use crate::namespace::scopes::BlockScope;
use crate::operations;
use crate::traversal::expressions;
use crate::traversal::utils::add_bin_operations_errors;
use fe_common::diagnostics::Label;
use fe_parser::ast as fe;
use fe_parser::node::Node;

/// Gather context information for assignments and check for type errors.
///
/// e.g. `foo[42] = "bar"`, `self.foo[42] = "bar"`, `foo = 42`
pub fn assign(scope: &mut BlockScope, stmt: &Node<fe::FuncStmt>) -> Result<(), FatalError> {
    if let fe::FuncStmt::Assign { target, value } = &stmt.kind {
        let target_attributes = expressions::expr(scope, target, None)?;

        let value_attributes = expressions::expr(scope, value, Some(target_attributes.typ))?;
        check_assign_target(scope, target)?;
        if target_attributes.typ != value_attributes.typ {
            scope.fancy_error(
                "mismatched types",
                vec![
                    Label::primary(
                        target.span,
                        format!(
                            "this variable has type `{}`",
                            target_attributes.typ.display(scope.db())
                        ),
                    ),
                    Label::secondary(
                        value.span,
                        format!(
                            "this value has incompatible type `{}`",
                            value_attributes.typ.display(scope.db())
                        ),
                    ),
                ],
                vec![],
            );
        }

        if matches!(
            (
                target_attributes.location,
                value_attributes.final_location(),
            ),
            (Location::Memory, Location::Storage { .. })
        ) {
            scope.fancy_error(
                "location mismatch",
                vec![
                    Label::primary(target.span, "this variable is located in memory"),
                    Label::secondary(value.span, "this value is located in storage"),
                ],
                vec!["Hint: values located in storage can be copied to memory using the `to_mem` function.".into(),
                     "Example: `self.my_array.to_mem()`".into(),
                    ],
            );
        }

        return Ok(());
    }

    unreachable!()
}

fn check_assign_target(scope: &mut BlockScope, expr: &Node<fe::Expr>) -> Result<(), FatalError> {
    use fe::Expr::*;

    match &expr.kind {
        Attribute { .. } | Subscript { .. } => Ok(()),
        Tuple { elts } => {
            for elt in elts {
                check_assign_target(scope, elt)?;
            }
            Ok(())
        }

        Name(name) => match scope.resolve_name(name, expr.span)? {
            Some(NamedThing::SelfValue { .. }) => Ok(()),
            Some(NamedThing::Item(_)) | None => Err(invalid_assign_target(scope, expr)),
            Some(NamedThing::Variable { is_const, .. }) => {
                if is_const {
                    Err(FatalError::new(scope.fancy_error("cannot assign to constant variable",
                                                          vec![Label::primary(expr.span, "")],
                                                          vec!["The left side of an assignment can be a variable name, attribute, subscript, or tuple.".into()])))
                } else {
                    Ok(())
                }
            }
        },

        _ => Err(invalid_assign_target(scope, expr)),
    }
}

fn invalid_assign_target(scope: &mut BlockScope, expr: &Node<fe::Expr>) -> FatalError {
    FatalError::new(scope.fancy_error("invalid assignment target",
                                                  vec![Label::primary(expr.span, "")],
                                                  vec!["The left side of an assignment can be a variable name, attribute, subscript, or tuple.".into()]))
}

/// Gather context information for assignments and check for type errors.
pub fn aug_assign(scope: &mut BlockScope, stmt: &Node<fe::FuncStmt>) -> Result<(), FatalError> {
    if let fe::FuncStmt::AugAssign { target, op, value } = &stmt.kind {
        check_assign_target(scope, target)?;
        let target_attributes = expressions::expr(scope, target, None)?;
        let value_attributes = expressions::expr(scope, value, Some(target_attributes.typ))?;

        if let Err(err) = operations::bin(
            scope.db(),
            target_attributes.typ,
            op.kind,
            value_attributes.typ,
        ) {
            add_bin_operations_errors(
                scope,
                &op.kind,
                target.span,
                target_attributes.typ,
                value.span,
                value_attributes.typ,
                err,
            );
        }
        return Ok(());
    }

    unreachable!()
}
