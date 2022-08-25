use crate::context::{AnalyzerContext, NamedThing};
use crate::display::Displayable;
use crate::errors::{self, FatalError, TypeCoercionError};
use crate::namespace::scopes::BlockScope;
use crate::operations;
use crate::traversal::utils::add_bin_operations_errors;
use crate::traversal::{expressions, types::try_coerce_type};
use fe_common::diagnostics::Label;
use fe_parser::ast as fe;
use fe_parser::node::Node;

/// Gather context information for assignments and check for type errors.
///
/// e.g. `foo[42] = "bar"`, `self.foo[42] = "bar"`, `foo = 42`
pub fn assign(scope: &mut BlockScope, stmt: &Node<fe::FuncStmt>) -> Result<(), FatalError> {
    let (target, value) = match &stmt.kind {
        fe::FuncStmt::Assign { target, value } => (target, value),
        _ => unreachable!(),
    };
    let target_attributes = expressions::expr(scope, target, None)?;

    let expected_type = target_attributes.typ.deref(scope.db());
    let value_attributes = expressions::expr(scope, value, Some(expected_type))?;
    check_assign_target(scope, target)?;

    match try_coerce_type(
        scope,
        Some(value),
        value_attributes.typ,
        target_attributes.typ,
    ) {
        Err(TypeCoercionError::RequiresToMem) => {
            scope.add_diagnostic(errors::to_mem_error(value.span));
        }
        Err(TypeCoercionError::Incompatible) => {
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
        Err(TypeCoercionError::SelfContractType) => {
            scope.add_diagnostic(errors::self_contract_type_error(
                value.span,
                &target_attributes.typ.display(scope.db()),
            ));
        }
        Ok(_) => {}
    }
    Ok(())
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
        let target_attr = expressions::expr(scope, target, None)?;
        let value_attr = expressions::expr(scope, value, Some(target_attr.typ))?;

        if let Err(err) = operations::bin(
            scope,
            target_attr.typ,
            target,
            op.kind,
            value_attr.typ,
            value,
        ) {
            add_bin_operations_errors(
                scope,
                &op.kind,
                target.span,
                target_attr.typ,
                value.span,
                value_attr.typ,
                err,
            );
        }
        return Ok(());
    }

    unreachable!()
}
