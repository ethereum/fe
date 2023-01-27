use crate::context::{AnalyzerContext, DiagnosticVoucher, NamedThing};
use crate::errors::FatalError;
use crate::namespace::scopes::BlockScope;
use crate::namespace::types::{Type, TypeId};
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
    let (target, value) = match &stmt.kind {
        fe::FuncStmt::Assign { target, value } => (target, value),
        _ => unreachable!(),
    };
    if is_valid_assign_target(scope, target)? {
        let lhs_type = assignment_lhs_type(scope, target)?;
        expressions::expect_expr_type(scope, value, lhs_type, true)?;
    }
    Ok(())
}

/// Logs error if `target` type is not `Mut`.
/// Returns `target` type, with `Mut` stripped off.
fn assignment_lhs_type(
    scope: &mut BlockScope,
    target: &Node<fe::Expr>,
) -> Result<TypeId, FatalError> {
    let ty = expressions::expr_type(scope, target)?;
    match ty.typ(scope.db()) {
        Type::Mut(inner) => Ok(inner),
        _ => {
            let mut labels = vec![Label::primary(target.span, "not mutable")];
            if let Some((name, span)) = name_def_span(scope, target) {
                labels.push(Label::secondary(
                    span,
                    format!("consider changing this to be mutable: `mut {name}`"),
                ));
            }
            scope.fancy_error(
                &format!("cannot modify `{}`, as it is not mutable", &target.kind),
                labels,
                vec![],
            );
            Ok(ty)
        }
    }
}

fn name_def_span(scope: &BlockScope, expr: &Node<fe::Expr>) -> Option<(SmolStr, Span)> {
    match &expr.kind {
        fe::Expr::Attribute { value, .. } | fe::Expr::Subscript { value, .. } => {
            name_def_span(scope, value)
        }
        fe::Expr::Name(name) => {
            let thing = scope.resolve_name(name, expr.span).ok()??;
            thing.name_span(scope.db()).map(|span| (name.clone(), span))
        }
        _ => None,
    }
}

fn is_valid_assign_target(
    scope: &mut BlockScope,
    expr: &Node<fe::Expr>,
) -> Result<bool, FatalError> {
    use fe::Expr::*;

    match &expr.kind {
        Attribute { .. } | Subscript { .. } => Ok(true),
        Tuple { elts } => {
            for elt in elts {
                if !is_valid_assign_target(scope, elt)? {
                    return Ok(false);
                }
            }
            Ok(true)
        }

        Name(name) => match scope.resolve_name(name, expr.span) {
            Ok(Some(NamedThing::SelfValue { .. })) => Ok(true),
            Ok(Some(NamedThing::Item(_)) | Some(NamedThing::EnumVariant(_)) | None) => {
                bad_assign_target_error(scope, expr, "invalid assignment target");
                Ok(false)
            }
            Ok(Some(NamedThing::Variable { is_const, .. })) => {
                if is_const {
                    bad_assign_target_error(scope, expr, "cannot assign to a constant value");
                }
                Ok(!is_const)
            }
            Err(e) => Err(e.into()),
        },
        _ => {
            bad_assign_target_error(scope, expr, "invalid assignment target");
            Ok(false)
        }
    }
}

fn bad_assign_target_error(
    scope: &mut BlockScope,
    expr: &Node<fe::Expr>,
    msg: &str,
) -> DiagnosticVoucher {
    scope.fancy_error(
        msg,
        vec![Label::primary(expr.span, "")],
        vec!["The left side of an assignment can be a variable name, attribute, subscript, or tuple.".into()]
    )
}

/// Gather context information for assignments and check for type errors.
pub fn aug_assign(scope: &mut BlockScope, stmt: &Node<fe::FuncStmt>) -> Result<(), FatalError> {
    let (target, op, value) = match &stmt.kind {
        fe::FuncStmt::AugAssign { target, op, value } => (target, op, value),
        _ => unreachable!(),
    };

    if is_valid_assign_target(scope, target)? {
        let lhs_ty = assignment_lhs_type(scope, target)?;
        let rhs = expressions::expr(scope, value, Some(lhs_ty))?;

        if let Err(err) = operations::bin(scope, lhs_ty, target, op.kind, rhs.typ, value) {
            add_bin_operations_errors(
                scope,
                &op.kind,
                target.span,
                lhs_ty,
                value.span,
                rhs.typ,
                err,
            );
        }
    }

    Ok(())
}
