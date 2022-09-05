use crate::context::{AnalyzerContext, NamedThing};
use crate::errors::FatalError;
use crate::namespace::scopes::BlockScope;
use crate::operations;
use crate::traversal::expressions::{self, expect_expr_type, value_expr_type};
use crate::traversal::utils::add_bin_operations_errors;
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
    if is_valid_assign_target(scope, target)? {
        let target_type = expressions::expr_type(scope, target)?;
        expect_expr_type(scope, value, target_type)?;
    }
    Ok(())
}

fn is_valid_assign_target(
    scope: &mut BlockScope,
    expr: &Node<fe::Expr>,
) -> Result<bool, FatalError> {
    use fe::Expr::*;

    let nope = || {
        scope.fancy_error(
            "invalid assignment target",
            vec![Label::primary(expr.span, "")],
            vec!["The left side of an assignment can be a variable name, attribute, subscript, or tuple.".into()]
        );
        Ok(false)
    };

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
            Ok(Some(NamedThing::SelfValue { .. })) => Ok(true), // XXX ?
            Ok(Some(NamedThing::Item(_)) | None) => nope(),
            Ok(Some(NamedThing::Variable { is_const, .. })) => {
                if is_const {
                    scope.fancy_error("cannot assign to constant variable",
                                      vec![Label::primary(expr.span, "")],
                                      vec!["The left side of an assignment can be a variable name, attribute, subscript, or tuple.".into()]);
                }
                Ok(!is_const)
            }
            Err(e) => Err(e.into()),
        },
        _ => nope(),
    }
}

/// Gather context information for assignments and check for type errors.
pub fn aug_assign(scope: &mut BlockScope, stmt: &Node<fe::FuncStmt>) -> Result<(), FatalError> {
    if let fe::FuncStmt::AugAssign { target, op, value } = &stmt.kind {
        if is_valid_assign_target(scope, target)? {
            let target_typ = value_expr_type(scope, target, None)?;
            let value_typ = value_expr_type(scope, value, Some(target_typ))?;

            if let Err(err) = operations::bin(scope, target_typ, target, op.kind, value_typ, value) {
                add_bin_operations_errors(scope, &op.kind, target.span, target_typ, value.span, value_typ, err);
            }
        }
        return Ok(());
    }

    unreachable!()
}
