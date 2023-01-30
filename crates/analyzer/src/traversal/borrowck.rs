use super::call_args::LabeledParameter;
use crate::context::{AnalyzerContext, NamedThing};
use crate::namespace::types::{Type, TypeId};
use fe_common::diagnostics::Label;
use fe_parser::ast;
use fe_parser::node::{Node, Span};
use smallvec::{smallvec, SmallVec};

// NOTE: This is a temporary solution to the only borrowing bug that's possible
// in the current semantics of Fe, namely passing a mutable reference to a
// non-primitive object into a fn call, and another reference to that same
// object (mutable or otherwise).
// This is an ugly brute force solution that will definitely not scale
// beyond this simple case, and doesn't do anything smart like allow
// disjoint partial borrows.
// This should be replaced with a proper borrow checker (presumably operating
// on MIR) when Fe gains some kind of reference/projection type.
pub fn check_fn_call_arg_borrows(
    context: &mut dyn AnalyzerContext,
    fn_name: &str,
    method_self: Option<(&Node<ast::Expr>, TypeId)>,
    args: &[Node<ast::CallArg>],
    params: &[impl LabeledParameter],
) {
    // Return early if there are no mut params.
    // This function doesn't attempt to be efficient.
    let mut_self = method_self
        .map(|(_, ty)| ty.is_mut(context.db()))
        .unwrap_or(false);
    if !mut_self
        && !params
            .iter()
            .any(|p| p.typ().map(|t| t.is_mut(context.db())).unwrap_or(false))
    {
        return;
    }

    // Allocate a new Vec<(arg, ty)> including the method target (if present)
    let param_ty = params
        .iter()
        .map(|p| p.typ().unwrap_or_else(|_| Type::unit().id(context.db())));
    let mut args = args
        .iter()
        .map(|arg| &arg.kind.value)
        .zip(param_ty)
        .collect::<Vec<_>>();
    if let Some((target_expr, ty)) = method_self {
        args.insert(0, (target_expr, ty));
    }

    for (idx, (arg, _)) in args
        .iter()
        .enumerate()
        .filter(|(_, (_, ty))| ty.is_mut(context.db()))
    {
        // Find the "root" var of the mut arg expr. Eg the root of a.b.c is `a`.
        // In the case of a ternary expr, there may be more than one.
        // Eg foo(a if x else (b if y else c))
        let vars = resolve_expr_root_vars(context, arg);

        // Check all other non-primitive args for the same root var.
        for (_, (other, _)) in args
            .iter()
            .enumerate()
            .filter(|(i, (_, ty))| *i != idx && !ty.is_primitive(context.db()))
        {
            let other_vars = resolve_expr_root_vars(context, other);
            for (var, var_span) in &vars {
                if let Some((_, other_span)) = other_vars.iter().find(|(nt, _)| nt == var) {
                    let name = var.name(context.db());
                    context.fancy_error(
                        &format!("borrow conflict in call to fn `{fn_name}`"),
                        vec![
                            Label::primary(*var_span, format!("`{name}` is used mutably here")),
                            Label::secondary(*other_span, format!("`{name}` is used again here")),
                        ],
                        vec![],
                    );
                    return;
                }
            }
        }
    }
}

fn resolve_expr_root_vars(
    context: &dyn AnalyzerContext,
    expr: &Node<ast::Expr>,
) -> SmallVec<[(NamedThing, Span); 2]> {
    match &expr.kind {
        ast::Expr::Name(name) => match context.resolve_name(name, expr.span) {
            Ok(
                Some(nt @ NamedThing::Variable { .. }) | Some(nt @ NamedThing::SelfValue { .. }),
            ) => smallvec![(nt, expr.span)],
            _ => smallvec![],
        },

        ast::Expr::Attribute { value, .. } | ast::Expr::Subscript { value, .. } => {
            resolve_expr_root_vars(context, value)
        }
        ast::Expr::Ternary {
            if_expr, else_expr, ..
        } => {
            let mut left = resolve_expr_root_vars(context, if_expr);
            left.append(&mut resolve_expr_root_vars(context, else_expr));
            left
        }
        _ => smallvec![],
    }
}
